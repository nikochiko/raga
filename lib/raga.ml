let ( // ) = Filename.concat

let rec hb_literal_of_huml = function
  | `String s -> `String s
  | `Bool b -> `Bool b
  | `Int i -> `Int i
  | `Float f -> `Float f
  | `Null -> `Null
  | `Intlit s -> `String s (* Convert intlit to string for handlebars *)
  | `List l -> `List (List.map hb_literal_of_huml l)
  | `Assoc pairs ->
      `Assoc (List.map (fun (k, v) -> (k, hb_literal_of_huml v)) pairs)

let url_of_path path =
  let open Filename in
  let rec aux acc p =
    match (dirname p, basename p) with
    | dir, base when base = current_dir_name ->
        assert (dir = current_dir_name);
        let acc = "/" :: acc in
        String.concat "" acc
    | dir, base -> aux ((base ^ "/") :: acc) dir
  in
  match (dirname path, basename path) with
  | dir, "index.html" -> aux [] dir
  | dir, base -> aux [ base ] dir

(* Page type for content and frontmatter *)
module Page = struct
  type t = {
    content : string;
    frontmatter : (string * Huml.t) list;
    src_path : string option;
    dst_path : string;
    url : string;
    title : string;
  }

  let to_huml page =
    let frontmatter_handlebars =
      List.map (fun (k, v) -> (k, hb_literal_of_huml v)) page.frontmatter
    in
    `Assoc
      [
        ("content", `String page.content);
        ( "src_path",
          match page.src_path with Some s -> `String s | None -> `Null );
        ("dst_path", `String page.dst_path);
        ("url", `String page.url);
        ("title", `String page.title);
        ("frontmatter", `Assoc frontmatter_handlebars);
      ]
end

module PageRule = struct
  type src = SrcPath of string | SrcGlob of string

  type excl =
    | Frontmatter of (string * Huml.t) list
    | ExclGlob of string
    | ExclPath of string

  type t = {
    name : string;
    src : src list;
    dst : string;
    tmpl : string;
    excl : excl list;
  }

  let of_assoc ~name assoc =
    let src =
      match List.assoc_opt "src" assoc with
      | None -> []
      | Some (`String s) -> [ SrcPath s ]
      | Some (`Assoc lst) ->
          lst
          |> List.map (fun (k, v) ->
                 match (k, v) with
                 | "path", `String s -> SrcPath s
                 | "glob", `String s -> SrcGlob s
                 | _ -> failwith ("Invalid src entry in page " ^ name))
      | Some _ -> failwith ("Invalid src format in page " ^ name)
    in
    let dst =
      match List.assoc_opt "dst" assoc with
      | Some (`String s) -> s
      | _ -> failwith ("dst must be a string in page " ^ name)
    in
    let tmpl =
      match List.assoc_opt "tmpl" assoc with
      | Some (`String s) -> s
      | _ -> failwith ("tmpl must be a string in page " ^ name)
    in
    let excl =
      match List.assoc_opt "excl" assoc with
      | None -> []
      | Some (`String s) -> [ ExclPath s ]
      | Some (`Assoc lst) ->
          lst
          |> List.map (fun (k, v) ->
                 match (k, v) with
                 | "frontmatter", `Assoc fm -> Frontmatter fm
                 | "glob", `String s -> ExclGlob s
                 | "path", `String s -> ExclPath s
                 | "frontmatter", _ ->
                     failwith
                       ("frontmatter exclusion must be an object in page "
                      ^ name ^ ". Use: frontmatter:: key: value")
                 | "glob", _ ->
                     failwith
                       ("glob exclusion must be a string in page " ^ name
                      ^ ". Use: glob: \"pattern\"")
                 | "path", _ ->
                     failwith
                       ("path exclusion must be a string in page " ^ name
                      ^ ". Use: path: \"filepath\"")
                 | key, _ ->
                     failwith
                       ("Unknown exclusion type '" ^ key ^ "' in page " ^ name
                      ^ ". Supported types: frontmatter, glob, path"))
      | Some _ -> failwith ("Invalid excl format in page " ^ name)
    in
    { name; src; dst; tmpl; excl }
end

module Site = struct
  type t = {
    title : string;
    base_url : string option;
    rules : PageRule.t list;
    params : (string * Huml.t) list;
    templates_dir : string;
    partials_dir : string;
    content_dir : string;
    static_dir : string;
  }

  let default_templates_dir = "templates"
  let default_content_dir = "content"
  let default_static_dir = "static"
  let default_partials_subdir = "partials" (* {{templates_dir}}/partials *)

  let of_assoc assoc =
    let read_string_field_opt ?default key assoc =
      match List.assoc_opt key assoc with
      | Some (`String s) -> Some s
      | Some _ -> failwith (Printf.sprintf "Site %s must be a string" key)
      | None -> default
    in
    let read_string_field ?default key assoc =
      match read_string_field_opt key assoc with
      | Some s -> s
      | None -> (
          match default with
          | Some d -> d
          | None -> failwith (Printf.sprintf "Site %s is required" key))
    in
    let read_assoc_field ?default key assoc =
      match List.assoc_opt key assoc with
      | Some (`Assoc lst) -> lst
      | Some _ -> failwith (Printf.sprintf "Site %s must be a Huml dict" key)
      | None -> (
          match default with
          | Some d -> d
          | None -> failwith (Printf.sprintf "Site %s is required" key))
    in
    let title = read_string_field "title" assoc in
    let base_url = read_string_field_opt "base_url" assoc in
    let templates_dir =
      read_string_field ~default:default_templates_dir "templates_dir" assoc
    in
    let partials_dir =
      read_string_field
        ~default:(templates_dir // default_partials_subdir)
        "partials_dir" assoc
    in
    let content_dir =
      read_string_field ~default:default_content_dir "content_dir" assoc
    in
    let static_dir =
      read_string_field ~default:default_static_dir "static_dir" assoc
    in
    let rules =
      read_assoc_field "pages" assoc
      |> List.map (fun (name, doc) ->
             match doc with
             | `Assoc page_assoc -> PageRule.of_assoc ~name page_assoc
             | _ -> failwith ("Site->PageRule: " ^ name ^ " must be a Huml dict"))
    in
    let params = read_assoc_field "params" assoc ~default:[] in
    List.iter
      (fun (k, _) ->
        match k with
        | "title" | "base_url" | "pages" | "templates_dir" | "partials_dir"
        | "content_dir" | "static_dir" | "params" ->
            ()
        | unknown_key -> failwith ("Unknown site config key: " ^ unknown_key))
      assoc;
    {
      title;
      base_url;
      rules;
      templates_dir;
      partials_dir;
      content_dir;
      static_dir;
      params;
    }

  let of_huml doc =
    match doc with
    | `Assoc assoc -> of_assoc assoc
    | _ -> failwith "Site config must be a Huml dict"

  let to_huml site =
    let site_obj =
      [ ("title", `String site.title) ]
      @ (match site.base_url with
        | Some url -> [ ("base_url", `String url) ]
        | None -> [])
      @ [ ("params", `Assoc site.params) ]
    in
    `Assoc site_obj
end

module Template = struct
  type env = {
    templates_dir : string;
    partials_dir : string;
    pages : Page.t list;
  }

  let mk_env pages templates_dir partials_dir =
    { templates_dir; partials_dir; pages }

  let hb_is_truthy = function
    | `Null
    | `String ""
    | `Int 0
    | `Float 0.0
    | `Bool false
    | `List []
    | `Assoc [] ->
        false
    | _ -> true

  let get_helper env name =
    let md2html = function
      | [ `String md ] ->
          let html = Omd.to_html (Omd.of_string md) in
          Some (`String html)
      | _ -> None
    in

    let replace = function
      | [ `String pattern; `String replacement; `String text ] ->
          let regex = Re.Pcre.regexp pattern in
          let result = Re.replace_string regex ~by:replacement text in
          Some (`String result)
      | _ -> None
    in

    let or_helper args =
      let rec aux = function
        | [] -> None
        | x :: xs -> if hb_is_truthy x then Some x else aux xs
      in
      aux args
    in

    let and_helper args =
      let rec aux = function
        | [] -> None
        | x :: [] -> Some x
        | x :: xs -> if hb_is_truthy x then aux xs else Some x
      in
      aux args
    in

    let dirname_helper = function
      | [ `String path ] -> Some (`String (Filename.dirname path))
      | _ -> None
    in

    let basename_helper = function
      | [ `String path ] -> Some (`String (Filename.basename path))
      | _ -> None
    in

    let get_all_pages_helper = function
      | [ `String sort_by; `String order ] ->
          if order <> "asc" && order <> "desc" then
            failwith "order must be 'asc' or 'desc'"
          else
            let pages_hb =
              env.pages
              |> List.sort (fun p1 p2 ->
                     let v1 = List.assoc_opt sort_by p1.Page.frontmatter in
                     let v2 = List.assoc_opt sort_by p2.Page.frontmatter in
                     match (v1, v2) with
                     | Some (`String s1), Some (`String s2) ->
                         String.compare s1 s2
                     | Some (`Int i1), Some (`Int i2) -> Int.compare i1 i2
                     | Some (`Float f1), Some (`Float f2) -> Float.compare f1 f2
                     | Some _, Some _ -> 0
                     | Some _, None -> 1
                     | None, Some _ -> -1
                     | None, None -> 0)
              |> List.map (fun page -> Page.to_huml page)
              |> fun ls -> if order = "asc" then ls else List.rev ls
            in

            Some (`List pages_hb)
      | _ -> None
    in

    let parse_date = function
      | [ `String date_str ] -> (
          let date = Ptime.of_rfc3339 date_str in
          match date with
          | Ok (ptime, _, _) ->
              let (year, month, day), _ = Ptime.to_date_time ptime in
              Some
                (`Assoc
                   [
                     ("year", `Int year);
                     ("month", `Int month);
                     ("day", `Int day);
                   ])
          | Error _ ->
              failwith
                (Printf.sprintf "Invalid date format: %s. Expected RFC 3339."
                   date_str))
      | _ -> failwith "parse_date expects a single string argument"
    in

    let chop_suffix_helper = function
      | [ `String str; `String suffix ] -> (
          match Filename.chop_suffix_opt ~suffix str with
          | Some s -> Some (`String s)
          | _ -> Some (`String str))
      | _ -> None
    in

    let get_url_path = function
      | [ `String url ] ->
          let uri = Uri.of_string url in
          Some (`String (Uri.path uri))
      | _ -> failwith "get_url_path expects a single string argument"
    in

    let endswith = function
      | [ `String str; `String suffix ] ->
          Some (`Bool (String.ends_with ~suffix str))
      | _ -> None
    in
    let startswith = function
      | [ `String str; `String prefix ] ->
          Some (`Bool (String.starts_with ~prefix str))
      | _ -> None
    in

    let concat_path args =
      let rec aux = function
        | [] -> failwith "concat_path requires at least one argument"
        | `String last :: [] -> last
        | `String base :: rest -> base // aux rest
        | _ -> failwith "concat_path only accepts string arguments"
      in
      Some (`String (aux args))
    in

    match name with
    | "md2html" -> Some md2html
    | "replace" -> Some replace
    | "or" -> Some or_helper
    | "and" -> Some and_helper
    | "dirname" -> Some dirname_helper
    | "basename" -> Some basename_helper
    | "get_all_pages" -> Some get_all_pages_helper
    | "chop_suffix" -> Some chop_suffix_helper
    | "get_url_path" -> Some get_url_path
    | "parse_date" -> Some parse_date
    | "endswith" -> Some endswith
    | "startswith" -> Some startswith
    | "concat_path" -> Some concat_path
    | _ -> Handlebars_ml.default_get_helper name

  let get_partial env partial_name =
    let partial_path = env.partials_dir // (partial_name ^ ".hbs") in
    if Sys.file_exists partial_path then (
      let ic = open_in partial_path in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Some content)
    else None

  let render env template_name context =
    let template_path = env.templates_dir // template_name in
    if Sys.file_exists template_path then (
      let ic = open_in template_path in
      let template_content = really_input_string ic (in_channel_length ic) in
      close_in ic;

      (* Convert context to the correct type for handlebars-ml *)
      let hb_context = hb_literal_of_huml context in

      match
        Handlebars_ml.compile ~get_helper:(get_helper env)
          ~get_partial:(get_partial env) template_content hb_context
      with
      | Ok result -> result
      | Error err ->
          Printf.eprintf "Template rendering error in %s\n" template_path;
          Printf.eprintf "Error details: %s\n"
            (Handlebars_ml.Compiler.show_hb_error err);
          Printf.eprintf "Template content:\n%s\n" template_content;
          failwith ("Template rendering failed for " ^ template_name))
    else (
      Printf.eprintf "Warning: Template not found, skipping: %s\n" template_path;
      "")
end

(* File processing utilities *)
module FileUtils = struct
  let extract_frontmatter content =
    let lines = String.split_on_char '\n' content in
    let rec extract_frontmatter acc in_frontmatter = function
      | [] -> (List.rev acc, [])
      | "---" :: rest when not in_frontmatter ->
          extract_frontmatter acc true rest
      | "---" :: rest when in_frontmatter -> (List.rev acc, rest)
      | line :: rest when in_frontmatter ->
          extract_frontmatter (line :: acc) true rest
      | rest -> (List.rev acc, rest)
    in
    let frontmatter_lines, content_lines = extract_frontmatter [] false lines in
    let frontmatter = String.concat "\n" frontmatter_lines in
    let content = String.concat "\n" content_lines in
    (frontmatter, content)

  let parse_frontmatter frontmatter_str =
    (* Parse frontmatter as HUML document *)
    let lexbuf = Lexing.from_string frontmatter_str in
    match Huml.parse lexbuf with
    | Ok (`Assoc lst) -> lst
    | Ok _ -> failwith "Frontmatter must be a Huml dict"
    | Error msg -> failwith ("Failed to parse frontmatter: " ^ msg)

  let read_file path =
    let ic = open_in path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    content

  let find_files_glob pattern base_dir =
    let glob_re =
      let glob =
        Re.Glob.glob ~anchored:true ~expand_braces:true ~match_backslashes:true
          pattern
      in
      Re.compile glob
    in
    let rec collect_files dir acc =
      if Sys.is_directory dir then
        let entries = Sys.readdir dir in
        Array.fold_left
          (fun acc entry ->
            let full_path = dir // entry in
            let relative_path =
              if String.length full_path > String.length base_dir then
                String.sub full_path
                  (String.length base_dir + 1)
                  (String.length full_path - String.length base_dir - 1)
              else entry
            in
            if Re.execp glob_re relative_path then full_path :: acc
            else if Sys.is_directory full_path then collect_files full_path acc
            else acc)
          acc entries
      else acc
    in
    collect_files base_dir []

  let title_of_filename filename =
    (* convert a filename like "my-first-post.md" to "My First Post" *)
    let base = Filename.remove_extension filename in
    let words = String.split_on_char '-' base in
    let capitalized_words =
      List.map
        (fun word ->
          if String.length word > 0 then String.capitalize_ascii word else word)
        words
    in
    String.concat " " capitalized_words

  let rec rm_r dir =
    Sys.readdir dir
    |> Array.iter (fun entry ->
           let path = dir // entry in
           if Sys.is_directory path then rm_r path else Sys.remove path);
    Sys.rmdir dir

  let rec mv_r src dst =
    if Sys.file_exists dst && Sys.is_directory dst then rm_r dst;
    Sys.mkdir dst 0o755;
    Sys.readdir src
    |> Array.iter (fun entry ->
           let src_path = src // entry in
           let dst_path = dst // entry in
           if Sys.is_directory src_path then mv_r src_path dst_path
           else Sys.rename src_path dst_path);
    Sys.rmdir src

  let cp src dst =
    let ic = open_in src in
    let oc = open_out dst in
    let buffer_size = 8192 in
    let buffer = Bytes.create buffer_size in
    let rec copy_loop () =
      let bytes_read = input ic buffer 0 buffer_size in
      if bytes_read > 0 then (
        output oc buffer 0 bytes_read;
        copy_loop ())
    in
    copy_loop ();
    close_in ic;
    close_out oc

  let rec cp_r src dst =
    if Sys.is_directory src then (
      if not (Sys.file_exists dst) then Sys.mkdir dst 0o755;
      Sys.readdir src
      |> Array.iter (fun entry ->
             let src_path = src // entry in
             let dst_path = dst // entry in
             if Sys.is_directory src_path then cp_r src_path dst_path
             else cp src_path dst_path))
    else cp src dst
end

(* Main site generation *)
module Generator = struct
  let process_page_rule (site : Site.t) (rule : PageRule.t) base_dir =
    let content_dir = base_dir // site.content_dir in
    let find_source_files = function
      | PageRule.SrcPath path -> [ Some (content_dir // path) ]
      | PageRule.SrcGlob pattern ->
          FileUtils.find_files_glob pattern content_dir
          |> List.map (fun p -> Some p)
    in

    let all_source_files =
      if rule.src = [] then [ None ]
      else
        List.fold_left (fun acc src -> find_source_files src @ acc) [] rule.src
    in

    (* Apply exclusion filters *)
    let should_exclude file_path =
      let relative_path =
        if String.length file_path > String.length content_dir then
          String.sub file_path
            (String.length content_dir + 1)
            (String.length file_path - String.length content_dir - 1)
        else file_path
      in
      List.exists
        (function
          | PageRule.ExclGlob pattern ->
              let glob_re =
                Re.compile
                  (Re.Glob.glob ~anchored:true ~expand_braces:true
                     ~match_backslashes:true pattern)
              in
              Re.execp glob_re relative_path
          | PageRule.ExclPath path -> relative_path = path
          | PageRule.Frontmatter fm_rules ->
              let content = FileUtils.read_file file_path in
              let frontmatter, _ = FileUtils.extract_frontmatter content in
              let frontmatter_data =
                if frontmatter <> "" then
                  try FileUtils.parse_frontmatter frontmatter
                  with exn ->
                    failwith
                      (Printf.sprintf "Error parsing frontmatter in %s: %s"
                         file_path (Printexc.to_string exn))
                else []
              in
              (* Check if all frontmatter rules match *)
              List.exists
                (fun (key, expected_value) ->
                  match List.assoc_opt key frontmatter_data with
                  | Some actual_value ->
                      (* Direct HUML value comparison *)
                      actual_value = expected_value
                  | None -> false)
                fm_rules)
        rule.excl
    in

    let filtered_source_files =
      all_source_files
      |> List.filter (function
           | Some file -> not (should_exclude file)
           | None -> true)
    in

    let process_markdown_metadata file_path content =
      let frontmatter, markdown_content =
        FileUtils.extract_frontmatter content
      in
      let frontmatter_data =
        if frontmatter <> "" then
          try FileUtils.parse_frontmatter frontmatter
          with exn ->
            failwith
              (Printf.sprintf "Error parsing frontmatter in %s: %s" file_path
                 (Printexc.to_string exn))
        else []
      in
      (frontmatter_data, markdown_content)
    in

    let process_file file_path_opt =
      let frontmatter, content =
        match file_path_opt with
        | Some file_path -> (
            let raw_content = FileUtils.read_file file_path in
            match Filename.extension file_path with
            | ".md" | ".markdown" ->
                process_markdown_metadata file_path raw_content
            | _ -> ([], raw_content))
        | None -> ([], "")
      in
      let title =
        match (List.assoc_opt "title" frontmatter, file_path_opt) with
        | Some (`String t), _ -> t
        | Some _, _ -> failwith "Title in frontmatter must be a string"
        | None, Some file_path ->
            FileUtils.title_of_filename (Filename.basename file_path)
        | None, None -> ""
      in
      let src_path =
        match file_path_opt with
        | Some file_path ->
            let rel_path =
              if String.length file_path > String.length content_dir then
                String.sub file_path
                  (String.length content_dir + 1)
                  (String.length file_path - String.length content_dir - 1)
              else file_path
            in
            Some rel_path
        | None -> None
      in
      let dst_path =
        (* Apply dst template transformation using handlebars *)
        let dst_context =
          match src_path with
          | Some path -> `Assoc [ ("src_path", `String path) ]
          | None -> `Assoc []
        in
        let hb_dst_context = hb_literal_of_huml dst_context in

        let dummy_env = Template.mk_env [] "" "" in
        let compiled_dst =
          match
            Handlebars_ml.compile
              ~get_helper:(Template.get_helper dummy_env)
              rule.dst hb_dst_context
          with
          | Ok path ->
              let rec validate acc p =
                let open Filename in
                match (dirname p, basename p) with
                | _, base when base = parent_dir_name || base = current_dir_name
                  ->
                    failwith
                      (Printf.sprintf
                         "Destination path %S must be a relative implicit path \
                          (no .. or absolute paths allowed)"
                         p)
                | dir, base when dir = current_dir_name ->
                    List.fold_left (fun acc' s -> acc' // s) base acc
                | dir, base -> validate (base :: acc) dir
              in
              validate [] path
          | Error err ->
              let msg =
                Printf.sprintf "Error compiling dst template %S: %s" rule.dst
                  (Handlebars_ml.Compiler.show_hb_error err)
              in
              failwith msg
        in
        compiled_dst
      in
      let url = url_of_path dst_path in
      { Page.content; frontmatter; src_path; dst_path; url; title }
    in

    List.map process_file filtered_source_files

  let generate site base_dir output_dir =
    (* Process all page rules to get pages *)
    let all_pages =
      List.fold_left
        (fun acc rule ->
          let pages = process_page_rule site rule base_dir in
          pages @ acc)
        [] site.rules
    in

    (* Set up template environment *)
    let templates_dir = base_dir // site.templates_dir in
    let partials_dir = templates_dir // "partials" in
    let static_dir = base_dir // site.static_dir in
    let env = Template.mk_env all_pages templates_dir partials_dir in

    (* Generate each page *)
    let generate_page (page : Page.t) (rule : PageRule.t) =
      let site_context = Site.to_huml site in
      let page_context = Page.to_huml page in

      let context =
        `Assoc
          [
            ("site", site_context);
            ("page", page_context);
            ("content", `String page.content);
            ("url", `String page.url);
            ("title", `String page.title);
            ( "frontmatter",
              `Assoc
                (List.map
                   (fun (k, v) -> (k, hb_literal_of_huml v))
                   page.frontmatter) );
          ]
      in

      let rendered = Template.render env rule.tmpl context in
      let output_path = output_dir // page.dst_path in
      let output_dir_path = Filename.dirname output_path in

      (* Create output directory if it doesn't exist *)
      let rec create_dirs path =
        if not (Sys.file_exists path) then (
          create_dirs (Filename.dirname path);
          Sys.mkdir path 0o755)
      in
      create_dirs output_dir_path;

      (* Write the file *)
      let oc = open_out output_path in
      output_string oc rendered;
      close_out oc
    in

    if Sys.file_exists static_dir && Sys.is_directory static_dir then
      FileUtils.cp_r static_dir output_dir;

    (* Match pages with their rules and generate *)
    List.iter
      (fun rule ->
        let pages = process_page_rule site rule base_dir in
        List.iter (fun page -> generate_page page rule) pages)
      site.rules
end
