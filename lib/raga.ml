open Handlebars_ml

let ( // ) = Filename.concat
let compose = fun f g x -> f (g x)

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

let unwind ~protect f x =
  let res = try Either.Left (f x) with exn -> Either.Right exn in
  let _ = protect x in
  match res with Either.Left v -> v | Either.Right exn -> raise exn

let print_ascii_table lst =
  let max_fst_len =
    List.fold_left (fun acc (k, _) -> max acc (String.length k)) 0 lst
  in
  let max_snd_len =
    List.fold_left (fun acc (_, v) -> max acc (String.length v)) 5 lst
  in
  let print_sep () =
    Printf.eprintf "--%s--+--%s--\n"
      (String.make max_fst_len '-')
      (String.make max_snd_len '-')
  in
  let print_row (fst, snd) =
    Printf.eprintf "  %-*s  |  %-*s  \n" max_fst_len fst max_snd_len snd
  in
  print_row ("", "Count");
  print_sep ();
  List.iter print_row lst

module PageRule = struct
  type excl =
    | Frontmatter of (string * Huml.t) list
    | ExclGlob of string
    | ExclPath of string

  type t = {
    name : string;
    src : string option;
    dst : string;
    tmpl : string;
    excl : excl list;
    enable_frontmatter : bool;
  }

  let of_assoc ~name assoc =
    let src =
      match List.assoc_opt "src" assoc with
      | None -> None
      | Some (`String s) -> Some s
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
    let enable_frontmatter =
      match List.assoc_opt "enable_frontmatter" assoc with
      | Some (`Bool b) -> b
      | Some _ ->
          failwith
            ("enable_frontmatter must be a boolean in page " ^ name)
      | None -> true
    in
    { name; src; dst; tmpl; excl; enable_frontmatter }
end

module Page = struct
  type t = {
    content : string;
    frontmatter : (string * Huml.t) list;
    src_path : string option;
    dst_path : string;
    url : string;
    title : string;
    rule : PageRule.t;
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
    open_in path
    |> unwind ~protect:close_in @@ fun ic ->
       let len = in_channel_length ic in
       really_input_string ic len

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
      words
      |> List.map @@ fun word ->
         if String.length word > 0 then String.capitalize_ascii word else word
    in
    String.concat " " capitalized_words

  let rec mkdir_p path =
    if not (Sys.file_exists path) then (
      mkdir_p (Filename.dirname path);
      Sys.mkdir path 0o755)

  let rec rm_r dir =
    (Sys.readdir dir
    |> Array.iter @@ fun entry ->
       let path = dir // entry in
       if Sys.is_directory path then rm_r path else Sys.remove path);
    Sys.rmdir dir

  let rec mv_r src dst =
    if Sys.file_exists dst && Sys.is_directory dst then rm_r dst;
    Sys.mkdir dst 0o755;
    (Sys.readdir src
    |> Array.iter @@ fun entry ->
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

  let cp_r src dst =
    let count = ref 0 in
    let cp' p1 p2 =
      incr count;
      cp p1 p2
    in
    let rec aux src dst =
      if Sys.is_directory src then (
        if not (Sys.file_exists dst) then Sys.mkdir dst 0o755;
        Sys.readdir src
        |> Array.iter @@ fun entry ->
           let src_path = src // entry in
           let dst_path = dst // entry in
           if Sys.is_directory src_path then aux src_path dst_path
           else cp' src_path dst_path)
      else cp' src dst
    in
    aux src dst;
    !count
end

module Template = struct
  type t = {
    name : string; (* for error reporting *)
    content : string;
    context : Huml.t;
    partials : (string * string) list;
    helpers : (string * Handlebars.custom_helper) list;
  }

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

  let get_helper template name =
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

    let format_date = function
      | [ `String date_str ] -> (
          let date = Ptime.of_rfc3339 date_str in
          match date with
          | Ok (dt, tz, _) ->
              let tz_offset_s = match tz with
              | Some offset -> offset
              | None -> 0
              in
              let (y, m, d) = Ptime.to_date ~tz_offset_s dt in
              let month_str = match m with
                | 1 -> "January"
                | 2 -> "February"
                | 3 -> "March"
                | 4 -> "April"
                | 5 -> "May"
                | 6 -> "June"
                | 7 -> "July"
                | 8 -> "August"
                | 9 -> "September"
                | 10 -> "October"
                | 11 -> "November"
                | 12 -> "December"
                | _ -> ""
              in
              let formatted = Printf.sprintf "%s %02d, %d" month_str d y in
              Some (`String formatted)
          | Error _ ->
              failwith
                (Printf.sprintf "Invalid date format: %s. Expected RFC 3339."
                   date_str)
      )
      | _ -> failwith "format_date expects a single string argument"
    in

    match List.assoc_opt name template.helpers with
    | Some custom_helper -> Some custom_helper
    | None -> (
        match name with
        | "md2html" -> Some md2html
        | "replace" -> Some replace
        | "or" -> Some or_helper
        | "and" -> Some and_helper
        | "dirname" -> Some dirname_helper
        | "basename" -> Some basename_helper
        | "chop_suffix" -> Some chop_suffix_helper
        | "get_url_path" -> Some get_url_path
        | "parse_date" -> Some parse_date
        | "format_date" -> Some format_date
        | "endswith" -> Some endswith
        | "startswith" -> Some startswith
        | "concat_path" -> Some concat_path
        | _ -> Handlebars.default_get_helper name)

  let render template =
    let hb_context = hb_literal_of_huml template.context in
    let get_partial name = List.assoc_opt name template.partials in

    match
      Handlebars.compile ~get_helper:(get_helper template) ~get_partial
        template.content hb_context
    with
    | Ok result -> result
    | Error err ->
        let msg =
          Printf.sprintf "Render error in template %S: %s" template.name
            (Handlebars.show_hb_error err)
        in
        failwith msg

  let of_string ~name ?(context = `Assoc []) ?(partials = []) ?(helpers = [])
      content =
    { name; content; context; partials; helpers }
end

(* Main site generation *)
module Generator = struct
  let process_page_rule (site : Site.t) (rule : PageRule.t) base_dir =
    let content_dir = base_dir // site.content_dir in

    (* Apply exclusion filters *)
    let should_exclude file_path =
      let relative_path =
        if String.length file_path > String.length content_dir then
          String.sub file_path
            (String.length content_dir + 1)
            (String.length file_path - String.length content_dir - 1)
        else file_path
      in
      rule.excl
      |> List.exists @@ function
         | PageRule.ExclGlob pattern ->
             let glob_re =
               Re.compile
                 (Re.Glob.glob ~anchored:true ~expand_braces:true
                    ~match_backslashes:true pattern)
             in
             Re.execp glob_re relative_path
         | PageRule.ExclPath path -> relative_path = path
         | PageRule.Frontmatter fm_rules -> (
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
             fm_rules
             |> List.exists @@ fun (key, expected_value) ->
                match List.assoc_opt key frontmatter_data with
                | Some actual_value ->
                    (* Direct HUML value comparison *)
                    actual_value = expected_value
                | None -> false)
    in

    let source_files =
      match rule.src with
      | None -> None
      | Some pattern ->
          let file_paths =
            FileUtils.find_files_glob pattern content_dir
            |> List.filter @@ compose not should_exclude
          in
          Some file_paths
    in

    let extract_frontmatter file_path content =
      let frontmatter, other_content =
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
      (frontmatter_data, other_content)
    in

    let process_file file_path_opt =
      let frontmatter, content =
        match file_path_opt with
        | Some file_path -> (
            let raw_content = FileUtils.read_file file_path in
            if rule.enable_frontmatter then
              extract_frontmatter file_path raw_content
            else ([], raw_content))
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
        let tmpl_name = Printf.sprintf "dst_path(%s)" rule.name in
        let tmpl =
          Template.of_string ~name:tmpl_name ~context:dst_context rule.dst
        in
        let path = Template.render tmpl in
        let rec validate acc p =
          let open Filename in
          match (dirname p, basename p) with
          | _, base when base = parent_dir_name || base = current_dir_name ->
              failwith
                (Printf.sprintf
                   "Destination path %S must be a relative implicit path (no \
                    .. or absolute paths allowed)"
                   p)
          | dir, base when dir = current_dir_name ->
              List.fold_left (fun acc' s -> acc' // s) base acc
          | dir, base -> validate (base :: acc) dir
        in
        validate [] path
      in
      let url = url_of_path dst_path in
      { Page.content; frontmatter; src_path; dst_path; url; title; rule }
    in

    match source_files with
    | Some files -> List.map (fun fp -> process_file (Some fp)) files
    | None -> [ process_file None ]

  let collect_partials base_dir =
    FileUtils.find_files_glob "*.hbs" base_dir
    |> List.map @@ fun path ->
       let partial_name = Filename.basename path |> Filename.remove_extension in
       (partial_name, FileUtils.read_file path)

  let get_all_pages_helper all_pages = function
    | [ `String sort_by; `String order ] ->
        if order <> "asc" && order <> "desc" then
          failwith "order must be 'asc' or 'desc'"
        else
          let pages_hb =
            all_pages
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

  let make_default_context site page =
    let site_context = Site.to_huml site in
    let page_context = Page.to_huml page in

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

  let generate_page ~partials ~helpers ~templates_dir ~output_dir
      ~(site : Site.t) ~(page : Page.t) =
    let context = make_default_context site page in
    let tmpl =
      let tmpl_content =
        FileUtils.read_file (templates_dir // page.rule.tmpl)
      in
      Template.of_string ~name:page.rule.tmpl ~context ~partials ~helpers
        tmpl_content
    in
    let rendered = Template.render tmpl in
    let output_path = output_dir // page.dst_path in
    let () = FileUtils.mkdir_p (Filename.dirname output_path) in
    open_out output_path
    |> unwind ~protect:close_out @@ fun oc -> output_string oc rendered

  let generate (site : Site.t) base_dir output_dir =
    let count_by_rule = Hashtbl.create (List.length site.rules) in
    let all_pages =
      site.rules
      |> List.concat_map @@ fun rule ->
         let pages = process_page_rule site rule base_dir in
         let new_count =
           match Hashtbl.find_opt count_by_rule rule.name with
           | Some c -> c + List.length pages
           | None -> List.length pages
         in
         Hashtbl.replace count_by_rule rule.name new_count;
         pages
    in

    let templates_dir = base_dir // site.templates_dir in
    let partials_dir = base_dir // site.partials_dir in
    let static_dir = base_dir // site.static_dir in

    let helpers = [ ("get_all_pages", get_all_pages_helper all_pages) ] in
    let partials = collect_partials partials_dir in

    let static_files_copied =
      if Sys.file_exists static_dir && Sys.is_directory static_dir then
        FileUtils.cp_r static_dir output_dir
      else 0
    in

    let () =
      all_pages
      |> List.iter @@ fun page ->
         generate_page ~helpers ~partials ~output_dir ~templates_dir ~site ~page
    in

    let info_table =
      [
        ("-------------------", "----");
        ("Rules", string_of_int (List.length site.rules));
        ("Pages", string_of_int (List.length all_pages));
        ("Static Files", string_of_int static_files_copied);
      ]
      |> Hashtbl.fold
           (fun rule_name count acc ->
             (Printf.sprintf "%s pages" rule_name, string_of_int count) :: acc)
           count_by_rule
      |> List.rev
    in
    print_ascii_table info_table
end
