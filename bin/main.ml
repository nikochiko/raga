let usage_msg = "raga [OPTIONS] SOURCE_DIR"
let config_file = ref "config.huml"
let output_dir = ref "" (* Will be set to src_dir/_site by default *)
let base_url_override = ref "" (* Will override config base_url if provided *)
let source_dir = ref ""
let set_source_dir s = source_dir := s

let show_version () =
  Printf.printf "raga %s\n" Version.version;
  exit 0

let show_help ?exit_code () =
  Printf.printf "raga [OPTIONS] SOURCE_DIR\n\n";
  Printf.printf "Generate static sites from templates and content.\n\n";
  Printf.printf "Examples:\n";
  Printf.printf "  raga my-blog/\n";
  Printf.printf "  raga --output /var/www/html my-site/\n";
  Printf.printf
    "  raga --config prod.huml --base-url https://example.com my-site/\n\n";
  Printf.printf "Options:\n";
  Printf.printf
    "  --config FILE       Configuration file (default: config.huml)\n";
  Printf.printf
    "  --output DIR        Output directory (default: SOURCE_DIR/_site)\n";
  Printf.printf "  -b, --base-url URL  Override base URL from config\n";
  Printf.printf "  -h, --help          Show this help\n";
  Printf.printf "  -v, --version       Show version\n";
  exit (match exit_code with Some code -> code | None -> 0)

let spec_list =
  [
    ("--config", Arg.Set_string config_file, "");
    ("--output", Arg.Set_string output_dir, "");
    ("--base-url", Arg.Set_string base_url_override, "");
    ("-b", Arg.Set_string base_url_override, "");
    ("--help", Arg.Unit (fun () -> show_help ()), "");
    ("-h", Arg.Unit (fun () -> show_help ()), "");
    ("--version", Arg.Unit show_version, "");
    ("-v", Arg.Unit show_version, "");
    ("-help", Arg.Unit (fun () -> show_help ()), "");
  ]

let mk_temp_dir prefix =
  let temp_dir = Filename.get_temp_dir_name () in
  let mk_random n =
    let chars =
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    in
    String.init n (fun _ -> String.get chars (Random.int (String.length chars)))
  in
  let rec find_unique_dir () =
    let dir_name = Printf.sprintf "%s%s%s" prefix "_" (mk_random 16) in
    let full_path = Filename.concat temp_dir dir_name in
    if Sys.file_exists full_path then find_unique_dir ()
    else full_path
  in
  let unique_dir = find_unique_dir () in
  Sys.mkdir unique_dir 0o700;
  unique_dir

let () =
  Arg.parse spec_list set_source_dir usage_msg;

  if !source_dir = "" then (
    Printf.eprintf "Error: Please specify a source directory\n\n";
    show_help ~exit_code:1 ());

  (* Set default output directory if not specified *)
  let final_output_dir =
    if !output_dir = "" then Filename.concat !source_dir "_site"
    else !output_dir
  in

  let config_path = Filename.concat !source_dir !config_file in

  if not (Sys.file_exists config_path) then (
    Printf.eprintf "Error: Configuration file not found: %s\n" config_path;
    exit 1);

  let config_content = Raga.FileUtils.read_file config_path in
  let lexbuf = Lexing.from_string config_content in
  let config_huml =
    match Huml.parse lexbuf with
    | Ok ast -> ast
    | Error msg -> failwith ("HUML parse error: " ^ msg)
  in
  let site = Raga.Site.of_huml config_huml in

  (* Override base_url if provided via command line *)
  let site =
    if !base_url_override = "" then site
    else { site with base_url = Some !base_url_override }
  in

  let init_time = Sys.time () in
  Printf.eprintf "raga %s\n" Version.version;
  Printf.eprintf "Generating site...\n\n";

  let tmp_output_dir = mk_temp_dir "raga_output_" in
  Raga.Generator.generate site !source_dir tmp_output_dir;
  Raga.FileUtils.mv_r tmp_output_dir final_output_dir;

  let end_time = Sys.time () in
  Printf.eprintf "\nGenerated in %.1f seconds\n" (end_time -. init_time)
