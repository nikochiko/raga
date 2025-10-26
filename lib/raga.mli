(** Static site generator library for Raga *)

val hb_literal_of_huml : Huml.t -> Huml.t
(** Convert HUML values to handlebars-compatible literals *)

(** Page representation *)
module Page : sig
  type t = {
    content : string;  (** Page content without frontmatter *)
    frontmatter : (string * Huml.t) list;  (** Parsed frontmatter as HUML *)
    src_path : string option;  (** Source file path *)
    dst_path : string;  (** Destination file path *)
    url : string;  (** Generated URL *)
    title : string;  (** Page title *)
  }

  val to_huml : t -> Huml.t
  (** Convert page to HUML representation *)
end

(** Page rule configuration *)
module PageRule : sig
  (** Source specification *)
  type src = SrcPath of string | SrcGlob of string

  (** Exclusion rules *)
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
  (** Page rule definition *)

  val of_assoc : name:string -> (string * Huml.t) list -> t
  (** Create page rule from HUML association *)
end

(** Site configuration *)
module Site : sig
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

  val of_assoc : (string * Huml.t) list -> t
  (** Create site from HUML association *)

  val of_huml : Huml.t -> t
  (** Create site from HUML document *)

  val to_huml : t -> Huml.t
  (** Convert site to HUML representation *)
end

(** Template processing *)
module Template : sig
  type env = {
    templates_dir : string;
    partials_dir : string;
    pages : Page.t list;
  }
  (** Template environment *)

  val mk_env : Page.t list -> string -> string -> env
  (** Create template environment *)

  val render : env -> string -> Huml.t -> string
  (** Render template with context *)

  val get_helper : env -> string -> (Huml.t list -> Huml.t option) option
  (** Get handlebars helper function *)

  val get_partial : env -> string -> string option
  (** Get partial template *)
end

(** File processing utilities *)
module FileUtils : sig
  val read_file : string -> string
  (** Read entire file content *)

  val extract_frontmatter : string -> string * string
  (** Extract frontmatter from content, returns (frontmatter, content) *)

  val parse_frontmatter : string -> (string * Huml.t) list
  (** Parse frontmatter string as HUML *)

  val rm_r : string -> unit
  (** Recursively remove directory *)

  val mv_r : string -> string -> unit
  (** Recursively move directory *)

  val cp : string -> string -> unit
  (** Copy file *)

  val cp_r : string -> string -> unit
  (** Recursively copy directory *)
end

(** Site generation *)
module Generator : sig
  val generate : Site.t -> string -> string -> unit
  (** Generate static site *)
end
