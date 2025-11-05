(** Static site generator library for Raga *)

open Handlebars_ml

val hb_literal_of_huml : Huml.t -> Huml.t
(** Convert HUML values to handlebars-compatible literals *)

(** Page rule configuration *)
module PageRule : sig
  (** Exclusion rules *)
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
    enable_frontmatter: bool;
  }
  (** Page rule definition *)

  val of_assoc : name:string -> (string * Huml.t) list -> t
  (** Create page rule from HUML association *)
end

(** Page representation *)
module Page : sig
  type t = {
    content : string;  (** Page content without frontmatter *)
    frontmatter : (string * Huml.t) list;  (** Parsed frontmatter as HUML *)
    src_path : string option;  (** Source file path *)
    dst_path : string;  (** Destination file path *)
    url : string;  (** Generated URL *)
    title : string;  (** Page title *)
    rule : PageRule.t;  (** Page rule applied *)
  }

  val to_huml : t -> Huml.t
  (** Convert page to HUML representation *)
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
  type t = {
    name : string; (* for error reporting *)
    content : string;
    context : Huml.t;
    partials : (string * string) list;
    helpers : (string * Handlebars.custom_helper) list;
  }

  val render : t -> string
  (** Render template with context *)
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

  val cp_r : string -> string -> int
  (** Recursively copy directory *)
end

(** Site generation *)
module Generator : sig
  val generate : Site.t -> string -> string -> unit
  (** Generate static site *)
end
