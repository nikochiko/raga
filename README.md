# Raga

Raga is a static site generator.

## Installation

You need `dune`, `opam`, and `ocaml` to be pre-installed. Then run:

```
git clone https://github.com/nikochiko/raga
cd raga
opam install .
```

## Usage

### Directory Structure

`raga` works on a directory structure like this:

```
site/
├── config.huml
├── content/
│   ├── about.md
│   ├── home.md
│   └── posts/
│       ├── first-post.md
│       └── second-post.md
├── templates/
│   │── post.hbs
│   ├── home.hbs
│   └── partials/
│       └── header.hbs
└── static/
    ├── css/
    │   └── styles.css
    └── js/
        └── scripts.js
```

- `content/` contains markdown & other files for your site content. This is content
that will be processed and converted to HTML.
- `templates/` contains Handlebars templates for rendering your content.
path is configurable.
- `static/` contains static assets like CSS, JavaScript, and images that will be copied
as-is to the output directory.

### Configuration

This goes into `config.huml`:

```huml
title: "alice"
base_url: "https://alice.example.com"

pages::
  home::
    src: "home.md"
    dst: "index.html"
    tmpl: "home.hbs"
  rss::
    dst: "rss.xml"
    tmpl: "rss.hbs"
  posts::
    src:: glob: "{**/*,*}.md"
    excl::
      frontmatter:: draft: true
      glob: "{**/_*,_*}.md"
    dst: """
      {{ concat_path (chop_suffix src_path ".md") "index.html" }}
    """
    tmpl: "post.hbs"

params::
  social_links::
    github::
      url: "https://github.com/nikochiko"
      label: "gh/nikochiko"
    email::
      url: "mailto:k@kaustubh.page"
      label: "k@kaustubh.page"
  header_links::
    Home: "/"
    Blog: "/posts/"
```

* `title`: The title of your website. This is available to templates as `{{ site.title }}`.
* `base_url`: The base URL of your website. This is available to templates as `{{ site.base_url }}`.
* `pages`: Defines the pages to be generated. Each page rule can specify:
    * `src`: The source files for this rule. This is a glob pattern relative to the `content/` directory.
      This can also be omitted for pages that don't have a source file (e.g., RSS feeds).
    * `excl`: Exclusion patterns for source files. This supports:
        * `glob`: A glob pattern to exclude files.
        * `frontmatter`: Key-value pairs to exclude files based on frontmatter metadata.
    * `dst`: The destination path for the generated page. This can use template expressions. Required.
    * `tmpl`: The Handlebars template to use for rendering this page. This is relative to the `templates/` directory.
      Required.
* `params`: Custom parameters that can be accessed in templates via `{{ site.params.<param_name> }}`.

Look at `examples/blog` for a complete example.

### CLI

To generate the site, run this command from the root of your site directory:

```
raga . --output ./_site
```

The website will be built to the `_site` directory by default.

## LICENSE

MIT.
