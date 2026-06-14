#import "/_template/template.typ": template, tr, ln, inline-tree
#show: template(
  title:      [Writing in Weibian],
  date:       datetime(year: 2025, month: 08, day: 19, hour: 22, minute: 13, second: 29),
  tags:       (),
  author: (ln("wb:hanwenguo")[Hanwen Guo],),
  identifier: "0008",
)

Weibian works by compiling a Typst bundle. The process is intentionally thin: it discovers Typst files and public assets, generates a small entrypoint in memory, and pipes that entrypoint to the Typst compiler which compiles to the bundle target. Rendering behavior lives in Typst templates.

#inline-tree(
  identifier: "rendering-process",
  title: "Rendering Process",
  expanded: false
)[
To quickly get started, use the default templates in the `typ` directory in the repository of Weibian. Those templates define note pages, transclusions, links, citations, backmatter, and table of contents using Typst's bundle export and introspection features.

At compile time, Weibian scans `input_dir` recursively and collects `.typ` files whose paths match the configured include/exclude globs. These globs are matched relative to `input_dir`, and exclude rules have priority. Weibian then generates an entrypoint containing one root-relative include per discovered source:

```typ
#include "/@@0008--writing-in-weibian.typ"
#include "/blog/@@0001--typst-finds-a-sweet-spot-for-taking-scientific-notes.typ"
```

Weibian also scans `public_dir` for regular files and emits bundle assets. `public_dir` must be inside `input_dir`; when it is relative, it is resolved under `input_dir`, so `public_dir = "public"` means `typ/public` for the default project layout.

```typ
#asset("css/weibian.css", read("/public/css/weibian.css", encoding: none))
```

The generated entrypoint is not written to disk. Conceptually, Weibian runs:

```bash
typst compile --features=bundle,html --format=bundle --root=typ - dist
```

The templates loaded by the included files are responsible for constructing `document(...)` values for HTML pages and PDFs. In the default templates, each note calls `#show: template(...)`; the template wraps the note body in a bundle document, records note metadata through Typst labels/metadata, and uses Typst context and introspection to render transclusions, backlinks, contexts, references, related links, and the table of contents.

The linking helpers such as `ln`, `ct`, and `tr` are ordinary Typst functions supplied by the template. This means Weibian no longer emits or consumes custom HTML elements, no longer parses intermediate HTML, and no longer renders Tera templates.
]

#inline-tree(
  identifier: "using-configuration-file",
  title: "Using Configuration File"
)[
Weibian supports a `weibian.toml` configuration file in the current directory to set project options such as input/output directories, the public assets directory, include/exclude globs, and site options. CLI flags override config values.

The following is an example configuration file:

```toml
[files]
input_dir = "typ"
output_dir = "dist"
public_dir = "public"
include = ["**/*.typ"]
exclude = ["index.typ", "_template/*.typ"]

[site]
domain = "example.com"
root_dir = "/"
trailing_slash = true
```

If you keep a handwritten bundle entrypoint such as `typ/index.typ`, exclude it explicitly. Weibian respects filters exactly and will include `index.typ` if the filters allow it.

The `[site]` settings are passed to the Typst compiler as `--input` values:

- `wb-domain`
- `wb-root-dir`
- `wb-trailing-slash`

The default templates use these inputs for link generation. The `root_dir` and `trailing_slash` values do not directly determine where Rust writes files; bundle output paths are chosen by the Typst `document(...)` elements created by the templates.

`wb compile` also mirrors most Typst compile options, including `--input`, font and package paths, `--creation-timestamp`, `--pretty`, `--pages`, `--pdf-standard`, `--no-pdf-tags`, `--ppi`, `--deps`, `--deps-format`, `--jobs`, `--diagnostic-format`, `--open`, and `--timings`. Weibian owns the input path, output path, Typst root, output format, and required experimental features.
]

#tr("wb:0009", expanded: false)
