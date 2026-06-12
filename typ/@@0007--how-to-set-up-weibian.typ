#import "/_template/template.typ": ln, template, tr
#show: template(
  title: [How to set up Weibian],
  date: datetime(year: 2025, month: 08, day: 19, hour: 22, minute: 12, second: 55),
  tags: (),
  author: (ln("wb:hanwenguo")[Hanwen Guo],),
  identifier: "0007",
)

Weibian is implemented in Rust. To set up Weibian on your local machine, you can either install using `cargo`:

```sh
cargo install --locked --git https://github.com/hanwenguo/weibian.git
```

Or download the prebuilt binary from the #link("https://github.com/hanwenguo/weibian/releases")[releases page].

Weibian uses a simple project structure to organize your notes and resources. Say you have a directory called `notes/` to store all your notes. Usually, you would want to have a structure like this:

```plain
notes/
├── weibian.toml  # Configuration file (optional)
├── dist/         # Default output directory
│   └── ...
└── typ/          # Note files and Typst templates
    ├── _template/
    │   └── ...
    ├── public/   # Bundle assets for the default public_dir
    │   └── ...
    └── ...
```

Most of the above paths are the default configuration, which can be overridden by passing command line arguments or by adding `weibian.toml` in the project directory. Rendering behavior belongs in Typst templates under `typ/`.
