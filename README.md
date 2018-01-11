# .emacs
My personal .emacs file for Rust development. It contains:

- rust-mode.el (Syntax parsing for rust files)
- dash.el (List library for Emacs)
- flycheck.el (On-the-fly syntax checking)
- flycheck-rust.el (Syntax-checking specific to rust)
- cargo-process.el (Commands for cargo)
- cargo.el (Settings for cargo)

Additionally it contains other settings, most of which are from Casey Muratorys .emacs file. I am too lazy setting things up every time and I only want to copy one file. Should work "out of the box", without any additional packages.

If if doesn't work for you, there is a high chance that line 15128 is wrong:

```
(setq current-project-home "~/Development/mapedit/")
```

You have to adjust this to your own project directory. I usually leave it at the directory that I am currently working in.

## Installation

```
curl https://raw.githubusercontent.com/fschutt/.emacs/master/.emacs > ~/.emacs
```

## Shortcuts

```
CTRL + O            Open file in current active buffer
CTRL + SHIFT + O    Open file in other buffer
CTRL + U            Load buffer in current window
CTRL + T            Load buffer in other window
CTRL + D            Switch to other window (override delete in front)
CTRL + M            Newline and indent
CTRL + S            Save buffer
CTRL + J            Search + replace
CTRL + L            Select current line
CTRL + SHIFT + L    Duplicate current line
CTRL + SPACE        Set mark

CTRL + Q            Copy region
CTRL + X            Cut region
CTRL + V            Paste region
CTRL + G            Go to line
CTRL + Z            Undo
TAB                 Autocomplete

CTRL + A            Go to beginning of line
CTRL + SHIFT + A    Go to beginning of buffer
CTRL + E            Go to end of line
CTRL + SHIFT + E    Go to end of buffer

CTRL + P            Previous line
CTRL + SHIFT + P    Previous blank line
CTRL + N            Next line
CTRL + SHIFT + N    Next blank line
CTRL + TAB          Indent region
    
ALT + Z             Kill region
ALT + BACKSPACE     Kill last word
ALT + DELETE        Kill next word
ALT + R             Revert buffer
ALT + K             Kill buffer
ALT + P             Maximize current buffer
ALT + W             Maximize other buffer
ALT + :             Jump view back to last mark
ALT + ;             Exchange point and mark
ALT + 6             Uppercase words first letter
ALT + ^             Capitalize word
ALT + .             Fill paragraph
ALT + .             Replace in region (from last seen mark)
ALT + [             Start keyboard macro
ALT + ]             End keyboard macro

SHIFT + TAB         Actually tab
HOME                Go to begin of line
END                 Go to end of line
PGUP                Forward page
PGDOWN              Backward page

F8                  Replace
F9                  Go to first error
F10                 Go to previous error
F11                 Go to next error

CTRL + C, CTRL + SHIFT + F      rustfmt
CTRL + C, CTRL + E              cargo bench
CTRL + C, CTRL + B              cargo build
CTRL + C, CTRL + L              cargo clean
CTRL + C, CTRL + D              cargo doc
CTRL + C, CTRL + V              cargo doc --open
CTRL + C, CTRL + N              cargo new
CTRL + C, CTRL + I              cargo init
CTRL + C, CTRL + R              cargo run (also CTRL + ALT + B)
CTRL + C, CTRL + X              cargo run --example
CTRL + C, CTRL + S              cargo search
CTRL + C, CTRL + T              cargo test
CTRL + C, CTRL + U              cargo update
CTRL + C, CTRL + C              cargo repeat
CTRL + C, CTRL + F              cargo test --current
CTRL + C, CTRL + O              cargo test --current-file
CTRL + C, CTRL + M              cargo fmt
CTRL + C, CTRL + M              cargo check (also CTRL + SHIFT + B)
CTRL + C, CTRL + SHIFT + K      cargo clippy
 
CTRL + SHIFT + B                cargo check
CTRL + ALT + B                  cargo run
```

This file is made up of multiple copy-pasted licenses. I don't care about licensing.
