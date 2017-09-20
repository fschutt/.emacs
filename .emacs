; Modified emacs file from Casey Muratorys .emacs file, but for Rust instead of C++

; ------------------------------------ RUST SETTINGS --------------------------------------- ;

    ;;; rust-mode.el --- A major emacs mode for editing Rust source code -*-lexical-binding: t-*-

    ;; Version: 0.3.0
    ;; Author: Mozilla
    ;; Url: https://github.com/rust-lang/rust-mode
    ;; Keywords: languages
    ;; Package-Requires: ((emacs "24.0"))

    ;; This file is distributed under the terms of both the MIT license and the
    ;; Apache License (version 2.0).

    ;;; Commentary:
    ;;

    ;;; Code:

    (eval-when-compile (require 'rx)
                       (require 'compile)
                       (require 'url-vars))

    (require 'json)

    (defvar electric-pair-inhibit-predicate)
    (defvar electric-indent-chars)

    (defvar rust-buffer-project)
    (make-variable-buffer-local 'rust-buffer-project)

    ;; for GNU Emacs < 24.3
    (eval-when-compile
      (unless (fboundp 'setq-local)
        (defmacro setq-local (var val)
          "Set variable VAR to value VAL in current buffer."
          (list 'set (list 'make-local-variable (list 'quote var)) val))))

    (defconst rust-re-ident "[[:word:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")
    (defconst rust-re-lc-ident "[[:lower:][:multibyte:]_][[:word:][:multibyte:]_[:digit:]]*")
    (defconst rust-re-uc-ident "[[:upper:]][[:word:][:multibyte:]_[:digit:]]*")
    (defconst rust-re-vis "pub")
    (defconst rust-re-unsafe "unsafe")
    (defconst rust-re-extern "extern")
    (defconst rust-re-union
      (rx-to-string
       `(seq
        (or space line-start)
        (group symbol-start "union" symbol-end)
        (+ space) (regexp ,rust-re-ident))))

    ;;; Start of a Rust item
    (defvar rust-top-item-beg-re
      (concat "\\s-*\\(?:priv\\|pub\\)?\\s-*"
              (regexp-opt
               '("enum" "struct" "union" "type" "mod" "use" "fn" "static" "impl"
                 "extern" "trait"))
          "\\_>"))

    (defun rust-looking-back-str (str)
      "Like `looking-back' but for fixed strings rather than regexps (so that it's not so slow)"
      (let ((len (length str)))
        (and (> (point) len)
             (equal str (buffer-substring-no-properties (- (point) len) (point))))))

    (defun rust-looking-back-symbols (SYMS)
      "Return non-nil if the point is just after a complete symbol that is a member of the list of strings SYMS"
      (save-excursion
        (let* ((pt-orig (point))
               (beg-of-symbol (progn (forward-thing 'symbol -1) (point)))
               (end-of-symbol (progn (forward-thing 'symbol 1) (point))))
          (and
           (= end-of-symbol pt-orig)
           (member (buffer-substring-no-properties beg-of-symbol pt-orig) SYMS)))))

    (defun rust-looking-back-ident ()
      "Non-nil if we are looking backwards at a valid rust identifier"
      (let ((beg-of-symbol (save-excursion (forward-thing 'symbol -1) (point))))
        (looking-back rust-re-ident beg-of-symbol)))

    (defun rust-looking-back-macro ()
      "Non-nil if looking back at an ident followed by a !"
      (save-excursion (backward-char) (and (= ?! (char-after)) (rust-looking-back-ident))))

    ;; Syntax definitions and helpers
    (defvar rust-mode-syntax-table
      (let ((table (make-syntax-table)))

        ;; Operators
        (dolist (i '(?+ ?- ?* ?/ ?& ?| ?^ ?! ?< ?> ?~ ?@))
          (modify-syntax-entry i "." table))

        ;; Strings
        (modify-syntax-entry ?\" "\"" table)
        (modify-syntax-entry ?\\ "\\" table)

        ;; Angle brackets.  We suppress this with syntactic propertization
        ;; when needed
        (modify-syntax-entry ?< "(>" table)
        (modify-syntax-entry ?> ")<" table)

        ;; Comments
        (modify-syntax-entry ?/  ". 124b" table)
        (modify-syntax-entry ?*  ". 23n"  table)
        (modify-syntax-entry ?\n "> b"    table)
        (modify-syntax-entry ?\^m "> b"   table)

        table))

    (defgroup rust-mode nil
      "Support for Rust code."
      :link '(url-link "https://www.rust-lang.org/")
      :group 'languages)

    (defcustom rust-indent-offset 4
      "Indent Rust code by this number of spaces."
      :type 'integer
      :group 'rust-mode
      :safe #'integerp)

    (defcustom rust-indent-method-chain nil
      "Indent Rust method chains, aligned by the '.' operators"
      :type 'boolean
      :group 'rust-mode
      :safe #'booleanp)

    (defcustom rust-indent-where-clause t
      "Indent the line starting with the where keyword following a
    function or trait.  When nil, where will be aligned with fn or trait."
      :type 'boolean
      :group 'rust-mode
      :safe #'booleanp)

    (defcustom rust-playpen-url-format "https://play.rust-lang.org/?code=%s"
      "Format string to use when submitting code to the playpen"
      :type 'string
      :group 'rust-mode)
    (defcustom rust-shortener-url-format "https://is.gd/create.php?format=simple&url=%s"
      "Format string to use for creating the shortened link of a playpen submission"
      :type 'string
      :group 'rust-mode)

    (defcustom rust-match-angle-brackets t
      "Enable angle bracket matching.  Attempt to match `<' and `>' where
      appropriate."
      :type 'boolean
      :safe #'booleanp
      :group 'rust-mode)

    (defcustom rust-format-on-save nil
      "Format future rust buffers before saving using rustfmt."
      :type 'boolean
      :safe #'booleanp
      :group 'rust-mode)

    (defcustom rust-rustfmt-bin "rustfmt"
      "Path to rustfmt executable."
      :type 'string
      :group 'rust-mode)

    (defcustom rust-cargo-bin "cargo"
      "Path to cargo executable."
      :type 'string
      :group 'rust-mode)

    (defcustom rust-always-locate-project-on-open nil
      "Whether to run `cargo locate-project' every time `rust-mode'
      is activated."
      :type 'boolean
      :group 'rust-mode)

    (defface rust-unsafe-face
      '((t :inherit font-lock-warning-face))
      "Face for the `unsafe' keyword."
      :group 'rust-mode)

    (defface rust-question-mark-face
      '((t :weight bold :inherit font-lock-builtin-face))
      "Face for the question mark operator."
      :group 'rust-mode)

    (defface rust-builtin-formatting-macro-face
      '((t :inherit font-lock-builtin-face))
      "Face for builtin formatting macros (print! &c.)."
      :group 'rust-mode)

    (defface rust-string-interpolation-face
      '((t :slant italic :inherit font-lock-string-face))
      "Face for interpolating braces in builtin formatting macro strings."
      :group 'rust-mode)

    (defun rust-paren-level () (nth 0 (syntax-ppss)))
    (defun rust-in-str () (nth 3 (syntax-ppss)))
    (defun rust-in-str-or-cmnt () (nth 8 (syntax-ppss)))
    (defun rust-rewind-past-str-cmnt () (goto-char (nth 8 (syntax-ppss))))

    (defun rust-rewind-irrelevant ()
      (let ((continue t))
        (while continue
          (let ((starting (point)))
            (skip-chars-backward "[:space:]\n")
            (when (rust-looking-back-str "*/")
              (backward-char))
            (when (rust-in-str-or-cmnt)
              (rust-rewind-past-str-cmnt))
            ;; Rewind until the point no longer moves
            (setq continue (/= starting (point)))))))


    (defun rust-in-macro ()
      (save-excursion
        (when (> (rust-paren-level) 0)
          (backward-up-list)
          (rust-rewind-irrelevant)
          (or (rust-looking-back-macro)
              (and (rust-looking-back-ident) (save-excursion (backward-sexp) (rust-rewind-irrelevant) (rust-looking-back-str "macro_rules!")))
              (rust-in-macro))
          )))

    (defun rust-looking-at-where ()
      "Return T when looking at the \"where\" keyword."
      (and (looking-at-p "\\bwhere\\b")
           (not (rust-in-str-or-cmnt))))

    (defun rust-rewind-to-where (&optional limit)
      "Rewind the point to the closest occurrence of the \"where\" keyword.
    Return T iff a where-clause was found.  Does not rewind past
    LIMIT when passed, otherwise only stops at the beginning of the
    buffer."
      (when (re-search-backward "\\bwhere\\b" limit t)
        (if (rust-in-str-or-cmnt)
            (rust-rewind-to-where limit)
          t)))

    (defun rust-align-to-expr-after-brace ()
      (save-excursion
        (forward-char)
        ;; We don't want to indent out to the open bracket if the
        ;; open bracket ends the line
        (when (not (looking-at "[[:blank:]]*\\(?://.*\\)?$"))
          (when (looking-at "[[:space:]]")
        (forward-word 1)
        (backward-word 1))
          (current-column))))

    (defun rust-rewind-to-beginning-of-current-level-expr ()
      (let ((current-level (rust-paren-level)))
        (back-to-indentation)
        (when (looking-at "->")
          (rust-rewind-irrelevant)
          (back-to-indentation))
        (while (> (rust-paren-level) current-level)
          (backward-up-list)
          (back-to-indentation))
        ;; When we're in the where clause, skip over it.  First find out the start
        ;; of the function and its paren level.
        (let ((function-start nil) (function-level nil))
          (save-excursion
            (rust-beginning-of-defun)
            (back-to-indentation)
            ;; Avoid using multiple-value-bind
            (setq function-start (point)
                  function-level (rust-paren-level)))
          ;; On a where clause
          (when (or (rust-looking-at-where)
                    ;; or in one of the following lines, e.g.
                    ;; where A: Eq
                    ;;       B: Hash <- on this line
                    (and (save-excursion
                           (rust-rewind-to-where function-start))
                         (= current-level function-level)))
            (goto-char function-start)))))

    (defun rust-align-to-method-chain ()
      (save-excursion
        ;; for method-chain alignment to apply, we must be looking at
        ;; another method call or field access or something like
        ;; that. This avoids rather "eager" jumps in situations like:
        ;;
        ;; {
        ;;     something.foo()
        ;; <indent>
        ;;
        ;; Without this check, we would wind up with the cursor under the
        ;; `.`. In an older version, I had the inverse of the current
        ;; check, where we checked for situations that should NOT indent,
        ;; vs checking for the one situation where we SHOULD. It should be
        ;; clear that this is more robust, but also I find it mildly less
        ;; annoying to have to press tab again to align to a method chain
        ;; than to have an over-eager indent in all other cases which must
        ;; be undone via tab.

        (when (looking-at (concat "\s*\." rust-re-ident))
          (forward-line -1)
          (end-of-line)
          ;; Keep going up (looking for a line that could contain a method chain)
          ;; while we're in a comment or on a blank line. Stop when the paren
          ;; level changes.
          (let ((level (rust-paren-level)))
            (while (and (or (rust-in-str-or-cmnt)
                            ;; Only whitespace (or nothing) from the beginning to
                            ;; the end of the line.
                            (looking-back "^\s*" (point-at-bol)))
                        (= (rust-paren-level) level))
              (forward-line -1)
              (end-of-line)))

          (let
              ;; skip-dot-identifier is used to position the point at the
              ;; `.` when looking at something like
              ;;
              ;;      foo.bar
              ;;         ^   ^
              ;;         |   |
              ;;         |  position of point
              ;;       returned offset
              ;;
              ((skip-dot-identifier
                (lambda ()
                  (when (and (rust-looking-back-ident) (save-excursion (forward-thing 'symbol -1) (= ?. (char-before))))
                    (forward-thing 'symbol -1)
                    (backward-char)
                    (- (current-column) rust-indent-offset)))))
            (cond
             ;; foo.bar(...)
             ((rust-looking-back-str ")")
              (backward-list 1)
              (funcall skip-dot-identifier))

             ;; foo.bar
             (t (funcall skip-dot-identifier)))))))

    (defun rust-mode-indent-line ()
      (interactive)
      (let ((indent
             (save-excursion
               (back-to-indentation)
               ;; Point is now at beginning of current line
               (let* ((level (rust-paren-level))
                      (baseline
                       ;; Our "baseline" is one level out from the indentation of the expression
                       ;; containing the innermost enclosing opening bracket.  That
                       ;; way if we are within a block that has a different
                       ;; indentation than this mode would give it, we still indent
                       ;; the inside of it correctly relative to the outside.
                       (if (= 0 level)
                           0
                         (or
                          (when rust-indent-method-chain
                            (rust-align-to-method-chain))
                          (save-excursion
                            (rust-rewind-irrelevant)
                            (backward-up-list)
                            (rust-rewind-to-beginning-of-current-level-expr)
                            (+ (current-column) rust-indent-offset))))))
                 (cond
                  ;; Indent inside a non-raw string only if the the previous line
                  ;; ends with a backslash that is inside the same string
                  ((nth 3 (syntax-ppss))
                   (let*
                       ((string-begin-pos (nth 8 (syntax-ppss)))
                        (end-of-prev-line-pos (when (> (line-number-at-pos) 1)
                                                (save-excursion
                                                  (forward-line -1)
                                                  (end-of-line)
                                                  (point)))))
                     (when
                         (and
                          ;; If the string begins with an "r" it's a raw string and
                          ;; we should not change the indentation
                          (/= ?r (char-after string-begin-pos))

                          ;; If we're on the first line this will be nil and the
                          ;; rest does not apply
                          end-of-prev-line-pos

                          ;; The end of the previous line needs to be inside the
                          ;; current string...
                          (> end-of-prev-line-pos string-begin-pos)

                          ;; ...and end with a backslash
                          (= ?\\ (char-before end-of-prev-line-pos)))

                       ;; Indent to the same level as the previous line, or the
                       ;; start of the string if the previous line starts the string
                       (if (= (line-number-at-pos end-of-prev-line-pos) (line-number-at-pos string-begin-pos))
                           ;; The previous line is the start of the string.
                           ;; If the backslash is the only character after the
                           ;; string beginning, indent to the next indent
                           ;; level.  Otherwise align with the start of the string.
                           (if (> (- end-of-prev-line-pos string-begin-pos) 2)
                               (save-excursion
                                 (goto-char (+ 1 string-begin-pos))
                                 (current-column))
                             baseline)

                         ;; The previous line is not the start of the string, so
                         ;; match its indentation.
                         (save-excursion
                           (goto-char end-of-prev-line-pos)
                           (back-to-indentation)
                           (current-column))))))

                  ;; A function return type is indented to the corresponding function arguments
                  ((looking-at "->")
                   (save-excursion
                     (backward-list)
                     (or (rust-align-to-expr-after-brace)
                         (+ baseline rust-indent-offset))))

                  ;; A closing brace is 1 level unindented
                  ((looking-at "[]})]") (- baseline rust-indent-offset))

                  ;; Doc comments in /** style with leading * indent to line up the *s
                  ((and (nth 4 (syntax-ppss)) (looking-at "*"))
                   (+ 1 baseline))

                  ;; When the user chose not to indent the start of the where
                  ;; clause, put it on the baseline.
                  ((and (not rust-indent-where-clause)
                        (rust-looking-at-where))
                   baseline)

                  ;; If we're in any other token-tree / sexp, then:
                  (t
                   (or
                    ;; If we are inside a pair of braces, with something after the
                    ;; open brace on the same line and ending with a comma, treat
                    ;; it as fields and align them.
                    (when (> level 0)
                      (save-excursion
                        (rust-rewind-irrelevant)
                        (backward-up-list)
                        ;; Point is now at the beginning of the containing set of braces
                        (rust-align-to-expr-after-brace)))

                    ;; When where-clauses are spread over multiple lines, clauses
                    ;; should be aligned on the type parameters.  In this case we
                    ;; take care of the second and following clauses (the ones
                    ;; that don't start with "where ")
                    (save-excursion
                      ;; Find the start of the function, we'll use this to limit
                      ;; our search for "where ".
                      (let ((function-start nil) (function-level nil))
                        (save-excursion
                          ;; If we're already at the start of a function,
                          ;; don't go back any farther.  We can easily do
                          ;; this by moving to the end of the line first.
                          (end-of-line)
                          (rust-beginning-of-defun)
                          (back-to-indentation)
                          ;; Avoid using multiple-value-bind
                          (setq function-start (point)
                                function-level (rust-paren-level)))
                        ;; When we're not on a line starting with "where ", but
                        ;; still on a where-clause line, go to "where "
                        (when (and
                               (not (rust-looking-at-where))
                               ;; We're looking at something like "F: ..."
                               (looking-at (concat rust-re-ident ":"))
                               ;; There is a "where " somewhere after the
                               ;; start of the function.
                               (rust-rewind-to-where function-start)
                               ;; Make sure we're not inside the function
                               ;; already (e.g. initializing a struct) by
                               ;; checking we are the same level.
                               (= function-level level))
                          ;; skip over "where"
                          (forward-char 5)
                          ;; Unless "where" is at the end of the line
                          (if (eolp)
                              ;; in this case the type parameters bounds are just
                              ;; indented once
                              (+ baseline rust-indent-offset)
                            ;; otherwise, skip over whitespace,
                            (skip-chars-forward "[:space:]")
                            ;; get the column of the type parameter and use that
                            ;; as indentation offset
                            (current-column)))))

                    (progn
                      (back-to-indentation)
                      ;; Point is now at the beginning of the current line
                      (if (or
                           ;; If this line begins with "else" or "{", stay on the
                           ;; baseline as well (we are continuing an expression,
                           ;; but the "else" or "{" should align with the beginning
                           ;; of the expression it's in.)
                           ;; Or, if this line starts a comment, stay on the
                           ;; baseline as well.
                           (looking-at "\\<else\\>\\|{\\|/[/*]")

                           ;; If this is the start of a top-level item,
                           ;; stay on the baseline.
                           (looking-at rust-top-item-beg-re)

                           (save-excursion
                             (rust-rewind-irrelevant)
                             ;; Point is now at the end of the previous line
                             (or
                              ;; If we are at the start of the buffer, no
                              ;; indentation is needed, so stay at baseline...
                              (= (point) 1)
                              ;; ..or if the previous line ends with any of these:
                              ;;     { ? : ( , ; [ }
                              ;; then we are at the beginning of an expression, so stay on the baseline...
                              (looking-back "[(,:;?[{}]\\|[^|]|" (- (point) 2))
                              ;; or if the previous line is the end of an attribute, stay at the baseline...
                              (progn (rust-rewind-to-beginning-of-current-level-expr) (looking-at "#")))))
                          baseline

                        ;; Otherwise, we are continuing the same expression from the previous line,
                        ;; so add one additional indent level
                        (+ baseline rust-indent-offset))))))))))

        (when indent
          ;; If we're at the beginning of the line (before or at the current
          ;; indentation), jump with the indentation change.  Otherwise, save the
          ;; excursion so that adding the indentations will leave us at the
          ;; equivalent position within the line to where we were before.
          (if (<= (current-column) (current-indentation))
              (indent-line-to indent)
            (save-excursion (indent-line-to indent))))))


    ;; Font-locking definitions and helpers
    (defconst rust-mode-keywords
      '("as"
        "box" "break"
        "const" "continue" "crate"
        "do"
        "else" "enum" "extern"
        "false" "fn" "for"
        "if" "impl" "in"
        "let" "loop"
        "match" "mod" "move" "mut"
        "priv" "pub"
        "ref" "return"
        "self" "static" "struct" "super"
        "true" "trait" "type"
        "use"
        "virtual"
        "where" "while"))

    (defconst rust-special-types
      '("u8" "i8"
        "u16" "i16"
        "u32" "i32"
        "u64" "i64"
        "u128" "i128"

        "f32" "f64"
        "isize" "usize"
        "bool"
        "str" "char"))

    (defconst rust-re-type-or-constructor
      (rx symbol-start
          (group upper (0+ (any word nonascii digit "_")))
          symbol-end))

    (defconst rust-re-pre-expression-operators "[-=!%&*/:<>[{(|.^;}]")
    (defun rust-re-word (inner) (concat "\\<" inner "\\>"))
    (defun rust-re-grab (inner) (concat "\\(" inner "\\)"))
    (defun rust-re-shy (inner) (concat "\\(?:" inner "\\)"))
    (defun rust-re-item-def (itype)
      (concat (rust-re-word itype) "[[:space:]]+" (rust-re-grab rust-re-ident)))
    (defun rust-re-item-def-imenu (itype)
      (concat "^[[:space:]]*"
              (rust-re-shy (concat (rust-re-word rust-re-vis) "[[:space:]]+")) "?"
              (rust-re-shy (concat (rust-re-word "default") "[[:space:]]+")) "?"
              (rust-re-shy (concat (rust-re-word rust-re-unsafe) "[[:space:]]+")) "?"
              (rust-re-shy (concat (rust-re-word rust-re-extern) "[[:space:]]+"
                                   (rust-re-shy "\"[^\"]+\"[[:space:]]+") "?")) "?"
              (rust-re-item-def itype)))

    (defconst rust-re-special-types (regexp-opt rust-special-types 'symbols))


    (defun rust-path-font-lock-matcher (re-ident)
      "Matches names like \"foo::\" or \"Foo::\" (depending on RE-IDENT, which should match
    the desired identifiers), but does not match type annotations \"foo::<\"."
      `(lambda (limit)
         (catch 'rust-path-font-lock-matcher
           (while t
             (let* ((symbol-then-colons (rx-to-string '(seq (group (regexp ,re-ident)) "::")))
                    (match (re-search-forward symbol-then-colons limit t)))
               (cond
                ;; If we didn't find a match, there are no more occurrences
                ;; of foo::, so return.
                ((null match) (throw 'rust-path-font-lock-matcher nil))
                ;; If this isn't a type annotation foo::<, we've found a
                ;; match, so a return it!
                ((not (looking-at (rx (0+ space) "<")))
             (throw 'rust-path-font-lock-matcher match))))))))

    (defun rust-next-string-interpolation (limit)
      "Search forward from point for next Rust interpolation marker
    before LIMIT.
    Set point to the end of the occurrence found, and return match beginning
    and end."
      (catch 'match
        (save-match-data
          (save-excursion
            (while (search-forward "{" limit t)
              (if (eql (char-after (point)) ?{)
                  (forward-char)
                (let ((start (match-beginning 0)))
                  ;; According to fmt_macros::Parser::next, an opening brace
                  ;; must be followed by an optional argument and/or format
                  ;; specifier, then a closing brace. A single closing brace
                  ;; without a corresponding unescaped opening brace is an
                  ;; error. We don't need to do anything special with
                  ;; arguments, specifiers, or errors, so we only search for
                  ;; the single closing brace.
                  (when (search-forward "}" limit t)
                    (throw 'match (list start (point)))))))))))

    (defun rust-string-interpolation-matcher (limit)
      "Match next Rust interpolation marker before LIMIT and set
    match data if found. Returns nil if not within a Rust string."
      (when (rust-in-str)
        (let ((match (rust-next-string-interpolation limit)))
          (when match
            (set-match-data match)
            (goto-char (cadr match))
            match))))

    (defvar rust-builtin-formatting-macros
      '("eprint"
        "eprintln"
        "format"
        "print"
        "println")
      "List of builtin Rust macros for string formatting used by `rust-mode-font-lock-keywords'. (`write!' is handled separately.)")

    (defvar rust-formatting-macro-opening-re
      "[[:space:]]*[({[][[:space:]]*"
      "Regular expression to match the opening delimiter of a Rust formatting macro.")

    (defvar rust-start-of-string-re
      "\\(?:r#*\\)?\""
      "Regular expression to match the start of a Rust raw string.")

    (defvar rust-mode-font-lock-keywords
      (append
       `(
         ;; Keywords proper
         (,(regexp-opt rust-mode-keywords 'symbols) . font-lock-keyword-face)

         ;; Contextual keywords
         ("\\_<\\(default\\)[[:space:]]+fn\\_>" 1 font-lock-keyword-face)
         (,rust-re-union 1 font-lock-keyword-face)

         ;; Special types
         (,(regexp-opt rust-special-types 'symbols) . font-lock-type-face)

         ;; The unsafe keyword
         ("\\_<unsafe\\_>" . 'rust-unsafe-face)

         ;; Attributes like `#[bar(baz)]` or `#![bar(baz)]` or `#[bar = "baz"]`
         (,(rust-re-grab (concat "#\\!?\\[" rust-re-ident "[^]]*\\]"))
          1 font-lock-preprocessor-face keep)

         ;; Builtin formatting macros
         (,(concat (rust-re-grab (concat (regexp-opt rust-builtin-formatting-macros) "!")) (concat rust-formatting-macro-opening-re rust-start-of-string-re))
          (1 'rust-builtin-formatting-macro-face)
          (rust-string-interpolation-matcher
           (rust-end-of-string)
           nil
           (0 'rust-string-interpolation-face t nil)))

         ;; write! macro
         (,(concat (rust-re-grab "write\\(ln\\)?!") (concat rust-formatting-macro-opening-re "[[:space:]]*[^\"]+,[[:space:]]*" rust-start-of-string-re))
          (1 'rust-builtin-formatting-macro-face)
          (rust-string-interpolation-matcher
           (rust-end-of-string)
           nil
           (0 'rust-string-interpolation-face t nil)))

         ;; Syntax extension invocations like `foo!`, highlight including the !
         (,(concat (rust-re-grab (concat rust-re-ident "!")) "[({[:space:][]")
          1 font-lock-preprocessor-face)

         ;; Field names like `foo:`, highlight excluding the :
         (,(concat (rust-re-grab rust-re-ident) ":[^:]") 1 font-lock-variable-name-face)

         ;; CamelCase Means Type Or Constructor
         (,rust-re-type-or-constructor 1 font-lock-type-face)

         ;; Type-inferred binding
         (,(concat "\\_<\\(?:let\\|ref\\)\\s-+\\(?:mut\\s-+\\)?" (rust-re-grab rust-re-ident) "\\_>") 1 font-lock-variable-name-face)

         ;; Type names like `Foo::`, highlight excluding the ::
         (,(rust-path-font-lock-matcher rust-re-uc-ident) 1 font-lock-type-face)

         ;; Module names like `foo::`, highlight excluding the ::
         (,(rust-path-font-lock-matcher rust-re-lc-ident) 1 font-lock-constant-face)

         ;; Lifetimes like `'foo`
         (,(concat "'" (rust-re-grab rust-re-ident) "[^']") 1 font-lock-variable-name-face)

         ;; Question mark operator
         ("\\?" . 'rust-question-mark-face)
         )

       ;; Ensure we highlight `Foo` in `struct Foo` as a type.
       (mapcar #'(lambda (x)
                   (list (rust-re-item-def (car x))
                         1 (cdr x)))
               '(("enum" . font-lock-type-face)
                 ("struct" . font-lock-type-face)
                 ("union" . font-lock-type-face)
                 ("type" . font-lock-type-face)
                 ("mod" . font-lock-constant-face)
                 ("use" . font-lock-constant-face)
                 ("fn" . font-lock-function-name-face)))))

    (defun rust-syntax-class-before-point ()
      (when (> (point) 1)
        (syntax-class (syntax-after (1- (point))))))

    (defun rust-rewind-qualified-ident ()
      (while (rust-looking-back-ident)
        (backward-sexp)
        (when (save-excursion (rust-rewind-irrelevant) (rust-looking-back-str "::"))
          (rust-rewind-irrelevant)
          (backward-char 2)
          (rust-rewind-irrelevant))))

    (defun rust-rewind-type-param-list ()
      (cond
       ((and (rust-looking-back-str ">") (equal 5 (rust-syntax-class-before-point)))
        (backward-sexp)
        (rust-rewind-irrelevant))

       ;; We need to be able to back up past the Fn(args) -> RT form as well.  If
       ;; we're looking back at this, we want to end up just after "Fn".
       ((member (char-before) '(?\] ?\) ))
        (let* ((is-paren (rust-looking-back-str ")"))
               (dest (save-excursion
                      (backward-sexp)
                      (rust-rewind-irrelevant)
                      (or
                       (when (rust-looking-back-str "->")
                         (backward-char 2)
                         (rust-rewind-irrelevant)
                         (when (rust-looking-back-str ")")
                           (backward-sexp)
                           (point)))
                       (and is-paren (point))))))
          (when dest
            (goto-char dest))))))

    (defun rust-rewind-to-decl-name ()
      "If we are before an ident that is part of a declaration that
      can have a where clause, rewind back to just before the name of
      the subject of that where clause and return the new point.
      Otherwise return nil"

      (let* ((ident-pos (point))
             (newpos (save-excursion
                       (rust-rewind-irrelevant)
                       (rust-rewind-type-param-list)
                       (cond
                           ((rust-looking-back-symbols '("fn" "trait" "enum" "struct" "union" "impl" "type")) ident-pos)

                           ((equal 5 (rust-syntax-class-before-point))
                            (backward-sexp)
                            (rust-rewind-to-decl-name))

                           ((looking-back "[:,'+=]" (1- (point)))
                            (backward-char)
                            (rust-rewind-to-decl-name))

                           ((rust-looking-back-str "->")
                            (backward-char 2)
                            (rust-rewind-to-decl-name))

                           ((rust-looking-back-ident)
                            (rust-rewind-qualified-ident)
                            (rust-rewind-to-decl-name))))))
        (when newpos (goto-char newpos))
        newpos))

    (defun rust-is-in-expression-context (token)
      "Return t if what comes right after the point is part of an
      expression (as opposed to starting a type) by looking at what
      comes before.  Takes a symbol that roughly indicates what is
      after the point.

      This function is used as part of `rust-is-lt-char-operator' as
      part of angle bracket matching, and is not intended to be used
      outside of this context."

      (save-excursion
        (let ((postchar (char-after)))
          (rust-rewind-irrelevant)

          ;; A type alias or ascription could have a type param list.  Skip backwards past it.
          (when (member token '(ambiguous-operator open-brace))
            (rust-rewind-type-param-list))

          (cond

           ;; Certain keywords always introduce expressions
           ((rust-looking-back-symbols '("if" "while" "match" "return" "box" "in")) t)

           ;; "as" introduces a type
           ((rust-looking-back-symbols '("as")) nil)

           ;; An open angle bracket never introduces expression context WITHIN the angle brackets
           ((and (equal token 'open-brace) (equal postchar ?<)) nil)

           ;; An ident! followed by an open brace is a macro invocation.  Consider
           ;; it to be an expression.
           ((and (equal token 'open-brace) (rust-looking-back-macro)) t)

           ;; In a brace context a "]" introduces an expression.
           ((and (eq token 'open-brace) (rust-looking-back-str "]")))

           ;; An identifier is right after an ending paren, bracket, angle bracket
           ;; or curly brace.  It's a type if the last sexp was a type.
           ((and (equal token 'ident) (equal 5 (rust-syntax-class-before-point)))
            (backward-sexp)
            (rust-is-in-expression-context 'open-brace))

           ;; If a "for" appears without a ; or { before it, it's part of an
           ;; "impl X for y", so the y is a type.  Otherwise it's
           ;; introducing a loop, so the y is an expression
           ((and (equal token 'ident) (rust-looking-back-symbols '("for")))
            (backward-sexp)
            (rust-rewind-irrelevant)
            (looking-back "[{;]" (1- (point))))

           ((rust-looking-back-ident)
            (rust-rewind-qualified-ident)
            (rust-rewind-irrelevant)
            (cond
             ((equal token 'open-brace)
              ;; We now know we have:
              ;;   ident <maybe type params> [{([]
              ;; where [{([] denotes either a {, ( or [.  This character is bound as postchar.
              (cond
               ;; If postchar is a paren or square bracket, then if the brace is a type if the identifier is one
               ((member postchar '(?\( ?\[ )) (rust-is-in-expression-context 'ident))

               ;; If postchar is a curly brace, the brace can only be a type if
               ;; ident2 is the name of an enum, struct or trait being declared.
               ;; Note that if there is a -> before the ident then the ident would
               ;; be a type but the { is not.
               ((equal ?{ postchar)
                (not (and (rust-rewind-to-decl-name)
                          (progn
                            (rust-rewind-irrelevant)
                            (rust-looking-back-symbols '("enum" "struct" "union" "trait" "type"))))))
               ))

             ((equal token 'ambiguous-operator)
              (cond
               ;; An ampersand after an ident has to be an operator rather than a & at the beginning of a ref type
               ((equal postchar ?&) t)

               ;; A : followed by a type then an = introduces an expression (unless it is part of a where clause of a "type" declaration)
               ((and (equal postchar ?=)
                     (looking-back "[^:]:" (- (point) 2))
                     (not (save-excursion (and (rust-rewind-to-decl-name) (progn (rust-rewind-irrelevant) (rust-looking-back-symbols '("type"))))))))

               ;; "let ident =" introduces an expression--and so does "const" and "mut"
               ((and (equal postchar ?=) (rust-looking-back-symbols '("let" "const" "mut"))) t)

               ;; As a specific special case, see if this is the = in this situation:
               ;;     enum EnumName<type params> { Ident =
               ;; In this case, this is a c-like enum and despite Ident
               ;; representing a type, what comes after the = is an expression
               ((and
                 (> (rust-paren-level) 0)
                 (save-excursion
                   (backward-up-list)
                   (rust-rewind-irrelevant)
                   (rust-rewind-type-param-list)
                   (and
                    (rust-looking-back-ident)
                    (progn
                      (rust-rewind-qualified-ident)
                      (rust-rewind-irrelevant)
                      (rust-looking-back-str "enum")))))
                t)

               ;; Otherwise the ambiguous operator is a type if the identifier is a type
               ((rust-is-in-expression-context 'ident) t)))

             ((equal token 'colon)
              (cond
               ;; If we see a ident: not inside any braces/parens, we're at top level.
               ;; There are no allowed expressions after colons there, just types.
               ((<= (rust-paren-level) 0) nil)

               ;; We see ident: inside a list
               ((looking-back "[{,]" (1- (point)))
                (backward-up-list)

                ;; If a : appears whose surrounding paren/brackets/braces are
                ;; anything other than curly braces, it can't be a field
                ;; initializer and must be denoting a type.
                (when (looking-at "{")
                  (rust-rewind-irrelevant)
                  (rust-rewind-type-param-list)
                  (when (rust-looking-back-ident)
                    ;; We have a context that looks like this:
                    ;;    ident2 <maybe type params> { [maybe paren-balanced code ending in comma] ident1:
                    ;; the point is sitting just after ident2, and we trying to
                    ;; figure out if the colon introduces an expression or a type.
                    ;; The answer is that ident1 is a field name, and what comes
                    ;; after the colon is an expression, if ident2 is an
                    ;; expression.
                    (rust-rewind-qualified-ident)
                    (rust-is-in-expression-context 'ident))))


               ;; Otherwise, if the ident: appeared with anything other than , or {
               ;; before it, it can't be part of a struct initializer and therefore
               ;; must be denoting a type.
           (t nil)
               ))
             ))

           ;; An operator-like character after a string is indeed an operator
           ((and (equal token 'ambiguous-operator)
                 (member (rust-syntax-class-before-point) '(5 7 15))) t)

           ;; A colon that has something other than an identifier before it is a
           ;; type ascription
           ((equal token 'colon) nil)

           ;; A :: introduces a type (or module, but not an expression in any case)
           ((rust-looking-back-str "::") nil)

           ((rust-looking-back-str ":")
            (backward-char)
            (rust-is-in-expression-context 'colon))

           ;; A -> introduces a type
           ((rust-looking-back-str "->") nil)

           ;; If we are up against the beginning of a list, or after a comma inside
           ;; of one, back up out of it and check what the list itself is
           ((or
             (equal 4 (rust-syntax-class-before-point))
             (rust-looking-back-str ","))
        (condition-case nil
            (progn
              (backward-up-list)
              (rust-is-in-expression-context 'open-brace))
          (scan-error nil)))

           ;; A => introduces an expression
           ((rust-looking-back-str "=>") t)

           ;; A == introduces an expression
           ((rust-looking-back-str "==") t)

           ;; These operators can introduce expressions or types
           ((looking-back "[-+=!?&*]" (1- (point)))
            (backward-char)
            (rust-is-in-expression-context 'ambiguous-operator))

           ;; These operators always introduce expressions.  (Note that if this
           ;; regexp finds a < it must not be an angle bracket, or it'd
           ;; have been caught in the syntax-class check above instead of this.)
           ((looking-back rust-re-pre-expression-operators (1- (point))) t)
           ))))

    (defun rust-is-lt-char-operator ()
      "Return t if the < sign just after point is an operator rather
      than an opening angle bracket, otherwise nil."

      (let ((case-fold-search nil))
        (save-excursion
          (rust-rewind-irrelevant)
          ;; We are now just after the character syntactically before the <.
          (cond

           ;; If we are looking back at a < that is not an angle bracket (but not
           ;; two of them) then this is the second < in a bit shift operator
           ((and (rust-looking-back-str "<")
                 (not (equal 4 (rust-syntax-class-before-point)))
                 (not (rust-looking-back-str "<<"))))

           ;; On the other hand, if we are after a closing paren/brace/bracket it
           ;; can only be an operator, not an angle bracket.  Likewise, if we are
           ;; after a string it's an operator.  (The string case could actually be
           ;; valid in rust for character literals.)
           ((member (rust-syntax-class-before-point) '(5 7 15)) t)

           ;; If we are looking back at an operator, we know that we are at
           ;; the beginning of an expression, and thus it has to be an angle
           ;; bracket (starting a "<Type as Trait>::" construct.)
           ((looking-back rust-re-pre-expression-operators (1- (point))) nil)

           ;; If we are looking back at a keyword, it's an angle bracket
           ;; unless that keyword is "self", "true" or "false"
           ((rust-looking-back-symbols rust-mode-keywords)
            (rust-looking-back-symbols '("self" "true" "false")))

           ((rust-looking-back-str "?")
        (rust-is-in-expression-context 'ambiguous-operator))

           ;; If we're looking back at an identifier, this depends on whether
           ;; the identifier is part of an expression or a type
           ((rust-looking-back-ident)
            (backward-sexp)
            (or
             ;; The special types can't take type param lists, so a < after one is
             ;; always an operator
             (looking-at rust-re-special-types)

             (rust-is-in-expression-context 'ident)))

           ;; Otherwise, assume it's an angle bracket
           ))))

    (defun rust-electric-pair-inhibit-predicate-wrap (char)
      "Wraps the default `electric-pair-inhibit-predicate' to prevent
      inserting a \"matching\" > after a < that would be treated as a
      less than sign rather than as an opening angle bracket."
      (or
       (when (= ?< char)
         (save-excursion
           (backward-char)
           (rust-is-lt-char-operator)))
       (funcall (default-value 'electric-pair-inhibit-predicate) char)))

    (defun rust-ordinary-lt-gt-p ()
      "Test whether the `<' or `>' at point is an ordinary operator of some kind.

    This returns t if the `<' or `>' is an ordinary operator (like
    less-than) or part of one (like `->'); and nil if the character
    should be considered a paired angle bracket."
      (cond
       ;; If matching is turned off suppress all of them
       ((not rust-match-angle-brackets) t)

       ;; We don't take < or > in strings or comments to be angle brackets
       ((rust-in-str-or-cmnt) t)

       ;; Inside a macro we don't really know the syntax.  Any < or > may be an
       ;; angle bracket or it may not.  But we know that the other braces have
       ;; to balance regardless of the < and >, so if we don't treat any < or >
       ;; as angle brackets it won't mess up any paren balancing.
       ((rust-in-macro) t)

       ((looking-at "<")
        (rust-is-lt-char-operator))

       ((looking-at ">")
        (cond
         ;; Don't treat the > in -> or => as an angle bracket
         ((member (char-before (point)) '(?- ?=)) t)

         ;; If we are at top level and not in any list, it can't be a closing
         ;; angle bracket
         ((>= 0 (rust-paren-level)) t)

         ;; Otherwise, treat the > as a closing angle bracket if it would
         ;; match an opening one
         ((save-excursion
        (backward-up-list)
        (not (looking-at "<"))))))))

    (defun rust-mode-syntactic-face-function (state)
      "Syntactic face function to distinguish doc comments from other comments."
      (if (nth 3 state) 'font-lock-string-face
        (save-excursion
          (goto-char (nth 8 state))
          (if (looking-at "/\\([*][*!][^*!]\\|/[/!][^/!]\\)")
              'font-lock-doc-face
            'font-lock-comment-face
        ))))

    (eval-and-compile
      (defconst rust--char-literal-rx
        (rx (seq
         (group "'")
         (or
          (seq
           "\\"
           (or
            (: "u{" (** 1 6 xdigit) "}")
            (: "x" (= 2 xdigit))
            (any "'nrt0\"\\")))
          (not (any "'\\"))
          )
         (group "'")))
        "A regular expression matching a character literal."))

    (defun rust--syntax-propertize-raw-string (end)
      "A helper for rust-syntax-propertize.

    If point is already in a raw string, this will apply the
    appropriate string syntax to the character up to the end of the
    raw string, or to `end', whichever comes first."
      (let ((str-start (nth 8 (syntax-ppss))))
        (when str-start
          (when (save-excursion
              (goto-char str-start)
              (looking-at "r\\(#*\\)\\(\"\\)"))
        ;; In a raw string, so try to find the end.
        (let ((hashes (match-string 1)))
          ;; Match \ characters at the end of the string to suppress
          ;; their normal character-quote syntax.
          (when (re-search-forward (concat "\\(\\\\*\\)\\(\"" hashes "\\)") end t)
            (put-text-property (match-beginning 1) (match-end 1)
                       'syntax-table (string-to-syntax "_"))
            (put-text-property (1- (match-end 2)) (match-end 2)
                       'syntax-table (string-to-syntax "|"))
            (goto-char (match-end 0))))))))

    (defun rust-syntax-propertize (start end)
      "A `syntax-propertize-function' for `rust-mode'."
      (goto-char start)
      (rust--syntax-propertize-raw-string end)
      (funcall
       (syntax-propertize-rules
        ;; Character literals.
        (rust--char-literal-rx (1 "\"") (2 "\""))
        ;; Raw strings.
        ("\\(r\\)#*\""
         (1 (prog1 "|"
          (goto-char (match-end 0))
          (rust--syntax-propertize-raw-string end))))
        ("[<>]"
         (0 (ignore
         (when (save-match-data
             (save-excursion
               (goto-char (match-beginning 0))
               (rust-ordinary-lt-gt-p)))
           (put-text-property (match-beginning 0) (match-end 0)
                      'syntax-table (string-to-syntax "."))
           (goto-char (match-end 0)))))))
       (point) end))

    (defun rust-fill-prefix-for-comment-start (line-start)
      "Determine what to use for `fill-prefix' based on what is at the beginning of a line."
      (let ((result
             ;; Replace /* with same number of spaces
             (replace-regexp-in-string
              "\\(?:/\\*+?\\)[!*]?"
              (lambda (s)
                ;; We want the * to line up with the first * of the
                ;; comment start
                (let ((offset (if (eq t
                                      (compare-strings "/*" nil nil
                                                       s
                                                       (- (length s) 2)
                                                       (length s)))
                                  1 2)))
                  (concat (make-string (- (length s) offset)
                                       ?\x20) "*")))
              line-start)))
        ;; Make sure we've got at least one space at the end
        (if (not (= (aref result (- (length result) 1)) ?\x20))
            (setq result (concat result " ")))
        result))

    (defun rust-in-comment-paragraph (body)
      ;; We might move the point to fill the next comment, but we don't want it
      ;; seeming to jump around on the user
      (save-excursion
        ;; If we're outside of a comment, with only whitespace and then a comment
        ;; in front, jump to the comment and prepare to fill it.
        (when (not (nth 4 (syntax-ppss)))
          (beginning-of-line)
          (when (looking-at (concat "[[:space:]\n]*" comment-start-skip))
            (goto-char (match-end 0))))

        ;; We need this when we're moving the point around and then checking syntax
        ;; while doing paragraph fills, because the cache it uses isn't always
        ;; invalidated during this.
        (syntax-ppss-flush-cache 1)
        ;; If we're at the beginning of a comment paragraph with nothing but
        ;; whitespace til the next line, jump to the next line so that we use the
        ;; existing prefix to figure out what the new prefix should be, rather than
        ;; inferring it from the comment start.
        (let ((next-bol (line-beginning-position 2)))
          (while (save-excursion
                   (end-of-line)
                   (syntax-ppss-flush-cache 1)
                   (and (nth 4 (syntax-ppss))
                        (save-excursion
                          (beginning-of-line)
                          (looking-at paragraph-start))
                        (looking-at "[[:space:]]*$")
                        (nth 4 (syntax-ppss next-bol))))
            (goto-char next-bol)))

        (syntax-ppss-flush-cache 1)
        ;; If we're on the last line of a multiline-style comment that started
        ;; above, back up one line so we don't mistake the * of the */ that ends
        ;; the comment for a prefix.
        (when (save-excursion
                (and (nth 4 (syntax-ppss (line-beginning-position 1)))
                     (looking-at "[[:space:]]*\\*/")))
          (goto-char (line-end-position 0)))
        (funcall body)))

    (defun rust-with-comment-fill-prefix (body)
      (let*
          ((line-string (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))
           (line-comment-start
            (when (nth 4 (syntax-ppss))
              (cond
               ;; If we're inside the comment and see a * prefix, use it
               ((string-match "^\\([[:space:]]*\\*+[[:space:]]*\\)"
                              line-string)
                (match-string 1 line-string))
               ;; If we're at the start of a comment, figure out what prefix
               ;; to use for the subsequent lines after it
               ((string-match (concat "[[:space:]]*" comment-start-skip) line-string)
                (rust-fill-prefix-for-comment-start
                 (match-string 0 line-string))))))
           (fill-prefix
            (or line-comment-start
                fill-prefix)))
        (funcall body)))

    (defun rust-find-fill-prefix ()
      (rust-in-comment-paragraph (lambda () (rust-with-comment-fill-prefix (lambda () fill-prefix)))))

    (defun rust-fill-paragraph (&rest args)
      "Special wrapping for `fill-paragraph' to handle multi-line comments with a * prefix on each line."
      (rust-in-comment-paragraph
       (lambda ()
         (rust-with-comment-fill-prefix
          (lambda ()
            (let
                ((fill-paragraph-function
                  (if (not (eq fill-paragraph-function 'rust-fill-paragraph))
                      fill-paragraph-function))
                 (fill-paragraph-handle-comment t))
              (apply 'fill-paragraph args)
              t))))))

    (defun rust-do-auto-fill (&rest args)
      "Special wrapping for `do-auto-fill' to handle multi-line comments with a * prefix on each line."
      (rust-with-comment-fill-prefix
       (lambda ()
         (apply 'do-auto-fill args)
         t)))

    (defun rust-fill-forward-paragraph (arg)
      ;; This is to work around some funny behavior when a paragraph separator is
      ;; at the very top of the file and there is a fill prefix.
      (let ((fill-prefix nil)) (forward-paragraph arg)))

    (defun rust-comment-indent-new-line (&optional arg)
      (rust-with-comment-fill-prefix
       (lambda () (comment-indent-new-line arg))))

    ;;; Imenu support
    (defvar rust-imenu-generic-expression
      (append (mapcar #'(lambda (x)
                          (list (capitalize x) (rust-re-item-def-imenu x) 1))
                      '("enum" "struct" "union" "type" "mod" "fn" "trait" "impl"))
              `(("Macro" ,(rust-re-item-def-imenu "macro_rules!") 1)))
      "Value for `imenu-generic-expression' in Rust mode.

    Create a hierarchical index of the item definitions in a Rust file.

    Imenu will show all the enums, structs, etc. in their own subheading.
    Use idomenu (imenu with `ido-mode') for best mileage.")

    ;;; Defun Motions

    (defun rust-beginning-of-defun (&optional arg)
      "Move backward to the beginning of the current defun.

    With ARG, move backward multiple defuns.  Negative ARG means
    move forward.

    This is written mainly to be used as `beginning-of-defun-function' for Rust.
    Don't move to the beginning of the line. `beginning-of-defun',
    which calls this, does that afterwards."
      (interactive "p")
      (let* ((arg (or arg 1))
         (magnitude (abs arg))
         (sign (if (< arg 0) -1 1)))
        ;; If moving forward, don't find the defun we might currently be
        ;; on.
        (when (< sign 0)
          (end-of-line))
        (catch 'done
          (dotimes (_ magnitude)
        ;; Search until we find a match that is not in a string or comment.
        (while (if (re-search-backward (concat "^\\(" rust-top-item-beg-re "\\)")
                           nil 'move sign)
               (rust-in-str-or-cmnt)
             ;; Did not find it.
             (throw 'done nil)))))
        t))

    (defun rust-end-of-defun ()
      "Move forward to the next end of defun.

    With argument, do it that many times.
    Negative argument -N means move back to Nth preceding end of defun.

    Assume that this is called after beginning-of-defun. So point is
    at the beginning of the defun body.

    This is written mainly to be used as `end-of-defun-function' for Rust."
      (interactive)
      ;; Find the opening brace
      (if (re-search-forward "[{]" nil t)
          (progn
            (goto-char (match-beginning 0))
            ;; Go to the closing brace
            (condition-case nil
                (forward-sexp)
              (scan-error
               ;; The parentheses are unbalanced; instead of being unable to fontify, just jump to the end of the buffer
               (goto-char (point-max)))))
        ;; There is no opening brace, so consider the whole buffer to be one "defun"
        (goto-char (point-max))))

    (defun rust-end-of-string ()
      "Skip to the end of the current string."
      (save-excursion
        (skip-syntax-forward "^\"|")
        (skip-syntax-forward "\"|")
        (point)))

    ;; Formatting using rustfmt
    (defun rust--format-call (buf)
      "Format BUF using rustfmt."
      (with-current-buffer (get-buffer-create "*rustfmt*")
        (erase-buffer)
        (insert-buffer-substring buf)
        (let* ((tmpf (make-temp-file "rustfmt"))
               (ret (call-process-region (point-min) (point-max) rust-rustfmt-bin
                                         t `(t ,tmpf) nil)))
          (unwind-protect
              (cond
               ((zerop ret)
                (if (not (string= (buffer-string)
                                  (with-current-buffer buf (buffer-string))))
                    (copy-to-buffer buf (point-min) (point-max)))
                (kill-buffer))
               ((= ret 3)
                (if (not (string= (buffer-string)
                                  (with-current-buffer buf (buffer-string))))
                    (copy-to-buffer buf (point-min) (point-max)))
                (erase-buffer)
                (insert-file-contents tmpf)
                (error "Rustfmt could not format some lines, see *rustfmt* buffer for details"))
               (t
                (error "Rustfmt failed, see *rustfmt* buffer for details"))))
          (delete-file tmpf))))

    (defconst rust--format-word "\\b\\(else\\|enum\\|fn\\|for\\|if\\|let\\|loop\\|match\\|struct\\|union\\|unsafe\\|while\\)\\b")
    (defconst rust--format-line "\\([\n]\\)")

    ;; Counts number of matches of regex beginning up to max-beginning,
    ;; leaving the point at the beginning of the last match.
    (defun rust--format-count (regex max-beginning)
      (let ((count 0)
            save-point
            beginning)
        (while (and (< (point) max-beginning)
                    (re-search-forward regex max-beginning t))
          (setq count (1+ count))
          (setq beginning (match-beginning 1)))
        ;; try one more in case max-beginning lies in the middle of a match
        (setq save-point (point))
        (when (re-search-forward regex nil t)
          (let ((try-beginning (match-beginning 1)))
            (if (> try-beginning max-beginning)
                (goto-char save-point)
              (setq count (1+ count))
              (setq beginning try-beginning))))
        (when beginning (goto-char beginning))
        count))

    ;; Gets list describing pos or (point).
    ;; The list contains:
    ;; 1. the number of matches of rust--format-word,
    ;; 2. the number of matches of rust--format-line after that,
    ;; 3. the number of columns after that.
    (defun rust--format-get-loc (buffer &optional pos)
      (with-current-buffer buffer
        (save-excursion
          (let ((pos (or pos (point)))
                words lines columns)
            (goto-char (point-min))
            (setq words (rust--format-count rust--format-word pos))
            (setq lines (rust--format-count rust--format-line pos))
            (if (> lines 0)
                (if (= (point) pos)
                    (setq columns -1)
                  (forward-char 1)
                  (goto-char pos)
                  (setq columns (current-column)))
              (let ((initial-column (current-column)))
                (goto-char pos)
                (setq columns (- (current-column) initial-column))))
            (list words lines columns)))))

    ;; Moves the point forward by count matches of regex up to max-pos,
    ;; and returns new max-pos making sure final position does not include another match.
    (defun rust--format-forward (regex count max-pos)
      (when (< (point) max-pos)
        (let ((beginning (point)))
          (while (> count 0)
            (setq count (1- count))
            (re-search-forward regex nil t)
            (setq beginning (match-beginning 1)))
          (when (re-search-forward regex nil t)
            (setq max-pos (min max-pos (match-beginning 1))))
          (goto-char beginning)))
      max-pos)

    ;; Gets the position from a location list obtained using rust--format-get-loc.
    (defun rust--format-get-pos (buffer loc)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (let ((max-pos (point-max))
                (words (pop loc))
                (lines (pop loc))
                (columns (pop loc)))
            (setq max-pos (rust--format-forward rust--format-word words max-pos))
            (setq max-pos (rust--format-forward rust--format-line lines max-pos))
            (when (> lines 0) (forward-char))
            (let ((initial-column (current-column))
                  (save-point (point)))
              (move-end-of-line nil)
              (when (> (current-column) (+ initial-column columns))
                (goto-char save-point)
                (forward-char columns)))
            (min (point) max-pos)))))

    (defun rust-format-buffer ()
      "Format the current buffer using rustfmt."
      (interactive)
      (unless (executable-find rust-rustfmt-bin)
        (error "Could not locate executable \"%s\"" rust-rustfmt-bin))

      (let* ((current (current-buffer))
             (base (or (buffer-base-buffer current) current))
             buffer-loc
             window-loc)
        (dolist (buffer (buffer-list))
          (when (or (eq buffer base)
                    (eq (buffer-base-buffer buffer) base))
            (push (list buffer
                        (rust--format-get-loc buffer nil))
                  buffer-loc)))
        (dolist (window (window-list))
          (let ((buffer (window-buffer window)))
            (when (or (eq buffer base)
                      (eq (buffer-base-buffer buffer) base))
              (let ((start (window-start window))
                    (point (window-point window)))
                (push (list window
                            (rust--format-get-loc buffer start)
                            (rust--format-get-loc buffer point))
                      window-loc)))))
        (unwind-protect
            (rust--format-call (current-buffer))
          (dolist (loc buffer-loc)
            (let* ((buffer (pop loc))
                   (pos (rust--format-get-pos buffer (pop loc))))
              (with-current-buffer buffer
                (goto-char pos))))
          (dolist (loc window-loc)
            (let* ((window (pop loc))
                   (buffer (window-buffer window))
                   (start (rust--format-get-pos buffer (pop loc)))
                   (pos (rust--format-get-pos buffer (pop loc))))
              (set-window-start window start)
              (set-window-point window pos)))))

      (message "Formatted buffer with rustfmt."))

    (defun rust-enable-format-on-save ()
      "Enable formatting using rustfmt when saving buffer."
      (interactive)
      (setq-local rust-format-on-save t))

    (defun rust-disable-format-on-save ()
      "Disable formatting using rustfmt when saving buffer."
      (interactive)
      (setq-local rust-format-on-save nil))

    ;(defvar rust-mode-map
    ;  (let ((map (make-sparse-keymap)))
    ;    (define-key map (kbd "C-c C-f") 'rust-format-buffer)
    ;    map)
    ;  "Keymap for Rust major mode.")

    ;;;###autoload
    (define-derived-mode rust-mode prog-mode "Rust"
      "Major mode for Rust code.

    \\{rust-mode-map}"
      :group 'rust-mode
      :syntax-table rust-mode-syntax-table

      ;; Syntax.
      (setq-local syntax-propertize-function #'rust-syntax-propertize)

      ;; Indentation
      (setq-local indent-line-function 'rust-mode-indent-line)

      ;; Fonts
      (setq-local font-lock-defaults '(rust-mode-font-lock-keywords
                                       nil nil nil nil
                                       (font-lock-syntactic-face-function . rust-mode-syntactic-face-function)
                                       ))

      ;; Misc
      (setq-local comment-start "// ")
      (setq-local comment-end   "")
      (setq-local indent-tabs-mode nil)
      (setq-local open-paren-in-column-0-is-defun-start nil)

      ;; Auto indent on }
      (setq-local
       electric-indent-chars (cons ?} (and (boundp 'electric-indent-chars)
                                           electric-indent-chars)))

      ;; Allow paragraph fills for comments
      (setq-local comment-start-skip "\\(?://[/!]*\\|/\\*[*!]?\\)[[:space:]]*")
      (setq-local paragraph-start
           (concat "[[:space:]]*\\(?:" comment-start-skip "\\|\\*/?[[:space:]]*\\|\\)$"))
      (setq-local paragraph-separate paragraph-start)
      (setq-local normal-auto-fill-function 'rust-do-auto-fill)
      (setq-local fill-paragraph-function 'rust-fill-paragraph)
      (setq-local fill-forward-paragraph-function 'rust-fill-forward-paragraph)
      (setq-local adaptive-fill-function 'rust-find-fill-prefix)
      (setq-local adaptive-fill-first-line-regexp "")
      (setq-local comment-multi-line t)
      (setq-local comment-line-break-function 'rust-comment-indent-new-line)
      (setq-local imenu-generic-expression rust-imenu-generic-expression)
      (setq-local imenu-syntax-alist '((?! . "w"))) ; For macro_rules!
      (setq-local beginning-of-defun-function 'rust-beginning-of-defun)
      (setq-local end-of-defun-function 'rust-end-of-defun)
      (setq-local parse-sexp-lookup-properties t)
      (setq-local electric-pair-inhibit-predicate 'rust-electric-pair-inhibit-predicate-wrap)

      ; (setq-local compile-command "cargo build")

      (add-hook 'before-save-hook 'rust--before-save-hook nil t)

      (setq-local rust-buffer-project nil)

      (when rust-always-locate-project-on-open
        (rust-update-buffer-project)))

    ;;;###autoload
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

    (defun rust-mode-reload ()
      (interactive)
      (unload-feature 'rust-mode)
      (require 'rust-mode)
      (rust-mode))

    (defun rust--before-save-hook ()
      (when rust-format-on-save (rust-format-buffer)))

    ;; Issue #6887: Rather than inheriting the 'gnu compilation error
    ;; regexp (which is broken on a few edge cases), add our own 'rust
    ;; compilation error regexp and use it instead.
    (defvar rustc-compilation-regexps
      (let ((file "\\([^\n]+\\)")
            (start-line "\\([0-9]+\\)")
            (start-col  "\\([0-9]+\\)")
            (end-line   "\\([0-9]+\\)")
            (end-col    "\\([0-9]+\\)")
            (msg-type   "\\(?:[Ee]rror\\|\\([Ww]arning\\)\\|\\([Nn]ote\\|[Hh]elp\\)\\)"))
        (let ((re (concat "^" file ":" start-line ":" start-col
                          ": " end-line ":" end-col
                          " " msg-type ":")))
          (cons re '(1 (2 . 4) (3 . 5) (6 . 7)))))
      "Specifications for matching errors in rustc invocations.
    See `compilation-error-regexp-alist' for help on their format.")

    (defvar rustc-new-compilation-regexps
      (let ((file "\\([^\n]+\\)")
            (start-line "\\([0-9]+\\)")
            (start-col  "\\([0-9]+\\)"))
        (let ((re (concat "^ *--> " file ":" start-line ":" start-col ; --> 1:2:3
                          )))
          (cons re '(1 2 3))))
      "Specifications for matching errors in rustc invocations (new style).
    See `compilation-error-regexp-alist' for help on their format.")

    ;; Match test run failures and panics during compilation as
    ;; compilation warnings
    (defvar cargo-compilation-regexps
      '("^\\s-+thread '[^']+' panicked at \\('[^']+', \\([^:]+\\):\\([0-9]+\\)\\)" 2 3 nil nil 1)
      "Specifications for matching panics in cargo test invocations.
    See `compilation-error-regexp-alist' for help on their format.")

    (defun rustc-scroll-down-after-next-error ()
      "In the new style error messages, the regular expression
       matches on the file name (which appears after `-->`), but the
       start of the error appears a few lines earlier. This hook runs
       after `M-x next-error`; it simply scrolls down a few lines in
       the compilation window until the top of the error is visible."
      (save-selected-window
        (when (eq major-mode 'rust-mode)
          (select-window (get-buffer-window next-error-last-buffer 'visible))
          (when (save-excursion
                  (beginning-of-line)
                  (looking-at " *-->"))
            (let ((start-of-error
                   (save-excursion
                     (beginning-of-line)
                     (while (not (looking-at "^[a-z]+:\\|^[a-z]+\\[E[0-9]+\\]:"))
                       (forward-line -1))
                     (point))))
              (set-window-start (selected-window) start-of-error))))))

    (eval-after-load 'compile
      '(progn
         (add-to-list 'compilation-error-regexp-alist-alist
                      (cons 'rustc-new rustc-new-compilation-regexps))
         (add-to-list 'compilation-error-regexp-alist 'rustc-new)
         (add-hook 'next-error-hook 'rustc-scroll-down-after-next-error)
         (add-to-list 'compilation-error-regexp-alist-alist
                      (cons 'rustc rustc-compilation-regexps))
         (add-to-list 'compilation-error-regexp-alist 'rustc)
         (add-to-list 'compilation-error-regexp-alist-alist
                      (cons 'cargo cargo-compilation-regexps))
         (add-to-list 'compilation-error-regexp-alist 'cargo)))

    ;;; Functions to submit (parts of) buffers to the rust playpen, for
    ;;; sharing.
    (defun rust-playpen-region (begin end)
      "Create a sharable URL for the contents of the current region
       on the Rust playpen."
      (interactive "r")
      (let* ((data (buffer-substring begin end))
             (escaped-data (url-hexify-string data))
             (escaped-playpen-url (url-hexify-string (format rust-playpen-url-format escaped-data))))
        (if (> (length escaped-playpen-url) 5000)
            (error "encoded playpen data exceeds 5000 character limit (length %s)"
                   (length escaped-playpen-url))
          (let ((shortener-url (format rust-shortener-url-format escaped-playpen-url))
                (url-request-method "POST"))
            (url-retrieve shortener-url
                          (lambda (state)
                            ; filter out the headers etc. included at the
                            ; start of the buffer: the relevant text
                            ; (shortened url or error message) is exactly
                            ; the last line.
                            (goto-char (point-max))
                            (let ((last-line (thing-at-point 'line t))
                                  (err (plist-get state :error)))
                              (kill-buffer)
                              (if err
                                  (error "failed to shorten playpen url: %s" last-line)
                                (message "%s" last-line)))))))))

    (defun rust-playpen-buffer ()
      "Create a sharable URL for the contents of the current buffer
       on the Rust playpen."
      (interactive)
      (rust-playpen-region (point-min) (point-max)))

    (defun rust-promote-module-into-dir ()
      "Promote the module file visited by the current buffer into its own directory.

    For example, if the current buffer is visiting the file `foo.rs',
    then this function creates the directory `foo' and renames the
    file to `foo/mod.rs'.  The current buffer will be updated to
    visit the new file."
      (interactive)
      (let ((filename (buffer-file-name)))
        (if (not filename)
            (message "Buffer is not visiting a file.")
          (if (string-equal (file-name-nondirectory filename) "mod.rs")
              (message "Won't promote a module file already named mod.rs.")
            (let* ((basename (file-name-sans-extension
                              (file-name-nondirectory filename)))
                   (mod-dir (file-name-as-directory
                             (concat (file-name-directory filename) basename)))
                   (new-name (concat mod-dir "mod.rs")))
              (mkdir mod-dir t)
              (rename-file filename new-name 1)
              (set-visited-file-name new-name))))))

    (defun rust-run-clippy ()
      "Run `cargo clippy'."
      (interactive)
      (when (null rust-buffer-project)
        (rust-update-buffer-project))
      (let* ((args (list rust-cargo-bin "clippy" (concat "--manifest-path=" rust-buffer-project)))
             ;; set `compile-command' temporarily so `compile' doesn't
             ;; clobber the existing value
             (compile-command (mapconcat #'shell-quote-argument args " ")))
        (compile compile-command)))

    (defun rust-update-buffer-project ()
      (setq-local rust-buffer-project (rust-buffer-project)))

    (defun rust-buffer-project ()
      "Get project root if possible."
      (with-temp-buffer
        (let ((ret (call-process rust-cargo-bin nil t nil "locate-project")))
          (when (/= ret 0)
            (error "`cargo locate-project' returned %s status: %s" ret (buffer-string)))
          (goto-char 0)
          (let ((output (json-read)))
            (cdr (assoc-string "root" output))))))

    (provide 'rust-mode)

    ;;; rust-mode.el ends here

; ------------------------------------ DASH SETTINGS --------------------------------------- ;

    ;;; dash.el --- A modern list library for Emacs  -*- lexical-binding: t -*-

    ;; Copyright (C) 2012-2016 Free Software Foundation, Inc.

    ;; Author: Magnar Sveen <magnars@gmail.com>
    ;; Version: 2.13.0
    ;; Keywords: lists

    ;; This program is free software; you can redistribute it and/or modify
    ;; it under the terms of the GNU General Public License as published by
    ;; the Free Software Foundation, either version 3 of the License, or
    ;; (at your option) any later version.

    ;; This program is distributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ;; GNU General Public License for more details.

    ;; You should have received a copy of the GNU General Public License
    ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

    ;;; Commentary:

    ;; A modern list api for Emacs.
    ;;
    ;; See documentation on https://github.com/magnars/dash.el#functions
    ;;
    ;; **Please note** The lexical binding in this file is not utilised at the
    ;; moment. We will take full advantage of lexical binding in an upcoming 3.0
    ;; release of Dash. In the meantime, we've added the pragma to avoid a bug that
    ;; you can read more about in https://github.com/magnars/dash.el/issues/130.
    ;;

    ;;; Code:

    (defgroup dash ()
      "Customize group for dash.el"
      :group 'lisp
      :prefix "dash-")

    (defun dash--enable-fontlock (symbol value)
      (when value
        (dash-enable-font-lock))
      (set-default symbol value))

    (defcustom dash-enable-fontlock nil
      "If non-nil, enable fontification of dash functions, macros and
    special values."
      :type 'boolean
      :set 'dash--enable-fontlock
      :group 'dash)

    (defmacro !cons (car cdr)
      "Destructive: Set CDR to the cons of CAR and CDR."
      `(setq ,cdr (cons ,car ,cdr)))

    (defmacro !cdr (list)
      "Destructive: Set LIST to the cdr of LIST."
      `(setq ,list (cdr ,list)))

    (defmacro --each (list &rest body)
      "Anaphoric form of `-each'."
      (declare (debug (form body))
               (indent 1))
      (let ((l (make-symbol "list")))
        `(let ((,l ,list)
               (it-index 0))
           (while ,l
             (let ((it (car ,l)))
               ,@body)
             (setq it-index (1+ it-index))
             (!cdr ,l)))))

    (defmacro -doto (eval-initial-value &rest forms)
      "Eval a form, then insert that form as the 2nd argument to other forms.
    The EVAL-INITIAL-VALUE form is evaluated once. Its result is
    passed to FORMS, which are then evaluated sequentially. Returns
    the target form."
      (declare (indent 1))
      (let ((retval (make-symbol "value")))
        `(let ((,retval ,eval-initial-value))
           ,@(mapcar (lambda (form)
                       (if (sequencep form)
                           `(,(-first-item form) ,retval ,@(cdr form))
                         `(funcall form ,retval)))
                     forms)
           ,retval)))

    (defun -each (list fn)
      "Call FN with every item in LIST. Return nil, used for side-effects only."
      (--each list (funcall fn it)))

    (put '-each 'lisp-indent-function 1)

    (defalias '--each-indexed '--each)

    (defun -each-indexed (list fn)
      "Call (FN index item) for each item in LIST.

    In the anaphoric form `--each-indexed', the index is exposed as symbol `it-index'.

    See also: `-map-indexed'."
      (--each list (funcall fn it-index it)))
    (put '-each-indexed 'lisp-indent-function 1)

    (defmacro --each-while (list pred &rest body)
      "Anaphoric form of `-each-while'."
      (declare (debug (form form body))
               (indent 2))
      (let ((l (make-symbol "list"))
            (c (make-symbol "continue")))
        `(let ((,l ,list)
               (,c t)
               (it-index 0))
           (while (and ,l ,c)
             (let ((it (car ,l)))
               (if (not ,pred) (setq ,c nil) ,@body))
             (setq it-index (1+ it-index))
             (!cdr ,l)))))

    (defun -each-while (list pred fn)
      "Call FN with every item in LIST while (PRED item) is non-nil.
    Return nil, used for side-effects only."
      (--each-while list (funcall pred it) (funcall fn it)))

    (put '-each-while 'lisp-indent-function 2)

    (defmacro --dotimes (num &rest body)
      "Repeatedly executes BODY (presumably for side-effects) with symbol `it' bound to integers from 0 through NUM-1."
      (declare (debug (form body))
               (indent 1))
      (let ((n (make-symbol "num")))
        `(let ((,n ,num)
               (it 0))
           (while (< it ,n)
             ,@body
             (setq it (1+ it))))))

    (defun -dotimes (num fn)
      "Repeatedly calls FN (presumably for side-effects) passing in integers from 0 through NUM-1."
      (--dotimes num (funcall fn it)))

    (put '-dotimes 'lisp-indent-function 1)

    (defun -map (fn list)
      "Return a new list consisting of the result of applying FN to the items in LIST."
      (mapcar fn list))

    (defmacro --map (form list)
      "Anaphoric form of `-map'."
      (declare (debug (form form)))
      `(mapcar (lambda (it) ,form) ,list))

    (defmacro --reduce-from (form initial-value list)
      "Anaphoric form of `-reduce-from'."
      (declare (debug (form form form)))
      `(let ((acc ,initial-value))
         (--each ,list (setq acc ,form))
         acc))

    (defun -reduce-from (fn initial-value list)
      "Return the result of applying FN to INITIAL-VALUE and the
    first item in LIST, then applying FN to that result and the 2nd
    item, etc. If LIST contains no items, return INITIAL-VALUE and
    FN is not called.

    In the anaphoric form `--reduce-from', the accumulated value is
    exposed as symbol `acc'.

    See also: `-reduce', `-reduce-r'"
      (--reduce-from (funcall fn acc it) initial-value list))

    (defmacro --reduce (form list)
      "Anaphoric form of `-reduce'."
      (declare (debug (form form)))
      (let ((lv (make-symbol "list-value")))
        `(let ((,lv ,list))
           (if ,lv
               (--reduce-from ,form (car ,lv) (cdr ,lv))
             (let (acc it) ,form)))))

    (defun -reduce (fn list)
      "Return the result of applying FN to the first 2 items in LIST,
    then applying FN to that result and the 3rd item, etc. If LIST
    contains no items, FN must accept no arguments as well, and
    reduce return the result of calling FN with no arguments. If
    LIST has only 1 item, it is returned and FN is not called.

    In the anaphoric form `--reduce', the accumulated value is
    exposed as symbol `acc'.

    See also: `-reduce-from', `-reduce-r'"
      (if list
          (-reduce-from fn (car list) (cdr list))
        (funcall fn)))

    (defun -reduce-r-from (fn initial-value list)
      "Replace conses with FN, nil with INITIAL-VALUE and evaluate
    the resulting expression. If LIST is empty, INITIAL-VALUE is
    returned and FN is not called.

    Note: this function works the same as `-reduce-from' but the
    operation associates from right instead of from left.

    See also: `-reduce-r', `-reduce'"
      (if (not list) initial-value
        (funcall fn (car list) (-reduce-r-from fn initial-value (cdr list)))))

    (defmacro --reduce-r-from (form initial-value list)
      "Anaphoric version of `-reduce-r-from'."
      (declare (debug (form form form)))
      `(-reduce-r-from (lambda (&optional it acc) ,form) ,initial-value ,list))

    (defun -reduce-r (fn list)
      "Replace conses with FN and evaluate the resulting expression.
    The final nil is ignored. If LIST contains no items, FN must
    accept no arguments as well, and reduce return the result of
    calling FN with no arguments. If LIST has only 1 item, it is
    returned and FN is not called.

    The first argument of FN is the new item, the second is the
    accumulated value.

    Note: this function works the same as `-reduce' but the operation
    associates from right instead of from left.

    See also: `-reduce-r-from', `-reduce'"
      (cond
       ((not list) (funcall fn))
       ((not (cdr list)) (car list))
       (t (funcall fn (car list) (-reduce-r fn (cdr list))))))

    (defmacro --reduce-r (form list)
      "Anaphoric version of `-reduce-r'."
      (declare (debug (form form)))
      `(-reduce-r (lambda (&optional it acc) ,form) ,list))

    (defmacro --filter (form list)
      "Anaphoric form of `-filter'."
      (declare (debug (form form)))
      (let ((r (make-symbol "result")))
        `(let (,r)
           (--each ,list (when ,form (!cons it ,r)))
           (nreverse ,r))))

    (defun -filter (pred list)
      "Return a new list of the items in LIST for which PRED returns a non-nil value.

    Alias: `-select'

    See also: `-keep'"
      (--filter (funcall pred it) list))

    (defalias '-select '-filter)
    (defalias '--select '--filter)

    (defmacro --remove (form list)
      "Anaphoric form of `-remove'."
      (declare (debug (form form)))
      `(--filter (not ,form) ,list))

    (defun -remove (pred list)
      "Return a new list of the items in LIST for which PRED returns nil.

    Alias: `-reject'"
      (--remove (funcall pred it) list))

    (defalias '-reject '-remove)
    (defalias '--reject '--remove)

    (defun -remove-first (pred list)
      "Return a new list with the first item matching PRED removed.

    Alias: `-reject-first'

    See also: `-remove', `-map-first'"
      (let (front)
        (while (and list (not (funcall pred (car list))))
          (push (car list) front)
          (!cdr list))
        (if list
            (-concat (nreverse front) (cdr list))
          (nreverse front))))

    (defmacro --remove-first (form list)
      "Anaphoric form of `-remove-first'."
      (declare (debug (form form)))
      `(-remove-first (lambda (it) ,form) ,list))

    (defalias '-reject-first '-remove-first)
    (defalias '--reject-first '--remove-first)

    (defun -remove-last (pred list)
      "Return a new list with the last item matching PRED removed.

    Alias: `-reject-last'

    See also: `-remove', `-map-last'"
      (nreverse (-remove-first pred (reverse list))))

    (defmacro --remove-last (form list)
      "Anaphoric form of `-remove-last'."
      (declare (debug (form form)))
      `(-remove-last (lambda (it) ,form) ,list))

    (defalias '-reject-last '-remove-last)
    (defalias '--reject-last '--remove-last)

    (defun -remove-item (item list)
      "Remove all occurences of ITEM from LIST.

    Comparison is done with `equal'."
      (declare (pure t) (side-effect-free t))
      (--remove (equal it item) list))

    (defmacro --keep (form list)
      "Anaphoric form of `-keep'."
      (declare (debug (form form)))
      (let ((r (make-symbol "result"))
            (m (make-symbol "mapped")))
        `(let (,r)
           (--each ,list (let ((,m ,form)) (when ,m (!cons ,m ,r))))
           (nreverse ,r))))

    (defun -keep (fn list)
      "Return a new list of the non-nil results of applying FN to the items in LIST.

    If you want to select the original items satisfying a predicate use `-filter'."
      (--keep (funcall fn it) list))

    (defun -non-nil (list)
      "Return all non-nil elements of LIST."
      (declare (pure t) (side-effect-free t))
      (-remove 'null list))

    (defmacro --map-indexed (form list)
      "Anaphoric form of `-map-indexed'."
      (declare (debug (form form)))
      (let ((r (make-symbol "result")))
        `(let (,r)
           (--each ,list
             (!cons ,form ,r))
           (nreverse ,r))))

    (defun -map-indexed (fn list)
      "Return a new list consisting of the result of (FN index item) for each item in LIST.

    In the anaphoric form `--map-indexed', the index is exposed as symbol `it-index'.

    See also: `-each-indexed'."
      (--map-indexed (funcall fn it-index it) list))

    (defmacro --map-when (pred rep list)
      "Anaphoric form of `-map-when'."
      (declare (debug (form form form)))
      (let ((r (make-symbol "result")))
        `(let (,r)
           (--each ,list (!cons (if ,pred ,rep it) ,r))
           (nreverse ,r))))

    (defun -map-when (pred rep list)
      "Return a new list where the elements in LIST that do not match the PRED function
    are unchanged, and where the elements in LIST that do match the PRED function are mapped
    through the REP function.

    Alias: `-replace-where'

    See also: `-update-at'"
      (--map-when (funcall pred it) (funcall rep it) list))

    (defalias '-replace-where '-map-when)
    (defalias '--replace-where '--map-when)

    (defun -map-first (pred rep list)
      "Replace first item in LIST satisfying PRED with result of REP called on this item.

    See also: `-map-when', `-replace-first'"
      (let (front)
        (while (and list (not (funcall pred (car list))))
          (push (car list) front)
          (!cdr list))
        (if list
            (-concat (nreverse front) (cons (funcall rep (car list)) (cdr list)))
          (nreverse front))))

    (defmacro --map-first (pred rep list)
      "Anaphoric form of `-map-first'."
      `(-map-first (lambda (it) ,pred) (lambda (it) (ignore it) ,rep) ,list))

    (defun -map-last (pred rep list)
      "Replace last item in LIST satisfying PRED with result of REP called on this item.

    See also: `-map-when', `-replace-last'"
      (nreverse (-map-first pred rep (reverse list))))

    (defmacro --map-last (pred rep list)
      "Anaphoric form of `-map-last'."
      `(-map-last (lambda (it) ,pred) (lambda (it) (ignore it) ,rep) ,list))

    (defun -replace (old new list)
      "Replace all OLD items in LIST with NEW.

    Elements are compared using `equal'.

    See also: `-replace-at'"
      (declare (pure t) (side-effect-free t))
      (--map-when (equal it old) new list))

    (defun -replace-first (old new list)
      "Replace the first occurence of OLD with NEW in LIST.

    Elements are compared using `equal'.

    See also: `-map-first'"
      (declare (pure t) (side-effect-free t))
      (--map-first (equal old it) new list))

    (defun -replace-last (old new list)
      "Replace the last occurence of OLD with NEW in LIST.

    Elements are compared using `equal'.

    See also: `-map-last'"
      (declare (pure t) (side-effect-free t))
      (--map-last (equal old it) new list))

    (defmacro --mapcat (form list)
      "Anaphoric form of `-mapcat'."
      (declare (debug (form form)))
      `(apply 'append (--map ,form ,list)))

    (defun -mapcat (fn list)
      "Return the concatenation of the result of mapping FN over LIST.
    Thus function FN should return a list."
      (--mapcat (funcall fn it) list))

    (defun -flatten (l)
      "Take a nested list L and return its contents as a single, flat list.

    Note that because `nil' represents a list of zero elements (an
    empty list), any mention of nil in L will disappear after
    flattening.  If you need to preserve nils, consider `-flatten-n'
    or map them to some unique symbol and then map them back.

    Conses of two atoms are considered \"terminals\", that is, they
    aren't flattened further.

    See also: `-flatten-n'"
      (declare (pure t) (side-effect-free t))
      (if (and (listp l) (listp (cdr l)))
          (-mapcat '-flatten l)
        (list l)))

    (defmacro --iterate (form init n)
      "Anaphoric version of `-iterate'."
      (declare (debug (form form form)))
      `(-iterate (lambda (it) ,form) ,init ,n))

    (defun -flatten-n (num list)
      "Flatten NUM levels of a nested LIST.

    See also: `-flatten'"
      (declare (pure t) (side-effect-free t))
      (-last-item (--iterate (--mapcat (-list it) it) list (1+ num))))

    (defun -concat (&rest lists)
      "Return a new list with the concatenation of the elements in the supplied LISTS."
      (declare (pure t) (side-effect-free t))
      (apply 'append lists))

    (defalias '-copy 'copy-sequence
      "Create a shallow copy of LIST.

    \(fn LIST)")

    (defun -splice (pred fun list)
      "Splice lists generated by FUN in place of elements matching PRED in LIST.

    FUN takes the element matching PRED as input.

    This function can be used as replacement for `,@' in case you
    need to splice several lists at marked positions (for example
    with keywords).

    See also: `-splice-list', `-insert-at'"
      (let (r)
        (--each list
          (if (funcall pred it)
              (let ((new (funcall fun it)))
                (--each new (!cons it r)))
            (!cons it r)))
        (nreverse r)))

    (defmacro --splice (pred form list)
      "Anaphoric form of `-splice'."
      `(-splice (lambda (it) ,pred) (lambda (it) ,form) ,list))

    (defun -splice-list (pred new-list list)
      "Splice NEW-LIST in place of elements matching PRED in LIST.

    See also: `-splice', `-insert-at'"
      (-splice pred (lambda (_) new-list) list))

    (defmacro --splice-list (pred new-list list)
      "Anaphoric form of `-splice-list'."
      `(-splice-list (lambda (it) ,pred) ,new-list ,list))

    (defun -cons* (&rest args)
      "Make a new list from the elements of ARGS.

    The last 2 members of ARGS are used as the final cons of the
    result so if the final member of ARGS is not a list the result is
    a dotted list."
      (declare (pure t) (side-effect-free t))
      (-reduce-r 'cons args))

    (defun -snoc (list elem &rest elements)
      "Append ELEM to the end of the list.

    This is like `cons', but operates on the end of list.

    If ELEMENTS is non nil, append these to the list as well."
      (-concat list (list elem) elements))

    (defmacro --first (form list)
      "Anaphoric form of `-first'."
      (declare (debug (form form)))
      (let ((n (make-symbol "needle")))
        `(let (,n)
           (--each-while ,list (not ,n)
             (when ,form (setq ,n it)))
           ,n)))

    (defun -first (pred list)
      "Return the first x in LIST where (PRED x) is non-nil, else nil.

    To get the first item in the list no questions asked, use `car'.

    Alias: `-find'"
      (--first (funcall pred it) list))

    (defalias '-find '-first)
    (defalias '--find '--first)

    (defmacro --some (form list)
      "Anaphoric form of `-some'."
      (declare (debug (form form)))
      (let ((n (make-symbol "needle")))
        `(let (,n)
           (--each-while ,list (not ,n)
             (setq ,n ,form))
           ,n)))

    (defun -some (pred list)
      "Return (PRED x) for the first LIST item where (PRED x) is non-nil, else nil.

    Alias: `-any'"
      (--some (funcall pred it) list))

    (defalias '-any '-some)
    (defalias '--any '--some)

    (defmacro --last (form list)
      "Anaphoric form of `-last'."
      (declare (debug (form form)))
      (let ((n (make-symbol "needle")))
        `(let (,n)
           (--each ,list
             (when ,form (setq ,n it)))
           ,n)))

    (defun -last (pred list)
      "Return the last x in LIST where (PRED x) is non-nil, else nil."
      (--last (funcall pred it) list))

    (defalias '-first-item 'car
      "Return the first item of LIST, or nil on an empty list.

    \(fn LIST)")

    ;; Ensure that calls to `-first-item' are compiled to a single opcode,
    ;; just like `car'.
    (put '-first-item 'byte-opcode 'byte-car)
    (put '-first-item 'byte-compile 'byte-compile-one-arg)

    ;; TODO: emacs23 support, when dropped remove the condition
    (eval-when-compile
      (require 'cl)
      (if (fboundp 'gv-define-simple-setter)
          (gv-define-simple-setter -first-item setcar)
        (require 'cl)
        (with-no-warnings
          (defsetf -first-item (x) (val) `(setcar ,x ,val)))))

    (defun -last-item (list)
      "Return the last item of LIST, or nil on an empty list."
      (declare (pure t) (side-effect-free t))
      (car (last list)))

    ;; TODO: emacs23 support, when dropped remove the condition
    (eval-when-compile
      (if (fboundp 'gv-define-setter)
          (gv-define-setter -last-item (val x) `(setcar (last ,x) ,val))
        (with-no-warnings
          (defsetf -last-item (x) (val) `(setcar (last ,x) ,val)))))

    (defun -butlast (list)
      "Return a list of all items in list except for the last."
      ;; no alias as we don't want magic optional argument
      (declare (pure t) (side-effect-free t))
      (butlast list))

    (defmacro --count (pred list)
      "Anaphoric form of `-count'."
      (declare (debug (form form)))
      (let ((r (make-symbol "result")))
        `(let ((,r 0))
           (--each ,list (when ,pred (setq ,r (1+ ,r))))
           ,r)))

    (defun -count (pred list)
      "Counts the number of items in LIST where (PRED item) is non-nil."
      (--count (funcall pred it) list))

    (defun ---truthy? (val)
      (declare (pure t) (side-effect-free t))
      (not (null val)))

    (defmacro --any? (form list)
      "Anaphoric form of `-any?'."
      (declare (debug (form form)))
      `(---truthy? (--first ,form ,list)))

    (defun -any? (pred list)
      "Return t if (PRED x) is non-nil for any x in LIST, else nil.

    Alias: `-any-p', `-some?', `-some-p'"
      (--any? (funcall pred it) list))

    (defalias '-some? '-any?)
    (defalias '--some? '--any?)
    (defalias '-any-p '-any?)
    (defalias '--any-p '--any?)
    (defalias '-some-p '-any?)
    (defalias '--some-p '--any?)

    (defmacro --all? (form list)
      "Anaphoric form of `-all?'."
      (declare (debug (form form)))
      (let ((a (make-symbol "all")))
        `(let ((,a t))
           (--each-while ,list ,a (setq ,a ,form))
           (---truthy? ,a))))

    (defun -all? (pred list)
      "Return t if (PRED x) is non-nil for all x in LIST, else nil.

    Alias: `-all-p', `-every?', `-every-p'"
      (--all? (funcall pred it) list))

    (defalias '-every? '-all?)
    (defalias '--every? '--all?)
    (defalias '-all-p '-all?)
    (defalias '--all-p '--all?)
    (defalias '-every-p '-all?)
    (defalias '--every-p '--all?)

    (defmacro --none? (form list)
      "Anaphoric form of `-none?'."
      (declare (debug (form form)))
      `(--all? (not ,form) ,list))

    (defun -none? (pred list)
      "Return t if (PRED x) is nil for all x in LIST, else nil.

    Alias: `-none-p'"
      (--none? (funcall pred it) list))

    (defalias '-none-p '-none?)
    (defalias '--none-p '--none?)

    (defmacro --only-some? (form list)
      "Anaphoric form of `-only-some?'."
      (declare (debug (form form)))
      (let ((y (make-symbol "yes"))
            (n (make-symbol "no")))
        `(let (,y ,n)
           (--each-while ,list (not (and ,y ,n))
             (if ,form (setq ,y t) (setq ,n t)))
           (---truthy? (and ,y ,n)))))

    (defun -only-some? (pred list)
      "Return `t` if at least one item of LIST matches PRED and at least one item of LIST does not match PRED.
    Return `nil` both if all items match the predicate or if none of the items match the predicate.

    Alias: `-only-some-p'"
      (--only-some? (funcall pred it) list))

    (defalias '-only-some-p '-only-some?)
    (defalias '--only-some-p '--only-some?)

    (defun -slice (list from &optional to step)
      "Return copy of LIST, starting from index FROM to index TO.

    FROM or TO may be negative.  These values are then interpreted
    modulo the length of the list.

    If STEP is a number, only each STEPth item in the resulting
    section is returned.  Defaults to 1."
      (declare (pure t) (side-effect-free t))
      (let ((length (length list))
            (new-list nil))
        ;; to defaults to the end of the list
        (setq to (or to length))
        (setq step (or step 1))
        ;; handle negative indices
        (when (< from 0)
          (setq from (mod from length)))
        (when (< to 0)
          (setq to (mod to length)))

        ;; iterate through the list, keeping the elements we want
        (--each-while list (< it-index to)
          (when (and (>= it-index from)
                     (= (mod (- from it-index) step) 0))
            (push it new-list)))
        (nreverse new-list)))

    (defun -take (n list)
      "Return a new list of the first N items in LIST, or all items if there are fewer than N.

    See also: `-take-last'"
      (declare (pure t) (side-effect-free t))
      (let (result)
        (--dotimes n
          (when list
            (!cons (car list) result)
            (!cdr list)))
        (nreverse result)))

    (defun -take-last (n list)
      "Return the last N items of LIST in order.

    See also: `-take'"
      (declare (pure t) (side-effect-free t))
      (copy-sequence (last list n)))

    (defalias '-drop 'nthcdr
      "Return the tail of LIST without the first N items.

    See also: `-drop-last'

    \(fn N LIST)")

    (defun -drop-last (n list)
      "Remove the last N items of LIST and return a copy.

    See also: `-drop'"
      ;; No alias because we don't want magic optional argument
      (declare (pure t) (side-effect-free t))
      (butlast list n))

    (defmacro --take-while (form list)
      "Anaphoric form of `-take-while'."
      (declare (debug (form form)))
      (let ((r (make-symbol "result")))
        `(let (,r)
           (--each-while ,list ,form (!cons it ,r))
           (nreverse ,r))))

    (defun -take-while (pred list)
      "Return a new list of successive items from LIST while (PRED item) returns a non-nil value."
      (--take-while (funcall pred it) list))

    (defmacro --drop-while (form list)
      "Anaphoric form of `-drop-while'."
      (declare (debug (form form)))
      (let ((l (make-symbol "list")))
        `(let ((,l ,list))
           (while (and ,l (let ((it (car ,l))) ,form))
             (!cdr ,l))
           ,l)))

    (defun -drop-while (pred list)
      "Return the tail of LIST starting from the first item for which (PRED item) returns nil."
      (--drop-while (funcall pred it) list))

    (defun -split-at (n list)
      "Return a list of ((-take N LIST) (-drop N LIST)), in no more than one pass through the list."
      (declare (pure t) (side-effect-free t))
      (let (result)
        (--dotimes n
          (when list
            (!cons (car list) result)
            (!cdr list)))
        (list (nreverse result) list)))

    (defun -rotate (n list)
      "Rotate LIST N places to the right.  With N negative, rotate to the left.
    The time complexity is O(n)."
      (declare (pure t) (side-effect-free t))
      (if (> n 0)
          (append (last list n) (butlast list n))
        (append (-drop (- n) list) (-take (- n) list))))

    (defun -insert-at (n x list)
      "Return a list with X inserted into LIST at position N.

    See also: `-splice', `-splice-list'"
      (declare (pure t) (side-effect-free t))
      (let ((split-list (-split-at n list)))
        (nconc (car split-list) (cons x (cadr split-list)))))

    (defun -replace-at (n x list)
      "Return a list with element at Nth position in LIST replaced with X.

    See also: `-replace'"
      (declare (pure t) (side-effect-free t))
      (let ((split-list (-split-at n list)))
        (nconc (car split-list) (cons x (cdr (cadr split-list))))))

    (defun -update-at (n func list)
      "Return a list with element at Nth position in LIST replaced with `(func (nth n list))`.

    See also: `-map-when'"
      (let ((split-list (-split-at n list)))
        (nconc (car split-list) (cons (funcall func (car (cadr split-list))) (cdr (cadr split-list))))))

    (defmacro --update-at (n form list)
      "Anaphoric version of `-update-at'."
      (declare (debug (form form form)))
      `(-update-at ,n (lambda (it) ,form) ,list))

    (defun -remove-at (n list)
      "Return a list with element at Nth position in LIST removed.

    See also: `-remove-at-indices', `-remove'"
      (declare (pure t) (side-effect-free t))
      (-remove-at-indices (list n) list))

    (defun -remove-at-indices (indices list)
      "Return a list whose elements are elements from LIST without
    elements selected as `(nth i list)` for all i
    from INDICES.

    See also: `-remove-at', `-remove'"
      (declare (pure t) (side-effect-free t))
      (let* ((indices (-sort '< indices))
             (diffs (cons (car indices) (-map '1- (-zip-with '- (cdr indices) indices))))
             r)
        (--each diffs
          (let ((split (-split-at it list)))
            (!cons (car split) r)
            (setq list (cdr (cadr split)))))
        (!cons list r)
        (apply '-concat (nreverse r))))

    (defmacro --split-with (pred list)
      "Anaphoric form of `-split-with'."
      (declare (debug (form form)))
      (let ((l (make-symbol "list"))
            (r (make-symbol "result"))
            (c (make-symbol "continue")))
        `(let ((,l ,list)
               (,r nil)
               (,c t))
           (while (and ,l ,c)
             (let ((it (car ,l)))
               (if (not ,pred)
                   (setq ,c nil)
                 (!cons it ,r)
                 (!cdr ,l))))
           (list (nreverse ,r) ,l))))

    (defun -split-with (pred list)
      "Return a list of ((-take-while PRED LIST) (-drop-while PRED LIST)), in no more than one pass through the list."
      (--split-with (funcall pred it) list))

    (defmacro -split-on (item list)
      "Split the LIST each time ITEM is found.

    Unlike `-partition-by', the ITEM is discarded from the results.
    Empty lists are also removed from the result.

    Comparison is done by `equal'.

    See also `-split-when'"
      (declare (debug (form form)))
      `(-split-when (lambda (it) (equal it ,item)) ,list))

    (defmacro --split-when (form list)
      "Anaphoric version of `-split-when'."
      (declare (debug (form form)))
      `(-split-when (lambda (it) ,form) ,list))

    (defun -split-when (fn list)
      "Split the LIST on each element where FN returns non-nil.

    Unlike `-partition-by', the \"matched\" element is discarded from
    the results.  Empty lists are also removed from the result.

    This function can be thought of as a generalization of
    `split-string'."
      (let (r s)
        (while list
          (if (not (funcall fn (car list)))
              (push (car list) s)
            (when s (push (nreverse s) r))
            (setq s nil))
          (!cdr list))
        (when s (push (nreverse s) r))
        (nreverse r)))

    (defmacro --separate (form list)
      "Anaphoric form of `-separate'."
      (declare (debug (form form)))
      (let ((y (make-symbol "yes"))
            (n (make-symbol "no")))
        `(let (,y ,n)
           (--each ,list (if ,form (!cons it ,y) (!cons it ,n)))
           (list (nreverse ,y) (nreverse ,n)))))

    (defun -separate (pred list)
      "Return a list of ((-filter PRED LIST) (-remove PRED LIST)), in one pass through the list."
      (--separate (funcall pred it) list))

    (defun ---partition-all-in-steps-reversed (n step list)
      "Private: Used by -partition-all-in-steps and -partition-in-steps."
      (when (< step 1)
        (error "Step must be a positive number, or you're looking at some juicy infinite loops."))
      (let ((result nil))
        (while list
          (!cons (-take n list) result)
          (setq list (-drop step list)))
        result))

    (defun -partition-all-in-steps (n step list)
      "Return a new list with the items in LIST grouped into N-sized sublists at offsets STEP apart.
    The last groups may contain less than N items."
      (declare (pure t) (side-effect-free t))
      (nreverse (---partition-all-in-steps-reversed n step list)))

    (defun -partition-in-steps (n step list)
      "Return a new list with the items in LIST grouped into N-sized sublists at offsets STEP apart.
    If there are not enough items to make the last group N-sized,
    those items are discarded."
      (declare (pure t) (side-effect-free t))
      (let ((result (---partition-all-in-steps-reversed n step list)))
        (while (and result (< (length (car result)) n))
          (!cdr result))
        (nreverse result)))

    (defun -partition-all (n list)
      "Return a new list with the items in LIST grouped into N-sized sublists.
    The last group may contain less than N items."
      (declare (pure t) (side-effect-free t))
      (-partition-all-in-steps n n list))

    (defun -partition (n list)
      "Return a new list with the items in LIST grouped into N-sized sublists.
    If there are not enough items to make the last group N-sized,
    those items are discarded."
      (declare (pure t) (side-effect-free t))
      (-partition-in-steps n n list))

    (defmacro --partition-by (form list)
      "Anaphoric form of `-partition-by'."
      (declare (debug (form form)))
      (let ((r (make-symbol "result"))
            (s (make-symbol "sublist"))
            (v (make-symbol "value"))
            (n (make-symbol "new-value"))
            (l (make-symbol "list")))
        `(let ((,l ,list))
           (when ,l
             (let* ((,r nil)
                    (it (car ,l))
                    (,s (list it))
                    (,v ,form)
                    (,l (cdr ,l)))
               (while ,l
                 (let* ((it (car ,l))
                        (,n ,form))
                   (unless (equal ,v ,n)
                     (!cons (nreverse ,s) ,r)
                     (setq ,s nil)
                     (setq ,v ,n))
                   (!cons it ,s)
                   (!cdr ,l)))
               (!cons (nreverse ,s) ,r)
               (nreverse ,r))))))

    (defun -partition-by (fn list)
      "Apply FN to each item in LIST, splitting it each time FN returns a new value."
      (--partition-by (funcall fn it) list))

    (defmacro --partition-by-header (form list)
      "Anaphoric form of `-partition-by-header'."
      (declare (debug (form form)))
      (let ((r (make-symbol "result"))
            (s (make-symbol "sublist"))
            (h (make-symbol "header-value"))
            (b (make-symbol "seen-body?"))
            (n (make-symbol "new-value"))
            (l (make-symbol "list")))
        `(let ((,l ,list))
           (when ,l
             (let* ((,r nil)
                    (it (car ,l))
                    (,s (list it))
                    (,h ,form)
                    (,b nil)
                    (,l (cdr ,l)))
               (while ,l
                 (let* ((it (car ,l))
                        (,n ,form))
                   (if (equal ,h ,n)
                       (when ,b
                         (!cons (nreverse ,s) ,r)
                         (setq ,s nil)
                         (setq ,b nil))
                     (setq ,b t))
                   (!cons it ,s)
                   (!cdr ,l)))
               (!cons (nreverse ,s) ,r)
               (nreverse ,r))))))

    (defun -partition-by-header (fn list)
      "Apply FN to the first item in LIST. That is the header
    value. Apply FN to each item in LIST, splitting it each time FN
    returns the header value, but only after seeing at least one
    other value (the body)."
      (--partition-by-header (funcall fn it) list))

    (defun -partition-after-pred (pred list)
      "Partition directly after each time PRED is true on an element of LIST."
      (when list
        (let ((rest (-partition-after-pred pred
                                           (cdr list))))
          (if (funcall pred (car list))
              ;;split after (car list)
              (cons (list (car list))
                    rest)

            ;;don't split after (car list)
            (cons (cons (car list)
                        (car rest))
                  (cdr rest))))))

    (defun -partition-before-pred (pred list)
      "Partition directly before each time PRED is true on an element of LIST."
      (nreverse (-map #'reverse
                      (-partition-after-pred pred (reverse list)))))

    (defun -partition-after-item (item list)
      "Partition directly after each time ITEM appears in LIST."
      (-partition-after-pred (lambda (ele) (equal ele item))
                             list))

    (defun -partition-before-item (item list)
      "Partition directly before each time ITEM appears in LIST."
      (-partition-before-pred (lambda (ele) (equal ele item))
                              list))

    (defmacro --group-by (form list)
      "Anaphoric form of `-group-by'."
      (declare (debug t))
      (let ((n (make-symbol "n"))
            (k (make-symbol "k"))
            (grp (make-symbol "grp")))
        `(nreverse
          (-map
           (lambda (,n)
             (cons (car ,n)
                   (nreverse (cdr ,n))))
           (--reduce-from
            (let* ((,k (,@form))
                   (,grp (assoc ,k acc)))
              (if ,grp
                  (setcdr ,grp (cons it (cdr ,grp)))
                (push
                 (list ,k it)
                 acc))
              acc)
            nil ,list)))))

    (defun -group-by (fn list)
      "Separate LIST into an alist whose keys are FN applied to the
    elements of LIST.  Keys are compared by `equal'."
      (--group-by (funcall fn it) list))

    (defun -interpose (sep list)
      "Return a new list of all elements in LIST separated by SEP."
      (declare (pure t) (side-effect-free t))
      (let (result)
        (when list
          (!cons (car list) result)
          (!cdr list))
        (while list
          (setq result (cons (car list) (cons sep result)))
          (!cdr list))
        (nreverse result)))

    (defun -interleave (&rest lists)
      "Return a new list of the first item in each list, then the second etc."
      (declare (pure t) (side-effect-free t))
      (let (result)
        (while (-none? 'null lists)
          (--each lists (!cons (car it) result))
          (setq lists (-map 'cdr lists)))
        (nreverse result)))

    (defmacro --zip-with (form list1 list2)
      "Anaphoric form of `-zip-with'.

    The elements in list1 are bound as symbol `it', the elements in list2 as symbol `other'."
      (declare (debug (form form form)))
      (let ((r (make-symbol "result"))
            (l1 (make-symbol "list1"))
            (l2 (make-symbol "list2")))
        `(let ((,r nil)
               (,l1 ,list1)
               (,l2 ,list2))
           (while (and ,l1 ,l2)
             (let ((it (car ,l1))
                   (other (car ,l2)))
               (!cons ,form ,r)
               (!cdr ,l1)
               (!cdr ,l2)))
           (nreverse ,r))))

    (defun -zip-with (fn list1 list2)
      "Zip the two lists LIST1 and LIST2 using a function FN.  This
    function is applied pairwise taking as first argument element of
    LIST1 and as second argument element of LIST2 at corresponding
    position.

    The anaphoric form `--zip-with' binds the elements from LIST1 as symbol `it',
    and the elements from LIST2 as symbol `other'."
      (--zip-with (funcall fn it other) list1 list2))

    (defun -zip (&rest lists)
      "Zip LISTS together.  Group the head of each list, followed by the
    second elements of each list, and so on. The lengths of the returned
    groupings are equal to the length of the shortest input list.

    If two lists are provided as arguments, return the groupings as a list
    of cons cells. Otherwise, return the groupings as a list of lists.

    Please note! This distinction is being removed in an upcoming 3.0
    release of Dash. If you rely on this behavior, use -zip-pair instead."
      (declare (pure t) (side-effect-free t))
      (let (results)
        (while (-none? 'null lists)
          (setq results (cons (mapcar 'car lists) results))
          (setq lists (mapcar 'cdr lists)))
        (setq results (nreverse results))
        (if (= (length lists) 2)
            ;; to support backward compatability, return
            ;; a cons cell if two lists were provided
            (--map (cons (car it) (cadr it)) results)
          results)))

    (defalias '-zip-pair '-zip)

    (defun -zip-fill (fill-value &rest lists)
      "Zip LISTS, with FILL-VALUE padded onto the shorter lists. The
    lengths of the returned groupings are equal to the length of the
    longest input list."
      (declare (pure t) (side-effect-free t))
      (apply '-zip (apply '-pad (cons fill-value lists))))

    (defun -unzip (lists)
      "Unzip LISTS.

    This works just like `-zip' but takes a list of lists instead of
    a variable number of arguments, such that

      (-unzip (-zip L1 L2 L3 ...))

    is identity (given that the lists are the same length).

    See also: `-zip'"
      (apply '-zip lists))

    (defun -cycle (list)
      "Return an infinite copy of LIST that will cycle through the
    elements and repeat from the beginning."
      (declare (pure t) (side-effect-free t))
      (let ((newlist (-map 'identity list)))
        (nconc newlist newlist)))

    (defun -pad (fill-value &rest lists)
      "Appends FILL-VALUE to the end of each list in LISTS such that they
    will all have the same length."
      (let* ((annotations (-annotate 'length lists))
             (n (-max (-map 'car annotations))))
        (--map (append (cdr it) (-repeat (- n (car it)) fill-value)) annotations)))

    (defun -annotate (fn list)
      "Return a list of cons cells where each cell is FN applied to each
    element of LIST paired with the unmodified element of LIST."
      (-zip (-map fn list) list))

    (defmacro --annotate (form list)
      "Anaphoric version of `-annotate'."
      (declare (debug (form form)))
      `(-annotate (lambda (it) ,form) ,list))

    (defun dash--table-carry (lists restore-lists &optional re)
      "Helper for `-table' and `-table-flat'.

    If a list overflows, carry to the right and reset the list."
      (while (not (or (car lists)
                      (equal lists '(nil))))
        (setcar lists (car restore-lists))
        (pop (cadr lists))
        (!cdr lists)
        (!cdr restore-lists)
        (when re
          (push (nreverse (car re)) (cadr re))
          (setcar re nil)
          (!cdr re))))

    (defun -table (fn &rest lists)
      "Compute outer product of LISTS using function FN.

    The function FN should have the same arity as the number of
    supplied lists.

    The outer product is computed by applying fn to all possible
    combinations created by taking one element from each list in
    order.  The dimension of the result is (length lists).

    See also: `-table-flat'"
      (let ((restore-lists (copy-sequence lists))
            (last-list (last lists))
            (re (make-list (length lists) nil)))
        (while (car last-list)
          (let ((item (apply fn (-map 'car lists))))
            (push item (car re))
            (setcar lists (cdar lists)) ;; silence byte compiler
            (dash--table-carry lists restore-lists re)))
        (nreverse (car (last re)))))

    (defun -table-flat (fn &rest lists)
      "Compute flat outer product of LISTS using function FN.

    The function FN should have the same arity as the number of
    supplied lists.

    The outer product is computed by applying fn to all possible
    combinations created by taking one element from each list in
    order.  The results are flattened, ignoring the tensor structure
    of the result.  This is equivalent to calling:

      (-flatten-n (1- (length lists)) (apply '-table fn lists))

    but the implementation here is much more efficient.

    See also: `-flatten-n', `-table'"
      (let ((restore-lists (copy-sequence lists))
            (last-list (last lists))
            re)
        (while (car last-list)
          (let ((item (apply fn (-map 'car lists))))
            (push item re)
            (setcar lists (cdar lists)) ;; silence byte compiler
            (dash--table-carry lists restore-lists)))
        (nreverse re)))

    (defun -partial (fn &rest args)
      "Take a function FN and fewer than the normal arguments to FN,
    and return a fn that takes a variable number of additional ARGS.
    When called, the returned function calls FN with ARGS first and
    then additional args."
      (apply 'apply-partially fn args))

    (defun -elem-index (elem list)
      "Return the index of the first element in the given LIST which
    is equal to the query element ELEM, or nil if there is no
    such element."
      (declare (pure t) (side-effect-free t))
      (car (-elem-indices elem list)))

    (defun -elem-indices (elem list)
      "Return the indices of all elements in LIST equal to the query
    element ELEM, in ascending order."
      (declare (pure t) (side-effect-free t))
      (-find-indices (-partial 'equal elem) list))

    (defun -find-indices (pred list)
      "Return the indices of all elements in LIST satisfying the
    predicate PRED, in ascending order."
      (apply 'append (--map-indexed (when (funcall pred it) (list it-index)) list)))

    (defmacro --find-indices (form list)
      "Anaphoric version of `-find-indices'."
      (declare (debug (form form)))
      `(-find-indices (lambda (it) ,form) ,list))

    (defun -find-index (pred list)
      "Take a predicate PRED and a LIST and return the index of the
    first element in the list satisfying the predicate, or nil if
    there is no such element.

    See also `-first'."
      (car (-find-indices pred list)))

    (defmacro --find-index (form list)
      "Anaphoric version of `-find-index'."
      (declare (debug (form form)))
      `(-find-index (lambda (it) ,form) ,list))

    (defun -find-last-index (pred list)
      "Take a predicate PRED and a LIST and return the index of the
    last element in the list satisfying the predicate, or nil if
    there is no such element.

    See also `-last'."
      (-last-item (-find-indices pred list)))

    (defmacro --find-last-index (form list)
      "Anaphoric version of `-find-last-index'."
      `(-find-last-index (lambda (it) ,form) ,list))

    (defun -select-by-indices (indices list)
      "Return a list whose elements are elements from LIST selected
    as `(nth i list)` for all i from INDICES."
      (declare (pure t) (side-effect-free t))
      (let (r)
        (--each indices
          (!cons (nth it list) r))
        (nreverse r)))

    (defun -select-columns (columns table)
      "Select COLUMNS from TABLE.

    TABLE is a list of lists where each element represents one row.
    It is assumed each row has the same length.

    Each row is transformed such that only the specified COLUMNS are
    selected.

    See also: `-select-column', `-select-by-indices'"
      (declare (pure t) (side-effect-free t))
      (--map (-select-by-indices columns it) table))

    (defun -select-column (column table)
      "Select COLUMN from TABLE.

    TABLE is a list of lists where each element represents one row.
    It is assumed each row has the same length.

    The single selected column is returned as a list.

    See also: `-select-columns', `-select-by-indices'"
      (declare (pure t) (side-effect-free t))
      (--mapcat (-select-by-indices (list column) it) table))

    (defmacro -> (x &optional form &rest more)
      "Thread the expr through the forms. Insert X as the second item
    in the first form, making a list of it if it is not a list
    already. If there are more forms, insert the first form as the
    second item in second form, etc."
      (declare (debug (form &rest [&or symbolp (sexp &rest form)])))
      (cond
       ((null form) x)
       ((null more) (if (listp form)
                        `(,(car form) ,x ,@(cdr form))
                      (list form x)))
       (:else `(-> (-> ,x ,form) ,@more))))

    (defmacro ->> (x &optional form &rest more)
      "Thread the expr through the forms. Insert X as the last item
    in the first form, making a list of it if it is not a list
    already. If there are more forms, insert the first form as the
    last item in second form, etc."
      (declare (debug ->))
      (cond
       ((null form) x)
       ((null more) (if (listp form)
                        `(,@form ,x)
                      (list form x)))
       (:else `(->> (->> ,x ,form) ,@more))))

    (defmacro --> (x &rest forms)
      "Starting with the value of X, thread each expression through FORMS.

    Insert X at the position signified by the symbol `it' in the first
    form.  If there are more forms, insert the first form at the position
    signified by `it' in in second form, etc."
      (declare (debug (form body)))
      `(-as-> ,x it ,@forms))

    (defmacro -as-> (value variable &rest forms)
      "Starting with VALUE, thread VARIABLE through FORMS.

    In the first form, bind VARIABLE to VALUE.  In the second form, bind
    VARIABLE to the result of the first form, and so forth."
      (declare (debug (form symbolp body)))
      (if (null forms)
          `,value
        `(let ((,variable ,value))
           (-as-> ,(if (symbolp (car forms))
                     (list (car forms) variable)
                   (car forms))
                ,variable
                  ,@(cdr forms)))))

    (defmacro -some-> (x &optional form &rest more)
      "When expr is non-nil, thread it through the first form (via `->'),
    and when that result is non-nil, through the next form, etc."
      (declare (debug ->))
      (if (null form) x
        (let ((result (make-symbol "result")))
          `(-some-> (-when-let (,result ,x)
                      (-> ,result ,form))
                    ,@more))))

    (defmacro -some->> (x &optional form &rest more)
      "When expr is non-nil, thread it through the first form (via `->>'),
    and when that result is non-nil, through the next form, etc."
      (declare (debug ->))
      (if (null form) x
        (let ((result (make-symbol "result")))
          `(-some->> (-when-let (,result ,x)
                       (->> ,result ,form))
                     ,@more))))

    (defmacro -some--> (x &optional form &rest more)
      "When expr in non-nil, thread it through the first form (via `-->'),
    and when that result is non-nil, through the next form, etc."
      (declare (debug ->))
      (if (null form) x
        (let ((result (make-symbol "result")))
          `(-some--> (-when-let (,result ,x)
                       (--> ,result ,form))
                     ,@more))))

    (defun -grade-up (comparator list)
      "Grade elements of LIST using COMPARATOR relation, yielding a
    permutation vector such that applying this permutation to LIST
    sorts it in ascending order."
      ;; ugly hack to "fix" lack of lexical scope
      (let ((comp `(lambda (it other) (funcall ',comparator (car it) (car other)))))
        (->> (--map-indexed (cons it it-index) list)
             (-sort comp)
             (-map 'cdr))))

    (defun -grade-down (comparator list)
      "Grade elements of LIST using COMPARATOR relation, yielding a
    permutation vector such that applying this permutation to LIST
    sorts it in descending order."
      ;; ugly hack to "fix" lack of lexical scope
      (let ((comp `(lambda (it other) (funcall ',comparator (car other) (car it)))))
        (->> (--map-indexed (cons it it-index) list)
             (-sort comp)
             (-map 'cdr))))

    (defvar dash--source-counter 0
      "Monotonic counter for generated symbols.")

    (defun dash--match-make-source-symbol ()
      "Generate a new dash-source symbol.

    All returned symbols are guaranteed to be unique."
      (prog1 (make-symbol (format "--dash-source-%d--" dash--source-counter))
        (setq dash--source-counter (1+ dash--source-counter))))

    (defun dash--match-ignore-place-p (symbol)
      "Return non-nil if SYMBOL is a symbol and starts with _."
      (and (symbolp symbol)
           (eq (aref (symbol-name symbol) 0) ?_)))

    (defun dash--match-cons-skip-cdr (skip-cdr source)
      "Helper function generating idiomatic shifting code."
      (cond
       ((= skip-cdr 0)
        `(pop ,source))
       (t
        `(prog1 ,(dash--match-cons-get-car skip-cdr source)
           (setq ,source ,(dash--match-cons-get-cdr (1+ skip-cdr) source))))))

    (defun dash--match-cons-get-car (skip-cdr source)
      "Helper function generating idiomatic code to get nth car."
      (cond
       ((= skip-cdr 0)
        `(car ,source))
       ((= skip-cdr 1)
        `(cadr ,source))
       (t
        `(nth ,skip-cdr ,source))))

    (defun dash--match-cons-get-cdr (skip-cdr source)
      "Helper function generating idiomatic code to get nth cdr."
      (cond
       ((= skip-cdr 0)
        source)
       ((= skip-cdr 1)
        `(cdr ,source))
       (t
        `(nthcdr ,skip-cdr ,source))))

    (defun dash--match-cons (match-form source)
      "Setup a cons matching environment and call the real matcher."
      (let ((s (dash--match-make-source-symbol))
            (n 0)
            (m match-form))
        (while (and (consp m)
                    (dash--match-ignore-place-p (car m)))
          (setq n (1+ n)) (!cdr m))
        (cond
         ;; when we only have one pattern in the list, we don't have to
         ;; create a temporary binding (--dash-source--) for the source
         ;; and just use the input directly
         ((and (consp m)
               (not (cdr m)))
          (dash--match (car m) (dash--match-cons-get-car n source)))
         ;; handle other special types
         ((> n 0)
          (dash--match m (dash--match-cons-get-cdr n source)))
         ;; this is the only entry-point for dash--match-cons-1, that's
         ;; why we can't simply use the above branch, it would produce
         ;; infinite recursion
         (t
          (cons (list s source) (dash--match-cons-1 match-form s))))))

    (defun dash--match-cons-1 (match-form source &optional props)
      "Match MATCH-FORM against SOURCE.

    MATCH-FORM is a proper or improper list.  Each element of
    MATCH-FORM is either a symbol, which gets bound to the respective
    value in source or another match form which gets destructured
    recursively.

    If the cdr of last cons cell in the list is `nil', matching stops
    there.

    SOURCE is a proper or improper list."
      (let ((skip-cdr (or (plist-get props :skip-cdr) 0)))
        (cond
         ((consp match-form)
          (cond
           ((cdr match-form)
            (cond
             ((and (symbolp (car match-form))
                   (memq (car match-form) '(&keys &plist &alist &hash)))
              (dash--match-kv match-form (dash--match-cons-get-cdr skip-cdr source)))
             ((dash--match-ignore-place-p (car match-form))
              (dash--match-cons-1 (cdr match-form) source
                                  (plist-put props :skip-cdr (1+ skip-cdr))))
             (t
              (-concat (dash--match (car match-form) (dash--match-cons-skip-cdr skip-cdr source))
                       (dash--match-cons-1 (cdr match-form) source)))))
           (t ;; Last matching place, no need for shift
            (dash--match (car match-form) (dash--match-cons-get-car skip-cdr source)))))
         ((eq match-form nil)
          nil)
         (t ;; Handle improper lists.  Last matching place, no need for shift
          (dash--match match-form (dash--match-cons-get-cdr skip-cdr source))))))

    (defun dash--vector-tail (seq start)
      "Return the tail of SEQ starting at START."
      (cond
       ((vectorp seq)
        (let* ((re-length (- (length seq) start))
               (re (make-vector re-length 0)))
          (--dotimes re-length (aset re it (aref seq (+ it start))))
          re))
       ((stringp seq)
        (substring seq start))))

    (defun dash--match-vector (match-form source)
      "Setup a vector matching environment and call the real matcher."
      (let ((s (dash--match-make-source-symbol)))
        (cond
         ;; don't bind `s' if we only have one sub-pattern
         ((= (length match-form) 1)
          (dash--match (aref match-form 0) `(aref ,source 0)))
         ;; if the source is a symbol, we don't need to re-bind it
         ((symbolp source)
          (dash--match-vector-1 match-form source))
         ;; don't bind `s' if we only have one sub-pattern which is not ignored
         ((let* ((ignored-places (mapcar 'dash--match-ignore-place-p match-form))
                 (ignored-places-n (length (-remove 'null ignored-places))))
            (when (= ignored-places-n (1- (length match-form)))
              (let ((n (-find-index 'null ignored-places)))
                (dash--match (aref match-form n) `(aref ,source ,n))))))
         (t
          (cons (list s source) (dash--match-vector-1 match-form s))))))

    (defun dash--match-vector-1 (match-form source)
      "Match MATCH-FORM against SOURCE.

    MATCH-FORM is a vector.  Each element of MATCH-FORM is either a
    symbol, which gets bound to the respective value in source or
    another match form which gets destructured recursively.

    If second-from-last place in MATCH-FORM is the symbol &rest, the
    next element of the MATCH-FORM is matched against the tail of
    SOURCE, starting at index of the &rest symbol.  This is
    conceptually the same as the (head . tail) match for improper
    lists, where dot plays the role of &rest.

    SOURCE is a vector.

    If the MATCH-FORM vector is shorter than SOURCE vector, only
    the (length MATCH-FORM) places are bound, the rest of the SOURCE
    is discarded."
      (let ((i 0)
            (l (length match-form))
            (re))
        (while (< i l)
          (let ((m (aref match-form i)))
            (push (cond
                   ((and (symbolp m)
                         (eq m '&rest))
                    (prog1 (dash--match
                            (aref match-form (1+ i))
                            `(dash--vector-tail ,source ,i))
                      (setq i l)))
                   ((and (symbolp m)
                         ;; do not match symbols starting with _
                         (not (eq (aref (symbol-name m) 0) ?_)))
                    (list (list m `(aref ,source ,i))))
                   ((not (symbolp m))
                    (dash--match m `(aref ,source ,i))))
                  re)
            (setq i (1+ i))))
        (-flatten-n 1 (nreverse re))))

    (defun dash--match-kv (match-form source)
      "Setup a kv matching environment and call the real matcher.

    kv can be any key-value store, such as plist, alist or hash-table."
      (let ((s (dash--match-make-source-symbol)))
        (cond
         ;; don't bind `s' if we only have one sub-pattern (&type key val)
         ((= (length match-form) 3)
          (dash--match-kv-1 (cdr match-form) source (car match-form)))
         ;; if the source is a symbol, we don't need to re-bind it
         ((symbolp source)
          (dash--match-kv-1 (cdr match-form) source (car match-form)))
         (t
          (cons (list s source) (dash--match-kv-1 (cdr match-form) s (car match-form)))))))

    (defun dash--match-kv-1 (match-form source type)
      "Match MATCH-FORM against SOURCE of type TYPE.

    MATCH-FORM is a proper list of the form (key1 place1 ... keyN
    placeN).  Each placeK is either a symbol, which gets bound to the
    value of keyK retrieved from the key-value store, or another
    match form which gets destructured recursively.

    SOURCE is a key-value store of type TYPE, which can be a plist,
    an alist or a hash table.

    TYPE is a token specifying the type of the key-value store.
    Valid values are &plist, &alist and &hash."
      (-flatten-n 1 (-map
                     (lambda (kv)
                       (let* ((k (car kv))
                              (v (cadr kv))
                              (getter (cond
                                       ((or (eq type '&plist) (eq type '&keys))
                                        `(plist-get ,source ,k))
                                       ((eq type '&alist)
                                        `(cdr (assoc ,k ,source)))
                                       ((eq type '&hash)
                                        `(gethash ,k ,source)))))
                         (cond
                          ((symbolp v)
                           (list (list v getter)))
                          (t (dash--match v getter)))))
                     (-partition 2 match-form))))

    (defun dash--match-symbol (match-form source)
      "Bind a symbol.

    This works just like `let', there is no destructuring."
      (list (list match-form source)))

    (defun dash--match (match-form source)
      "Match MATCH-FORM against SOURCE.

    This function tests the MATCH-FORM and dispatches to specific
    matchers based on the type of the expression.

    Key-value stores are disambiguated by placing a token &plist,
    &alist or &hash as a first item in the MATCH-FORM."
      (cond
       ((symbolp match-form)
        (dash--match-symbol match-form source))
       ((consp match-form)
        (cond
         ;; Handle the "x &as" bindings first.
         ((and (consp (cdr match-form))
               (symbolp (car match-form))
               (eq '&as (cadr match-form)))
          (let ((s (car match-form)))
            (cons (list s source)
                  (dash--match (cddr match-form) s))))
         ((memq (car match-form) '(&keys &plist &alist &hash))
          (dash--match-kv match-form source))
         (t (dash--match-cons match-form source))))
       ((vectorp match-form)
        ;; We support the &as binding in vectors too
        (cond
         ((and (> (length match-form) 2)
               (symbolp (aref match-form 0))
               (eq '&as (aref match-form 1)))
          (let ((s (aref match-form 0)))
            (cons (list s source)
                  (dash--match (dash--vector-tail match-form 2) s))))
         (t (dash--match-vector match-form source))))))

    (defmacro -let* (varlist &rest body)
      "Bind variables according to VARLIST then eval BODY.

    VARLIST is a list of lists of the form (PATTERN SOURCE).  Each
    PATTERN is matched against the SOURCE structurally.  SOURCE is
    only evaluated once for each PATTERN.

    Each SOURCE can refer to the symbols already bound by this
    VARLIST.  This is useful if you want to destructure SOURCE
    recursively but also want to name the intermediate structures.

    See `-let' for the list of all possible patterns."
      (declare (debug ((&rest (sexp form)) body))
               (indent 1))
      (let ((bindings (--mapcat (dash--match (car it) (cadr it)) varlist)))
        `(let* ,bindings
           ,@body)))

    (defmacro -let (varlist &rest body)
      "Bind variables according to VARLIST then eval BODY.

    VARLIST is a list of lists of the form (PATTERN SOURCE).  Each
    PATTERN is matched against the SOURCE \"structurally\".  SOURCE
    is only evaluated once for each PATTERN.  Each PATTERN is matched
    recursively, and can therefore contain sub-patterns which are
    matched against corresponding sub-expressions of SOURCE.

    All the SOURCEs are evalled before any symbols are
    bound (i.e. \"in parallel\").

    If VARLIST only contains one (PATTERN SOURCE) element, you can
    optionally specify it using a vector and discarding the
    outer-most parens.  Thus

      (-let ((PATTERN SOURCE)) ..)

    becomes

      (-let [PATTERN SOURCE] ..).

    `-let' uses a convention of not binding places (symbols) starting
    with _ whenever it's possible.  You can use this to skip over
    entries you don't care about.  However, this is not *always*
    possible (as a result of implementation) and these symbols might
    get bound to undefined values.

    Following is the overview of supported patterns.  Remember that
    patterns can be matched recursively, so every a, b, aK in the
    following can be a matching construct and not necessarily a
    symbol/variable.

    Symbol:

      a - bind the SOURCE to A.  This is just like regular `let'.

    Conses and lists:

      (a) - bind `car' of cons/list to A

      (a . b) - bind car of cons to A and `cdr' to B

      (a b) - bind car of list to A and `cadr' to B

      (a1 a2 a3  ...) - bind 0th car of list to A1, 1st to A2, 2nd to A3 ...

      (a1 a2 a3 ... aN . rest) - as above, but bind the Nth cdr to REST.

    Vectors:

      [a] - bind 0th element of a non-list sequence to A (works with
            vectors, strings, bit arrays...)

      [a1 a2 a3 ...] - bind 0th element of non-list sequence to A0, 1st to
                       A1, 2nd to A2, ...
                       If the PATTERN is shorter than SOURCE, the values at
                       places not in PATTERN are ignored.
                       If the PATTERN is longer than SOURCE, an `error' is
                       thrown.

      [a1 a2 a3 ... &rest rest] - as above, but bind the rest of
                                  the sequence to REST.  This is
                                  conceptually the same as improper list
                                  matching (a1 a2 ... aN . rest)

    Key/value stores:

      (&plist key0 a0 ... keyN aN) - bind value mapped by keyK in the
                                     SOURCE plist to aK.  If the
                                     value is not found, aK is nil.

      (&alist key0 a0 ... keyN aN) - bind value mapped by keyK in the
                                     SOURCE alist to aK.  If the
                                     value is not found, aK is nil.

      (&hash key0 a0 ... keyN aN) - bind value mapped by keyK in the
                                    SOURCE hash table to aK.  If the
                                    value is not found, aK is nil.

    Further, special keyword &keys supports \"inline\" matching of
    plist-like key-value pairs, similarly to &keys keyword of
    `cl-defun'.

      (a1 a2 ... aN &keys key1 b1 ... keyN bK)

    This binds N values from the list to a1 ... aN, then interprets
    the cdr as a plist (see key/value matching above).

    You can name the source using the syntax SYMBOL &as PATTERN.
    This syntax works with lists (proper or improper), vectors and
    all types of maps.

      (list &as a b c) (list 1 2 3)

    binds A to 1, B to 2, C to 3 and LIST to (1 2 3).

    Similarly:

      (bounds &as beg . end) (cons 1 2)

    binds BEG to 1, END to 2 and BOUNDS to (1 . 2).

      (items &as first . rest) (list 1 2 3)

    binds FIRST to 1, REST to (2 3) and ITEMS to (1 2 3)

      [vect &as _ b c] [1 2 3]

    binds B to 2, C to 3 and VECT to [1 2 3] (_ avoids binding as usual).

      (plist &as &plist :b b) (list :a 1 :b 2 :c 3)

    binds B to 2 and PLIST to (:a 1 :b 2 :c 3).  Same for &alist and &hash.

    This is especially useful when we want to capture the result of a
    computation and destructure at the same time.  Consider the
    form (function-returning-complex-structure) returning a list of
    two vectors with two items each.  We want to capture this entire
    result and pass it to another computation, but at the same time
    we want to get the second item from each vector.  We can achieve
    it with pattern

      (result &as [_ a] [_ b]) (function-returning-complex-structure)

    Note: Clojure programmers may know this feature as the \":as
    binding\".  The difference is that we put the &as at the front
    because we need to support improper list binding."
      (declare (debug ([&or (&rest (sexp form))
                            (vector [&rest [sexp form]])]
                       body))
               (indent 1))
      (if (vectorp varlist)
          `(let* ,(dash--match (aref varlist 0) (aref varlist 1))
             ,@body)
        (let* ((inputs (--map-indexed (list (make-symbol (format "input%d" it-index)) (cadr it)) varlist))
               (new-varlist (--map (list (caar it) (cadr it)) (-zip varlist inputs))))
          `(let ,inputs
             (-let* ,new-varlist ,@body)))))

    (defmacro -lambda (match-form &rest body)
      "Return a lambda which destructures its input as MATCH-FORM and executes BODY.

    Note that you have to enclose the MATCH-FORM in a pair of parens,
    such that:

      (-lambda (x) body)
      (-lambda (x y ...) body)

    has the usual semantics of `lambda'.  Furthermore, these get
    translated into normal lambda, so there is no performance
    penalty.

    See `-let' for the description of destructuring mechanism."
      (declare (doc-string 2) (indent defun)
               (debug (&define sexp
                               [&optional stringp]
                               [&optional ("interactive" interactive)]
                               def-body)))
      (cond
       ((not (consp match-form))
        (signal 'wrong-type-argument "match-form must be a list"))
       ;; no destructuring, so just return regular lambda to make things faster
       ((-all? 'symbolp match-form)
        `(lambda ,match-form ,@body))
       (t
        (let* ((inputs (--map-indexed (list it (make-symbol (format "input%d" it-index))) match-form)))
          ;; TODO: because inputs to the lambda are evaluated only once,
          ;; -let* need not to create the extra bindings to ensure that.
          ;; We should find a way to optimize that.  Not critical however.
          `(lambda ,(--map (cadr it) inputs)
             (-let* ,inputs ,@body))))))

    (defmacro -if-let* (vars-vals then &rest else)
      "If all VALS evaluate to true, bind them to their corresponding
    VARS and do THEN, otherwise do ELSE. VARS-VALS should be a list
    of (VAR VAL) pairs.

    Note: binding is done according to `-let*'.  VALS are evaluated
    sequentially, and evaluation stops after the first nil VAL is
    encountered."
      (declare (debug ((&rest (sexp form)) form body))
               (indent 2))
      (->> vars-vals
           (--mapcat (dash--match (car it) (cadr it)))
           (--reduce-r-from
            (let ((var (car it))
                  (val (cadr it)))
              `(let ((,var ,val))
                 (if ,var ,acc ,@else)))
            then)))

    (defmacro -if-let (var-val then &rest else)
      "If VAL evaluates to non-nil, bind it to VAR and do THEN,
    otherwise do ELSE.

    Note: binding is done according to `-let'.

    \(fn (VAR VAL) THEN &rest ELSE)"
      (declare (debug ((sexp form) form body))
               (indent 2))
      `(-if-let* (,var-val) ,then ,@else))

    (defmacro --if-let (val then &rest else)
      "If VAL evaluates to non-nil, bind it to symbol `it' and do THEN,
    otherwise do ELSE."
      (declare (debug (form form body))
               (indent 2))
      `(-if-let (it ,val) ,then ,@else))

    (defmacro -when-let* (vars-vals &rest body)
      "If all VALS evaluate to true, bind them to their corresponding
    VARS and execute body. VARS-VALS should be a list of (VAR VAL)
    pairs.

    Note: binding is done according to `-let*'.  VALS are evaluated
    sequentially, and evaluation stops after the first nil VAL is
    encountered."
      (declare (debug ((&rest (sexp form)) body))
               (indent 1))
      `(-if-let* ,vars-vals (progn ,@body)))

    (defmacro -when-let (var-val &rest body)
      "If VAL evaluates to non-nil, bind it to VAR and execute body.

    Note: binding is done according to `-let'.

    \(fn (VAR VAL) &rest BODY)"
      (declare (debug ((sexp form) body))
               (indent 1))
      `(-if-let ,var-val (progn ,@body)))

    (defmacro --when-let (val &rest body)
      "If VAL evaluates to non-nil, bind it to symbol `it' and
    execute body."
      (declare (debug (form body))
               (indent 1))
      `(--if-let ,val (progn ,@body)))

    (defvar -compare-fn nil
      "Tests for equality use this function or `equal' if this is nil.
    It should only be set using dynamic scope with a let, like:

      (let ((-compare-fn #'=)) (-union numbers1 numbers2 numbers3)")

    (defun -distinct (list)
      "Return a new list with all duplicates removed.
    The test for equality is done with `equal',
    or with `-compare-fn' if that's non-nil.

    Alias: `-uniq'"
      (let (result)
        (--each list (unless (-contains? result it) (!cons it result)))
        (nreverse result)))

    (defalias '-uniq '-distinct)

    (defun -union (list list2)
      "Return a new list containing the elements of LIST and elements of LIST2 that are not in LIST.
    The test for equality is done with `equal',
    or with `-compare-fn' if that's non-nil."
      ;; We fall back to iteration implementation if the comparison
      ;; function isn't one of `eq', `eql' or `equal'.
      (let* ((result (reverse list))
             ;; TODO: get rid of this dynamic variable, pass it as an
             ;; argument instead.
             (-compare-fn (if (bound-and-true-p -compare-fn)
                              -compare-fn
                            'equal)))
        (if (memq -compare-fn '(eq eql equal))
            (let ((ht (make-hash-table :test -compare-fn)))
              (--each list (puthash it t ht))
              (--each list2 (unless (gethash it ht) (!cons it result))))
          (--each list2 (unless (-contains? result it) (!cons it result))))
        (nreverse result)))

    (defun -intersection (list list2)
      "Return a new list containing only the elements that are members of both LIST and LIST2.
    The test for equality is done with `equal',
    or with `-compare-fn' if that's non-nil."
      (--filter (-contains? list2 it) list))

    (defun -difference (list list2)
      "Return a new list with only the members of LIST that are not in LIST2.
    The test for equality is done with `equal',
    or with `-compare-fn' if that's non-nil."
      (--filter (not (-contains? list2 it)) list))

    (defun -powerset (list)
      "Return the power set of LIST."
      (if (null list) '(())
        (let ((last (-powerset (cdr list))))
          (append (mapcar (lambda (x) (cons (car list) x)) last)
                  last))))

    (defun -permutations (list)
      "Return the permutations of LIST."
      (if (null list) '(())
        (apply #'append
               (mapcar (lambda (x)
                         (mapcar (lambda (perm) (cons x perm))
                                 (-permutations (remove x list))))
                       list))))

    (defun -contains? (list element)
      "Return non-nil if LIST contains ELEMENT.

    The test for equality is done with `equal', or with `-compare-fn'
    if that's non-nil.

    Alias: `-contains-p'"
      (not
       (null
        (cond
         ((null -compare-fn)    (member element list))
         ((eq -compare-fn 'eq)  (memq element list))
         ((eq -compare-fn 'eql) (memql element list))
         (t
          (let ((lst list))
            (while (and lst
                        (not (funcall -compare-fn element (car lst))))
              (setq lst (cdr lst)))
            lst))))))

    (defalias '-contains-p '-contains?)

    (defun -same-items? (list list2)
      "Return true if LIST and LIST2 has the same items.

    The order of the elements in the lists does not matter.

    Alias: `-same-items-p'"
      (let ((length-a (length list))
            (length-b (length list2)))
        (and
         (= length-a length-b)
         (= length-a (length (-intersection list list2))))))

    (defalias '-same-items-p '-same-items?)

    (defun -is-prefix? (prefix list)
      "Return non-nil if PREFIX is prefix of LIST.

    Alias: `-is-prefix-p'"
      (declare (pure t) (side-effect-free t))
      (--each-while list (equal (car prefix) it)
        (!cdr prefix))
      (not prefix))

    (defun -is-suffix? (suffix list)
      "Return non-nil if SUFFIX is suffix of LIST.

    Alias: `-is-suffix-p'"
      (declare (pure t) (side-effect-free t))
      (-is-prefix? (reverse suffix) (reverse list)))

    (defun -is-infix? (infix list)
      "Return non-nil if INFIX is infix of LIST.

    This operation runs in O(n^2) time

    Alias: `-is-infix-p'"
      (declare (pure t) (side-effect-free t))
      (let (done)
        (while (and (not done) list)
          (setq done (-is-prefix? infix list))
          (!cdr list))
        done))

    (defalias '-is-prefix-p '-is-prefix?)
    (defalias '-is-suffix-p '-is-suffix?)
    (defalias '-is-infix-p '-is-infix?)

    (defun -sort (comparator list)
      "Sort LIST, stably, comparing elements using COMPARATOR.
    Return the sorted list.  LIST is NOT modified by side effects.
    COMPARATOR is called with two elements of LIST, and should return non-nil
    if the first element should sort before the second."
      (sort (copy-sequence list) comparator))

    (defmacro --sort (form list)
      "Anaphoric form of `-sort'."
      (declare (debug (form form)))
      `(-sort (lambda (it other) ,form) ,list))

    (defun -list (&rest args)
      "Return a list with ARGS.

    If first item of ARGS is already a list, simply return ARGS.  If
    not, return a list with ARGS as elements."
      (declare (pure t) (side-effect-free t))
      (let ((arg (car args)))
        (if (listp arg) arg args)))

    (defun -repeat (n x)
      "Return a list with X repeated N times.
    Return nil if N is less than 1."
      (declare (pure t) (side-effect-free t))
      (let (ret)
        (--dotimes n (!cons x ret))
        ret))

    (defun -sum (list)
      "Return the sum of LIST."
      (declare (pure t) (side-effect-free t))
      (apply '+ list))

    (defun -product (list)
      "Return the product of LIST."
      (declare (pure t) (side-effect-free t))
      (apply '* list))

    (defun -max (list)
      "Return the largest value from LIST of numbers or markers."
      (declare (pure t) (side-effect-free t))
      (apply 'max list))

    (defun -min (list)
      "Return the smallest value from LIST of numbers or markers."
      (declare (pure t) (side-effect-free t))
      (apply 'min list))

    (defun -max-by (comparator list)
      "Take a comparison function COMPARATOR and a LIST and return
    the greatest element of the list by the comparison function.

    See also combinator `-on' which can transform the values before
    comparing them."
      (--reduce (if (funcall comparator it acc) it acc) list))

    (defun -min-by (comparator list)
      "Take a comparison function COMPARATOR and a LIST and return
    the least element of the list by the comparison function.

    See also combinator `-on' which can transform the values before
    comparing them."
      (--reduce (if (funcall comparator it acc) acc it) list))

    (defmacro --max-by (form list)
      "Anaphoric version of `-max-by'.

    The items for the comparator form are exposed as \"it\" and \"other\"."
      (declare (debug (form form)))
      `(-max-by (lambda (it other) ,form) ,list))

    (defmacro --min-by (form list)
      "Anaphoric version of `-min-by'.

    The items for the comparator form are exposed as \"it\" and \"other\"."
      (declare (debug (form form)))
      `(-min-by (lambda (it other) ,form) ,list))

    (defun -iterate (fun init n)
      "Return a list of iterated applications of FUN to INIT.

    This means a list of form:

      (init (fun init) (fun (fun init)) ...)

    N is the length of the returned list."
      (if (= n 0) nil
        (let ((r (list init)))
          (--dotimes (1- n)
            (push (funcall fun (car r)) r))
          (nreverse r))))

    (defun -fix (fn list)
      "Compute the (least) fixpoint of FN with initial input LIST.

    FN is called at least once, results are compared with `equal'."
      (let ((re (funcall fn list)))
        (while (not (equal list re))
          (setq list re)
          (setq re (funcall fn re)))
        re))

    (defmacro --fix (form list)
      "Anaphoric form of `-fix'."
      `(-fix (lambda (it) ,form) ,list))

    (defun -unfold (fun seed)
      "Build a list from SEED using FUN.

    This is \"dual\" operation to `-reduce-r': while -reduce-r
    consumes a list to produce a single value, `-unfold' takes a
    seed value and builds a (potentially infinite!) list.

    FUN should return `nil' to stop the generating process, or a
    cons (A . B), where A will be prepended to the result and B is
    the new seed."
      (let ((last (funcall fun seed)) r)
        (while last
          (push (car last) r)
          (setq last (funcall fun (cdr last))))
        (nreverse r)))

    (defmacro --unfold (form seed)
      "Anaphoric version of `-unfold'."
      (declare (debug (form form)))
      `(-unfold (lambda (it) ,form) ,seed))

    (defun -cons-pair? (con)
      "Return non-nil if CON is true cons pair.
    That is (A . B) where B is not a list."
      (declare (pure t) (side-effect-free t))
      (and (listp con)
           (not (listp (cdr con)))))

    (defun -cons-to-list (con)
      "Convert a cons pair to a list with `car' and `cdr' of the pair respectively."
      (declare (pure t) (side-effect-free t))
      (list (car con) (cdr con)))

    (defun -value-to-list (val)
      "Convert a value to a list.

    If the value is a cons pair, make a list with two elements, `car'
    and `cdr' of the pair respectively.

    If the value is anything else, wrap it in a list."
      (declare (pure t) (side-effect-free t))
      (cond
       ((-cons-pair? val) (-cons-to-list val))
       (t (list val))))

    (defun -tree-mapreduce-from (fn folder init-value tree)
      "Apply FN to each element of TREE, and make a list of the results.
    If elements of TREE are lists themselves, apply FN recursively to
    elements of these nested lists.

    Then reduce the resulting lists using FOLDER and initial value
    INIT-VALUE. See `-reduce-r-from'.

    This is the same as calling `-tree-reduce-from' after `-tree-map'
    but is twice as fast as it only traverse the structure once."
      (cond
       ((not tree) nil)
       ((-cons-pair? tree) (funcall fn tree))
       ((listp tree)
        (-reduce-r-from folder init-value (mapcar (lambda (x) (-tree-mapreduce-from fn folder init-value x)) tree)))
       (t (funcall fn tree))))

    (defmacro --tree-mapreduce-from (form folder init-value tree)
      "Anaphoric form of `-tree-mapreduce-from'."
      (declare (debug (form form form form)))
      `(-tree-mapreduce-from (lambda (it) ,form) (lambda (it acc) ,folder) ,init-value ,tree))

    (defun -tree-mapreduce (fn folder tree)
      "Apply FN to each element of TREE, and make a list of the results.
    If elements of TREE are lists themselves, apply FN recursively to
    elements of these nested lists.

    Then reduce the resulting lists using FOLDER and initial value
    INIT-VALUE. See `-reduce-r-from'.

    This is the same as calling `-tree-reduce' after `-tree-map'
    but is twice as fast as it only traverse the structure once."
      (cond
       ((not tree) nil)
       ((-cons-pair? tree) (funcall fn tree))
       ((listp tree)
        (-reduce-r folder (mapcar (lambda (x) (-tree-mapreduce fn folder x)) tree)))
       (t (funcall fn tree))))

    (defmacro --tree-mapreduce (form folder tree)
      "Anaphoric form of `-tree-mapreduce'."
      (declare (debug (form form form)))
      `(-tree-mapreduce (lambda (it) ,form) (lambda (it acc) ,folder) ,tree))

    (defun -tree-map (fn tree)
      "Apply FN to each element of TREE while preserving the tree structure."
      (cond
       ((not tree) nil)
       ((-cons-pair? tree) (funcall fn tree))
       ((listp tree)
        (mapcar (lambda (x) (-tree-map fn x)) tree))
       (t (funcall fn tree))))

    (defmacro --tree-map (form tree)
      "Anaphoric form of `-tree-map'."
      (declare (debug (form form)))
      `(-tree-map (lambda (it) ,form) ,tree))

    (defun -tree-reduce-from (fn init-value tree)
      "Use FN to reduce elements of list TREE.
    If elements of TREE are lists themselves, apply the reduction recursively.

    FN is first applied to INIT-VALUE and first element of the list,
    then on this result and second element from the list etc.

    The initial value is ignored on cons pairs as they always contain
    two elements."
      (cond
       ((not tree) nil)
       ((-cons-pair? tree) tree)
       ((listp tree)
        (-reduce-r-from fn init-value (mapcar (lambda (x) (-tree-reduce-from fn init-value x)) tree)))
       (t tree)))

    (defmacro --tree-reduce-from (form init-value tree)
      "Anaphoric form of `-tree-reduce-from'."
      (declare (debug (form form form)))
      `(-tree-reduce-from (lambda (it acc) ,form) ,init-value ,tree))

    (defun -tree-reduce (fn tree)
      "Use FN to reduce elements of list TREE.
    If elements of TREE are lists themselves, apply the reduction recursively.

    FN is first applied to first element of the list and second
    element, then on this result and third element from the list etc.

    See `-reduce-r' for how exactly are lists of zero or one element handled."
      (cond
       ((not tree) nil)
       ((-cons-pair? tree) tree)
       ((listp tree)
        (-reduce-r fn (mapcar (lambda (x) (-tree-reduce fn x)) tree)))
       (t tree)))

    (defmacro --tree-reduce (form tree)
      "Anaphoric form of `-tree-reduce'."
      (declare (debug (form form)))
      `(-tree-reduce (lambda (it acc) ,form) ,tree))

    (defun -tree-map-nodes (pred fun tree)
      "Call FUN on each node of TREE that satisfies PRED.

    If PRED returns nil, continue descending down this node.  If PRED
    returns non-nil, apply FUN to this node and do not descend
    further."
      (if (funcall pred tree)
          (funcall fun tree)
        (if (and (listp tree)
                 (not (-cons-pair? tree)))
            (-map (lambda (x) (-tree-map-nodes pred fun x)) tree)
          tree)))

    (defmacro --tree-map-nodes (pred form tree)
      "Anaphoric form of `-tree-map-nodes'."
      `(-tree-map-nodes (lambda (it) ,pred) (lambda (it) ,form) ,tree))

    (defun -tree-seq (branch children tree)
      "Return a sequence of the nodes in TREE, in depth-first search order.

    BRANCH is a predicate of one argument that returns non-nil if the
    passed argument is a branch, that is, a node that can have children.

    CHILDREN is a function of one argument that returns the children
    of the passed branch node.

    Non-branch nodes are simply copied."
      (cons tree
            (when (funcall branch tree)
              (-mapcat (lambda (x) (-tree-seq branch children x))
                       (funcall children tree)))))

    (defmacro --tree-seq (branch children tree)
      "Anaphoric form of `-tree-seq'."
      `(-tree-seq (lambda (it) ,branch) (lambda (it) ,children) ,tree))

    (defun -clone (list)
      "Create a deep copy of LIST.
    The new list has the same elements and structure but all cons are
    replaced with new ones.  This is useful when you need to clone a
    structure such as plist or alist."
      (declare (pure t) (side-effect-free t))
      (-tree-map 'identity list))

    (defun dash-enable-font-lock ()
      "Add syntax highlighting to dash functions, macros and magic values."
      (eval-after-load 'lisp-mode
        '(progn
           (let ((new-keywords '(
                                 "-each"
                                 "--each"
                                 "-each-indexed"
                                 "--each-indexed"
                                 "-each-while"
                                 "--each-while"
                                 "-dotimes"
                                 "--dotimes"
                                 "-map"
                                 "--map"
                                 "-reduce-from"
                                 "--reduce-from"
                                 "-reduce"
                                 "--reduce"
                                 "-reduce-r-from"
                                 "--reduce-r-from"
                                 "-reduce-r"
                                 "--reduce-r"
                                 "-filter"
                                 "--filter"
                                 "-select"
                                 "--select"
                                 "-remove"
                                 "--remove"
                                 "-reject"
                                 "--reject"
                                 "-remove-first"
                                 "--remove-first"
                                 "-reject-first"
                                 "--reject-first"
                                 "-remove-last"
                                 "--remove-last"
                                 "-reject-last"
                                 "--reject-last"
                                 "-remove-item"
                                 "-non-nil"
                                 "-keep"
                                 "--keep"
                                 "-map-indexed"
                                 "--map-indexed"
                                 "-splice"
                                 "--splice"
                                 "-splice-list"
                                 "--splice-list"
                                 "-map-when"
                                 "--map-when"
                                 "-replace-where"
                                 "--replace-where"
                                 "-map-first"
                                 "--map-first"
                                 "-map-last"
                                 "--map-last"
                                 "-replace"
                                 "-replace-first"
                                 "-replace-last"
                                 "-flatten"
                                 "-flatten-n"
                                 "-concat"
                                 "-mapcat"
                                 "--mapcat"
                                 "-copy"
                                 "-cons*"
                                 "-snoc"
                                 "-first"
                                 "--first"
                                 "-find"
                                 "--find"
                                 "-some"
                                 "--some"
                                 "-any"
                                 "--any"
                                 "-last"
                                 "--last"
                                 "-first-item"
                                 "-last-item"
                                 "-butlast"
                                 "-count"
                                 "--count"
                                 "-any?"
                                 "--any?"
                                 "-some?"
                                 "--some?"
                                 "-any-p"
                                 "--any-p"
                                 "-some-p"
                                 "--some-p"
                                 "-all?"
                                 "--all?"
                                 "-every?"
                                 "--every?"
                                 "-all-p"
                                 "--all-p"
                                 "-every-p"
                                 "--every-p"
                                 "-none?"
                                 "--none?"
                                 "-none-p"
                                 "--none-p"
                                 "-only-some?"
                                 "--only-some?"
                                 "-only-some-p"
                                 "--only-some-p"
                                 "-slice"
                                 "-take"
                                 "-drop"
                                 "-take-while"
                                 "--take-while"
                                 "-drop-while"
                                 "--drop-while"
                                 "-split-at"
                                 "-rotate"
                                 "-insert-at"
                                 "-replace-at"
                                 "-update-at"
                                 "--update-at"
                                 "-remove-at"
                                 "-remove-at-indices"
                                 "-split-with"
                                 "--split-with"
                                 "-split-on"
                                 "-split-when"
                                 "--split-when"
                                 "-separate"
                                 "--separate"
                                 "-partition-all-in-steps"
                                 "-partition-in-steps"
                                 "-partition-all"
                                 "-partition"
                                 "-partition-by"
                                 "--partition-by"
                                 "-partition-by-header"
                                 "--partition-by-header"
                                 "-group-by"
                                 "--group-by"
                                 "-interpose"
                                 "-interleave"
                                 "-zip-with"
                                 "--zip-with"
                                 "-zip"
                                 "-zip-fill"
                                 "-cycle"
                                 "-pad"
                                 "-annotate"
                                 "--annotate"
                                 "-table"
                                 "-table-flat"
                                 "-partial"
                                 "-elem-index"
                                 "-elem-indices"
                                 "-find-indices"
                                 "--find-indices"
                                 "-find-index"
                                 "--find-index"
                                 "-find-last-index"
                                 "--find-last-index"
                                 "-select-by-indices"
                                 "-select-columns"
                                 "-select-column"
                                 "-grade-up"
                                 "-grade-down"
                                 "->"
                                 "->>"
                                 "-->"
                                 "-when-let"
                                 "-when-let*"
                                 "--when-let"
                                 "-if-let"
                                 "-if-let*"
                                 "--if-let"
                                 "-let*"
                                 "-let"
                                 "-lambda"
                                 "-distinct"
                                 "-uniq"
                                 "-union"
                                 "-intersection"
                                 "-difference"
                                 "-contains?"
                                 "-contains-p"
                                 "-same-items?"
                                 "-same-items-p"
                                 "-is-prefix-p"
                                 "-is-prefix?"
                                 "-is-suffix-p"
                                 "-is-suffix?"
                                 "-is-infix-p"
                                 "-is-infix?"
                                 "-sort"
                                 "--sort"
                                 "-list"
                                 "-repeat"
                                 "-sum"
                                 "-product"
                                 "-max"
                                 "-min"
                                 "-max-by"
                                 "--max-by"
                                 "-min-by"
                                 "--min-by"
                                 "-iterate"
                                 "--iterate"
                                 "-fix"
                                 "--fix"
                                 "-unfold"
                                 "--unfold"
                                 "-cons-pair?"
                                 "-cons-to-list"
                                 "-value-to-list"
                                 "-tree-mapreduce-from"
                                 "--tree-mapreduce-from"
                                 "-tree-mapreduce"
                                 "--tree-mapreduce"
                                 "-tree-map"
                                 "--tree-map"
                                 "-tree-reduce-from"
                                 "--tree-reduce-from"
                                 "-tree-reduce"
                                 "--tree-reduce"
                                 "-tree-seq"
                                 "--tree-seq"
                                 "-tree-map-nodes"
                                 "--tree-map-nodes"
                                 "-clone"
                                 "-rpartial"
                                 "-juxt"
                                 "-applify"
                                 "-on"
                                 "-flip"
                                 "-const"
                                 "-cut"
                                 "-orfn"
                                 "-andfn"
                                 "-iteratefn"
                                 "-fixfn"
                                 "-prodfn"
                                 ))
                 (special-variables '(
                                      "it"
                                      "it-index"
                                      "acc"
                                      "other"
                                      )))
             (font-lock-add-keywords 'emacs-lisp-mode `((,(concat "\\_<" (regexp-opt special-variables 'paren) "\\_>")
                                                         1 font-lock-variable-name-face)) 'append)
             (font-lock-add-keywords 'emacs-lisp-mode `((,(concat "(\\s-*" (regexp-opt new-keywords 'paren) "\\_>")
                                                         1 font-lock-keyword-face)) 'append))
           (--each (buffer-list)
             (with-current-buffer it
               (when (and (eq major-mode 'emacs-lisp-mode)
                          (boundp 'font-lock-mode)
                          font-lock-mode)
                 (font-lock-refresh-defaults)))))))

    (provide 'dash)
    ;;; dash.el ends here

; ---------------------------------- FLYCHECK SETTINGS ------------------------------------- ;

    ;;; flycheck.el --- On-the-fly syntax checking -*- lexical-binding: t; -*-

    ;; Copyright (C) 2017 Flycheck contributors
    ;; Copyright (C) 2012-2016 Sebastian Wiesner and Flycheck contributors
    ;; Copyright (C) 2013, 2014 Free Software Foundation, Inc.
    ;;
    ;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
    ;; Maintainer: Clément Pit-Claudel <clement.pitclaudel@live.com>
    ;;             fmdkdd <fmdkdd@gmail.com>
    ;; URL: http://www.flycheck.org
    ;; Keywords: convenience, languages, tools
    ;; Version: 31-cvs
    ;; Package-Requires: ((dash "2.12.1") (pkg-info "0.4") (let-alist "1.0.4") (seq "1.11") (emacs "24.3"))

    ;; This file is not part of GNU Emacs.

    ;; This program is free software: you can redistribute it and/or modify
    ;; it under the terms of the GNU General Public License as published by
    ;; the Free Software Foundation, either version 3 of the License, or
    ;; (at your option) any later version.

    ;; This program is distributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ;; GNU General Public License for more details.

    ;; You should have received a copy of the GNU General Public License
    ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

    ;;; Commentary:

    ;; On-the-fly syntax checking for GNU Emacs 24.
    ;;
    ;; Flycheck is a modern on-the-fly syntax checking extension for GNU Emacs,
    ;; intended as replacement for the older Flymake extension which is part of GNU
    ;; Emacs.
    ;;
    ;; Flycheck automatically checks buffers for errors while you type, and reports
    ;; warnings and errors directly in the buffer and in an optional IDE-like error
    ;; list.
    ;;
    ;; It comes with a rich interface for custom syntax checkers and other
    ;; extensions, and has already many 3rd party extensions adding new features.
    ;;
    ;; Please read the online manual at http://www.flycheck.org for more
    ;; information.  You can open the manual directly from Emacs with `M-x
    ;; flycheck-manual'.
    ;;
    ;; # Setup
    ;;
    ;; Flycheck works best on Unix systems.  It does not officially support Windows,
    ;; but tries to maintain Windows compatibility and should generally work fine on
    ;; Windows, too.
    ;;
    ;; To enable Flycheck add the following to your init file:
    ;;
    ;;    (add-hook 'after-init-hook #'global-flycheck-mode)
    ;;
    ;; Flycheck will then automatically check buffers in supported languages, as
    ;; long as all necessary tools are present.  Use `flycheck-verify-setup' to
    ;; troubleshoot your Flycheck setup.

    ;;; Code:

    (eval-when-compile
      (require 'let-alist)      ; `let-alist'
      (require 'compile)        ; Compile Mode integration
      (require 'jka-compr)      ; To inhibit compression of temp files
      (require 'pcase)          ; `pcase-dolist' (`pcase' itself is autoloaded)
      )

    (require 'dash)

    (require 'seq)                   ; Sequence functions
    (require 'subr-x nil 'no-error)  ; Additional utilities, Emacs 24.4 and upwards
    (require 'cl-lib)                ; `cl-defstruct' and CL utilities
    (require 'tabulated-list)        ; To list errors
    (require 'easymenu)              ; Flycheck Mode menu definition
    (require 'rx)                    ; Regexp fanciness in `flycheck-define-checker'
    (require 'help-mode)             ; `define-button-type'
    (require 'find-func)             ; `find-function-regexp-alist'
    (require 'json)                  ; `flycheck-parse-tslint'


    ;; Declare a bunch of dynamic variables that we need from other modes
    (defvar sh-shell)                       ; For shell script checker predicates
    (defvar ess-language)                   ; For r-lintr predicate

    ;; Tell the byte compiler about autoloaded functions from packages
    (declare-function pkg-info-version-info "pkg-info" (package))

    ;;; Compatibility
    (eval-and-compile
      (unless (fboundp 'string-suffix-p)
        ;; TODO: Remove when dropping support for Emacs 24.3 and earlier
        (defun string-suffix-p (suffix string &optional ignore-case)
          "Return non-nil if SUFFIX is a suffix of STRING.
    If IGNORE-CASE is non-nil, the comparison is done without paying
    attention to case differences."
          (let ((start-pos (- (length string) (length suffix))))
            (and (>= start-pos 0)
                 (eq t (compare-strings suffix nil nil
                                        string start-pos nil ignore-case))))))

      ;; TODO: Remove when dropping support for Emacs 24.3 and earlier
      (unless (featurep 'subr-x)
        ;; `subr-x' function for Emacs 24.3 and below
        (defsubst string-join (strings &optional separator)
          "Join all STRINGS using SEPARATOR."
          (mapconcat 'identity strings separator))

        (defsubst string-trim-left (string)
          "Remove leading whitespace from STRING."
          (if (string-match "\\`[ \t\n\r]+" string)
              (replace-match "" t t string)
            string))

        (defsubst string-trim-right (string)
          "Remove trailing whitespace from STRING."
          (if (string-match "[ \t\n\r]+\\'" string)
              (replace-match "" t t string)
            string))

        (defsubst string-trim (string)
          "Remove leading and trailing whitespace from STRING."
          (string-trim-left (string-trim-right string)))

        (defsubst string-empty-p (string)
          "Check whether STRING is empty."
          (string= string ""))))


    ;;; Customization
    (defgroup flycheck nil
      "Modern on-the-fly syntax checking for GNU Emacs."
      :prefix "flycheck-"
      :group 'tools
      :link '(url-link :tag "Website" "http://www.flycheck.org")
      :link '(url-link :tag "Github" "https://github.com/flycheck/flycheck"))

    (defgroup flycheck-config-files nil
      "Configuration files for on-the-fly syntax checkers."
      :prefix "flycheck-"
      :group 'flycheck)

    (defgroup flycheck-options nil
      "Options for on-the-fly syntax checkers."
      :prefix "flycheck-"
      :group 'flycheck)

    (defgroup flycheck-executables nil
      "Executables of syntax checkers."
      :prefix "flycheck-"
      :group 'flycheck)

    (defgroup flycheck-faces nil
      "Faces used by on-the-fly syntax checking."
      :prefix "flycheck-"
      :group 'flycheck)

    (defcustom flycheck-checkers
      '(ada-gnat
        asciidoctor
        asciidoc
        c/c++-clang
        c/c++-gcc
        c/c++-cppcheck
        cfengine
        chef-foodcritic
        coffee
        coffee-coffeelint
        coq
        css-csslint
        css-stylelint
        d-dmd
        dockerfile-hadolint
        elixir-dogma
        emacs-lisp
        emacs-lisp-checkdoc
        erlang-rebar3
        erlang
        eruby-erubis
        fortran-gfortran
        go-gofmt
        go-golint
        go-vet
        go-build
        go-test
        go-errcheck
        go-unconvert
        go-megacheck
        groovy
        haml
        handlebars
        haskell-stack-ghc
        haskell-ghc
        haskell-hlint
        html-tidy
        javascript-eslint
        javascript-jshint
        javascript-jscs
        javascript-standard
        json-jsonlint
        json-python-json
        less
        less-stylelint
        llvm-llc
        lua-luacheck
        lua
        perl
        perl-perlcritic
        php
        php-phpmd
        php-phpcs
        processing
        proselint
        protobuf-protoc
        pug
        puppet-parser
        puppet-lint
        python-flake8
        python-pylint
        python-pycompile
        r-lintr
        racket
        rpm-rpmlint
        markdown-mdl
        nix
        rst-sphinx
        rst
        ruby-rubocop
        ruby-reek
        ruby-rubylint
        ruby
        ruby-jruby
        rust-cargo
        rust
        scala
        scala-scalastyle
        scheme-chicken
        scss-lint
        scss-stylelint
        sass/scss-sass-lint
        sass
        scss
        sh-bash
        sh-posix-dash
        sh-posix-bash
        sh-zsh
        sh-shellcheck
        slim
        slim-lint
        sql-sqlint
        systemd-analyze
        tex-chktex
        tex-lacheck
        texinfo
        typescript-tslint
        verilog-verilator
        xml-xmlstarlet
        xml-xmllint
        yaml-jsyaml
        yaml-ruby)
      "Syntax checkers available for automatic selection.

    A list of Flycheck syntax checkers to choose from when syntax
    checking a buffer.  Flycheck will automatically select a suitable
    syntax checker from this list, unless `flycheck-checker' is set,
    either directly or with `flycheck-select-checker'.

    You should not need to change this variable normally.  In order
    to disable syntax checkers, please use
    `flycheck-disabled-checkers'.  This variable is intended for 3rd
    party extensions to tell Flycheck about new syntax checkers.

    Syntax checkers in this list must be defined with
    `flycheck-define-checker'."
      :group 'flycheck
      :type '(repeat (symbol :tag "Checker"))
      :risky t)

    (defcustom flycheck-disabled-checkers nil
      "Syntax checkers excluded from automatic selection.

    A list of Flycheck syntax checkers to exclude from automatic
    selection.  Flycheck will never automatically select a syntax
    checker in this list, regardless of the value of
    `flycheck-checkers'.

    However, syntax checkers in this list are still available for
    manual selection with `flycheck-select-checker'.

    Use this variable to disable syntax checkers, instead of removing
    the syntax checkers from `flycheck-checkers'.  You may also use
    this option as a file or directory local variable to disable
    specific checkers in individual files and directories
    respectively."
      :group 'flycheck
      :type '(repeat (symbol :tag "Checker"))
      :package-version '(flycheck . "0.16")
      :safe #'flycheck-symbol-list-p)
    (make-variable-buffer-local 'flycheck-disabled-checkers)

    (defvar-local flycheck-checker nil
      "Syntax checker to use for the current buffer.

    If unset or nil, automatically select a suitable syntax checker
    from `flycheck-checkers' on every syntax check.

    If set to a syntax checker only use this syntax checker and never
    select one from `flycheck-checkers' automatically.  The syntax
    checker is used regardless of whether it is contained in
    `flycheck-checkers' or `flycheck-disabled-checkers'.  If the
    syntax checker is unusable in the current buffer an error is
    signaled.

    A syntax checker assigned to this variable must be defined with
    `flycheck-define-checker'.

    Use the command `flycheck-select-checker' to select a syntax
    checker for the current buffer, or set this variable as file
    local variable to always use a specific syntax checker for a
    file.  See Info Node `(emacs)Specifying File Variables' for more
    information about file variables.")
    (put 'flycheck-checker 'safe-local-variable 'flycheck-registered-checker-p)

    (defcustom flycheck-locate-config-file-functions nil
      "Functions to locate syntax checker configuration files.

    Each function in this hook must accept two arguments: The value
    of the configuration file variable, and the syntax checker
    symbol.  It must return either a string with an absolute path to
    the configuration file, or nil, if it cannot locate the
    configuration file.

    The functions in this hook are called in order of appearance, until a
    function returns non-nil.  The configuration file returned by that
    function is then given to the syntax checker if it exists.

    This variable is an abnormal hook.  See Info
    node `(elisp)Hooks'."
      :group 'flycheck
      :type 'hook
      :risky t)

    (defcustom flycheck-checker-error-threshold 400
      "Maximum errors allowed per syntax checker.

    The value of this variable is either an integer denoting the
    maximum number of errors per syntax checker and buffer, or nil to
    not limit the errors reported from a syntax checker.

    If this variable is a number and a syntax checker reports more
    errors than the value of this variable, its errors are not
    discarded, and not highlighted in the buffer or available in the
    error list.  The affected syntax checker is also disabled for
    future syntax checks of the buffer."
      :group 'flycheck
      :type '(choice (const :tag "Do not limit reported errors" nil)
                     (integer :tag "Maximum number of errors"))
      :risky t
      :package-version '(flycheck . "0.22"))

    (defcustom flycheck-process-error-functions nil
      "Functions to process errors.

    Each function in this hook must accept a single argument: A
    Flycheck error to process.

    All functions in this hook are called in order of appearance,
    until a function returns non-nil.  Thus, a function in this hook
    may return nil, to allow for further processing of the error, or
    any non-nil value, to indicate that the error was fully processed
    and inhibit any further processing.

    The functions are called for each newly parsed error immediately
    after the corresponding syntax checker finished.  At this stage,
    the overlays from the previous syntax checks are still present,
    and there may be further syntax checkers in the chain.

    This variable is an abnormal hook.  See Info
    node `(elisp)Hooks'."
      :group 'flycheck
      :type 'hook
      :package-version '(flycheck . "0.13")
      :risky t)

    (defcustom flycheck-display-errors-delay 0.9
      "Delay in seconds before displaying errors at point.

    Use floating point numbers to express fractions of seconds."
      :group 'flycheck
      :type 'number
      :package-version '(flycheck . "0.15")
      :safe #'numberp)

    (defcustom flycheck-display-errors-function #'flycheck-display-error-messages
      "Function to display error messages.

    If set to a function, call the function with the list of errors
    to display as single argument.  Each error is an instance of the
    `flycheck-error' struct.

    If set to nil, do not display errors at all."
      :group 'flycheck
      :type '(choice (const :tag "Display error messages"
                            flycheck-display-error-messages)
                     (const :tag "Display error messages only if no error list"
                            flycheck-display-error-messages-unless-error-list)
                     (function :tag "Error display function"))
      :package-version '(flycheck . "0.13")
      :risky t)

    (defcustom flycheck-help-echo-function #'flycheck-help-echo-all-error-messages
      "Function to compute the contents of the error tooltips.

    If set to a function, call the function with the list of errors
    to display as single argument.  Each error is an instance of the
    `flycheck-error' struct.  The function is used to set the
    help-echo property of flycheck error overlays.  It should return
    a string, which is displayed when the user hovers over an error
    or presses \\[display-local-help].

    If set to nil, do not show error tooltips."
      :group 'flycheck
      :type '(choice (const :tag "Concatenate error messages to form a tooltip"
                            flycheck-help-echo-all-error-messages)
                     (function :tag "Help echo function"))
      :package-version '(flycheck . "0.25")
      :risky t)

    (defcustom flycheck-command-wrapper-function #'identity
      "Function to modify checker commands before execution.

    The value of this option is a function which is given a list
    containing the full command of a syntax checker after
    substitution through `flycheck-substitute-argument' but before
    execution.  The function may return a new command for Flycheck to
    execute.

    The default value is `identity' which does not change the
    command.  You may provide your own function to run Flycheck
    commands through `bundle exec', `nix-shell' or similar wrappers."
      :group 'flycheck
      :type '(choice (const :tag "Do not modify commands" identity)
                     (function :tag "Modify command with a custom function"))
      :package-version '(flycheck . "0.25")
      :risky t)

    (defcustom flycheck-executable-find #'executable-find
      "Function to search for executables.

    The value of this option is a function which is given the name or
    path of an executable and shall return the full path to the
    executable, or nil if the executable does not exit.

    The default is the standard `executable-find' function which
    searches `exec-path'.  You can customize this option to search
    for checkers in other environments such as bundle or NixOS
    sandboxes."
      :group 'flycheck
      :type '(choice (const :tag "Search executables in `exec-path'" executable-find)
                     (function :tag "Search executables with a custom function"))
      :package-version '(flycheck . "0.25")
      :risky t)

    (defcustom flycheck-indication-mode 'left-fringe
      "The indication mode for Flycheck errors and warnings.

    This variable controls how Flycheck indicates errors in buffers.
    May either be `left-fringe', `right-fringe', or nil.

    If set to `left-fringe' or `right-fringe', indicate errors and
    warnings via icons in the left and right fringe respectively.

    If set to nil, do not indicate errors and warnings, but just
    highlight them according to `flycheck-highlighting-mode'."
      :group 'flycheck
      :type '(choice (const :tag "Indicate in the left fringe" left-fringe)
                     (const :tag "Indicate in the right fringe" right-fringe)
                     (const :tag "Do not indicate" nil))
      :safe #'symbolp)

    (defcustom flycheck-highlighting-mode 'symbols
      "The highlighting mode for Flycheck errors and warnings.

    The highlighting mode controls how Flycheck highlights errors in
    buffers.  The following modes are known:

    `columns'
         Highlight the error column.  If the error does not have a column,
         highlight the whole line.

    `symbols'
         Highlight the symbol at the error column, if there is any,
         otherwise behave like `columns'.  This is the default.

    `sexps'
         Highlight the expression at the error column, if there is
         any, otherwise behave like `columns'.  Note that this mode
         can be *very* slow in some major modes.

    `lines'
         Highlight the whole line.

    nil
         Do not highlight errors at all.  However, errors will still
         be reported in the mode line and in error message popups,
         and indicated according to `flycheck-indication-mode'."
      :group 'flycheck
      :type '(choice (const :tag "Highlight columns only" columns)
                     (const :tag "Highlight symbols" symbols)
                     (const :tag "Highlight expressions" sexps)
                     (const :tag "Highlight whole lines" lines)
                     (const :tag "Do not highlight errors" nil))
      :package-version '(flycheck . "0.14")
      :safe #'symbolp)

    (defcustom flycheck-check-syntax-automatically '(save
                                                     idle-change
                                                     new-line
                                                     mode-enabled)
      "When Flycheck should check syntax automatically.

    This variable is a list of events that may trigger syntax checks.
    The following events are known:

    `save'
         Check syntax immediately after the buffer was saved.

    `idle-change'
         Check syntax a short time (see `flycheck-idle-change-delay')
         after the last change to the buffer.

    `new-line'
         Check syntax immediately after a new line was inserted into
         the buffer.

    `mode-enabled'
         Check syntax immediately when variable `flycheck-mode' is
         non-nil.

    Flycheck performs a syntax checks only on events, which are
    contained in this list.  For instance, if the value of this
    variable is `(mode-enabled save)', Flycheck will only check if
    the mode is enabled or the buffer was saved, but never after
    changes to the buffer contents.

    If nil, never check syntax automatically.  In this case, use
    `flycheck-buffer' to start a syntax check manually."
      :group 'flycheck
      :type '(set (const :tag "After the buffer was saved" save)
                  (const :tag "After the buffer was changed and idle" idle-change)
                  (const :tag "After a new line was inserted" new-line)
                  (const :tag "After `flycheck-mode' was enabled" mode-enabled))
      :package-version '(flycheck . "0.12")
      :safe #'flycheck-symbol-list-p)

    (defcustom flycheck-idle-change-delay 0.5
      "How many seconds to wait before checking syntax automatically.

    After the buffer was changed, Flycheck will wait as many seconds
    as the value of this variable before starting a syntax check.  If
    the buffer is modified during this time, Flycheck will wait
    again.

    This variable has no effect, if `idle-change' is not contained in
    `flycheck-check-syntax-automatically'."
      :group 'flycheck
      :type 'number
      :package-version '(flycheck . "0.13")
      :safe #'numberp)

    (defcustom flycheck-standard-error-navigation t
      "Whether to support error navigation with `next-error'.

    If non-nil, enable navigation of Flycheck errors with
    `next-error', `previous-error' and `first-error'.  Otherwise,
    these functions just navigate errors from compilation modes.

    Flycheck error navigation with `flycheck-next-error',
    `flycheck-previous-error' and `flycheck-first-error' is always
    enabled, regardless of the value of this variable.

    Note that this setting only takes effect when variable
    `flycheck-mode' is non-nil.  Changing it will not affect buffers
    where variable `flycheck-mode' is already non-nil."
      :group 'flycheck
      :type 'boolean
      :package-version '(flycheck . "0.15")
      :safe #'booleanp)

    (define-widget 'flycheck-minimum-level 'lazy
      "A radio-type choice of minimum error levels.

    See `flycheck-navigation-minimum-level' and
    `flycheck-error-list-minimum-level'."
      :type '(radio (const :tag "All locations" nil)
                    (const :tag "Informational messages" info)
                    (const :tag "Warnings" warning)
                    (const :tag "Errors" error)
                    (symbol :tag "Custom error level")))

    (defcustom flycheck-navigation-minimum-level nil
      "The minimum level of errors to navigate.

    If set to an error level, only navigate errors whose error level
    is at least as severe as this one.  If nil, navigate all errors."
      :group 'flycheck
      :type 'flycheck-minimum-level
      :safe #'flycheck-error-level-p
      :package-version '(flycheck . "0.21"))

    (defcustom flycheck-error-list-minimum-level nil
      "The minimum level of errors to display in the error list.

    If set to an error level, only display errors whose error level
    is at least as severe as this one in the error list.  If nil,
    display all errors.

    This is the default level, used when the error list is opened.
    You can temporarily change the level using
    \\[flycheck-error-list-set-filter], or reset it to this value
    using \\[flycheck-error-list-reset-filter]."
      :group 'flycheck
      :type 'flycheck-minimum-level
      :safe #'flycheck-error-level-p
      :package-version '(flycheck . "0.24"))

    (defcustom flycheck-completing-read-function #'completing-read
      "Function to read from minibuffer with completion.

    The function must be compatible to the built-in `completing-read'
    function."
      :group 'flycheck
      :type '(choice (const :tag "Default" completing-read)
                     (const :tag "IDO" ido-completing-read)
                     (function :tag "Custom function"))
      :risky t
      :package-version '(flycheck . "26"))

    (defcustom flycheck-temp-prefix "flycheck"
      "Prefix for temporary files created by Flycheck."
      :group 'flycheck
      :type 'string
      :package-version '(flycheck . "0.19")
      :risky t)

    (defcustom flycheck-mode-hook nil
      "Hooks to run after command `flycheck-mode' is toggled."
      :group 'flycheck
      :type 'hook
      :risky t)

    (defcustom flycheck-after-syntax-check-hook nil
      "Functions to run after each syntax check.

    This hook is run after a syntax check was finished.

    At this point, *all* chained checkers were run, and all errors
    were parsed, highlighted and reported.  The variable
    `flycheck-current-errors' contains all errors from all syntax
    checkers run during the syntax check, so you can apply any error
    analysis functions.

    Note that this hook does *not* run after each individual syntax
    checker in the syntax checker chain, but only after the *last
    checker*.

    This variable is a normal hook.  See Info node `(elisp)Hooks'."
      :group 'flycheck
      :type 'hook
      :risky t)

    (defcustom flycheck-before-syntax-check-hook nil
      "Functions to run before each syntax check.

    This hook is run right before a syntax check starts.

    Error information from the previous syntax check is *not*
    cleared before this hook runs.

    Note that this hook does *not* run before each individual syntax
    checker in the syntax checker chain, but only before the *first
    checker*.

    This variable is a normal hook.  See Info node `(elisp)Hooks'."
      :group 'flycheck
      :type 'hook
      :risky t)

    (defcustom flycheck-syntax-check-failed-hook nil
      "Functions to run if a syntax check failed.

    This hook is run whenever an error occurs during Flycheck's
    internal processing.  No information about the error is given to
    this hook.

    You should use this hook to conduct additional cleanup actions
    when Flycheck failed.

    This variable is a normal hook.  See Info node `(elisp)Hooks'."
      :group 'flycheck
      :type 'hook
      :risky t)

    (defcustom flycheck-status-changed-functions nil
      "Functions to run if the Flycheck status changed.

    This hook is run whenever the status of Flycheck changes.  Each
    hook function takes the status symbol as single argument, as
    given to `flycheck-report-status', which see.

    This variable is a abnormal hook.  See Info
    node `(elisp)Hooks'."
      :group 'flycheck
      :type 'hook
      :risky t
      :package-version '(flycheck . "0.20"))

    (defcustom flycheck-error-list-after-refresh-hook nil
      "Functions to run after the error list was refreshed.

    This hook is run whenever the error list is refreshed.

    This variable is a normal hook.  See Info node `(elisp)Hooks'."
      :group 'flycheck
      :type 'hook
      :risky t
      :package-version '(flycheck . "0.21"))

    (defface flycheck-error
      '((((supports :underline (:style wave)))
         :underline (:style wave :color "Red1"))
        (t
         :underline t :inherit error))
      "Flycheck face for errors."
      :package-version '(flycheck . "0.13")
      :group 'flycheck-faces)

    (defface flycheck-warning
      '((((supports :underline (:style wave)))
         :underline (:style wave :color "DarkOrange"))
        (t
         :underline t :inherit warning))
      "Flycheck face for warnings."
      :package-version '(flycheck . "0.13")
      :group 'flycheck-faces)

    (defface flycheck-info
      '((((supports :underline (:style wave)))
         :underline (:style wave :color "ForestGreen"))
        (t
         :underline t :inherit success))
      "Flycheck face for informational messages."
      :package-version '(flycheck . "0.15")
      :group 'flycheck-faces)

    (defface flycheck-fringe-error
      '((t :inherit error))
      "Flycheck face for fringe error indicators."
      :package-version '(flycheck . "0.13")
      :group 'flycheck-faces)

    (defface flycheck-fringe-warning
      '((t :inherit warning))
      "Flycheck face for fringe warning indicators."
      :package-version '(flycheck . "0.13")
      :group 'flycheck-faces)

    (defface flycheck-fringe-info
      ;; Semantically `success' is probably not the right face, but it looks nice as
      ;; a base face
      '((t :inherit success))
      "Flycheck face for fringe info indicators."
      :package-version '(flycheck . "0.15")
      :group 'flycheck-faces)

    (defface flycheck-error-list-error
      '((t :inherit error))
      "Flycheck face for error messages in the error list."
      :package-version '(flycheck . "0.16")
      :group 'flycheck-faces)

    (defface flycheck-error-list-warning
      '((t :inherit warning))
      "Flycheck face for warning messages in the error list."
      :package-version '(flycheck . "0.16")
      :group 'flycheck-faces)

    (defface flycheck-error-list-info
      '((t :inherit success))
      "Flycheck face for info messages in the error list."
      :package-version '(flycheck . "0.16")
      :group 'flycheck-faces)

    ;; The base faces for the following two faces are inspired by Compilation Mode
    (defface flycheck-error-list-line-number
      '((t :inherit font-lock-constant-face))
      "Face for line numbers in the error list."
      :group 'flycheck-faces
      :package-version '(flycheck . "0.16"))

    (defface flycheck-error-list-column-number
      '((t :inherit font-lock-constant-face))
      "Face for line numbers in the error list."
      :group 'flycheck-faces
      :package-version '(flycheck . "0.16"))

    (defface flycheck-error-list-id
      '((t :inherit font-lock-type-face))
      "Face for the error ID in the error list."
      :group 'flycheck-faces
      :package-version '(flycheck . "0.22"))

    (defface flycheck-error-list-id-with-explainer
      '((t :inherit flycheck-error-list-id
           :box (:style released-button)))
      "Face for the error ID in the error list, for errors that have an explainer."
      :group 'flycheck-faces
      :package-version '(flycheck . "30"))

    (defface flycheck-error-list-checker-name
      '((t :inherit font-lock-function-name-face))
      "Face for the syntax checker name in the error list."
      :group 'flycheck-faces
      :package-version '(flycheck . "0.21"))

    (defface flycheck-error-list-highlight
      '((t :inherit highlight))
      "Flycheck face to highlight errors in the error list."
      :package-version '(flycheck . "0.15")
      :group 'flycheck-faces)

    (defvar flycheck-command-map
      (let ((map (make-sparse-keymap)))
        (define-key map "c"         #'flycheck-buffer)
        (define-key map "C"         #'flycheck-clear)
        (define-key map (kbd "C-c") #'flycheck-compile)
        (define-key map "n"         #'flycheck-next-error)
        (define-key map "p"         #'flycheck-previous-error)
        (define-key map "l"         #'flycheck-list-errors)
        (define-key map (kbd "C-w") #'flycheck-copy-errors-as-kill)
        (define-key map "s"         #'flycheck-select-checker)
        (define-key map "?"         #'flycheck-describe-checker)
        (define-key map "h"         #'flycheck-display-error-at-point)
        (define-key map "e"         #'flycheck-explain-error-at-point)
        (define-key map "H"         #'display-local-help)
        (define-key map "i"         #'flycheck-manual)
        (define-key map "V"         #'flycheck-version)
        (define-key map "v"         #'flycheck-verify-setup)
        (define-key map "x"         #'flycheck-disable-checker)
        map)
      "Keymap of Flycheck interactive commands.")

    (defcustom flycheck-keymap-prefix (kbd "C-c !")
      "Prefix for key bindings of Flycheck.

    Changing this variable outside Customize does not have any
    effect.  To change the keymap prefix from Lisp, you need to
    explicitly re-define the prefix key:

        (define-key flycheck-mode-map flycheck-keymap-prefix nil)
        (setq flycheck-keymap-prefix (kbd \"C-c f\"))
        (define-key flycheck-mode-map flycheck-keymap-prefix
                    flycheck-command-map)

    Please note that Flycheck's manual documents the default
    keybindings.  Changing this variable is at your own risk."
      :group 'flycheck
      :package-version '(flycheck . "0.19")
      :type 'string
      :risky t
      :set
      (lambda (variable key)
        (when (and (boundp variable) (boundp 'flycheck-mode-map))
          (define-key flycheck-mode-map (symbol-value variable) nil)
          (define-key flycheck-mode-map key flycheck-command-map))
        (set-default variable key)))

    (defcustom flycheck-mode-line '(:eval (flycheck-mode-line-status-text))
      "Mode line lighter for Flycheck.

    The value of this variable is a mode line template as in
    `mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
    more information.  Note that it should contain a _single_ mode
    line construct only.

    Customize this variable to change how Flycheck reports its status
    in the mode line.  You may use `flycheck-mode-line-status-text'
    to obtain a human-readable status text, including an
    error/warning count.

    You may also assemble your own status text.  The current status
    of Flycheck is available in `flycheck-last-status-change'.  The
    errors in the current buffer are stored in
    `flycheck-current-errors', and the function
    `flycheck-count-errors' may be used to obtain the number of
    errors grouped by error level.

    Set this variable to nil to disable the mode line completely."
      :group 'flycheck
      :type 'sexp
      :risky t
      :package-version '(flycheck . "0.20"))

    (defcustom flycheck-mode-line-prefix "FlyC"
      "Base mode line lighter for Flycheck.

    This will have an effect only with the default
    `flycheck-mode-line'.

    If you've customized `flycheck-mode-line' then the customized
    function must be updated to use this variable."
      :group 'flycheck
      :type 'string
      :package-version '(flycheck . "26"))

    (defcustom flycheck-error-list-mode-line
      `(,(propertized-buffer-identification "%12b")
        " for buffer "
        (:eval (flycheck-error-list-propertized-source-name))
        (:eval (flycheck-error-list-mode-line-filter-indicator)))
      "Mode line construct for Flycheck error list.

    The value of this variable is a mode line template as in
    `mode-line-format', to be used as
    `mode-line-buffer-identification' in `flycheck-error-list-mode'.
    See Info Node `(elisp)Mode Line Format' for more information.

    Customize this variable to change how the error list appears in
    the mode line.  The default shows the name of the buffer and the
    name of the source buffer, i.e. the buffer whose errors are
    currently listed."
      :group 'flycheck
      :type 'sexp
      :risky t
      :package-version '(flycheck . "0.20"))

    (defcustom flycheck-global-modes t
      "Modes for which option `flycheck-mode' is turned on.

    If t, Flycheck Mode is turned on for all major modes.  If a list,
    Flycheck Mode is turned on for all `major-mode' symbols in that
    list.  If the `car' of the list is `not', Flycheck Mode is turned
    on for all `major-mode' symbols _not_ in that list.  If nil,
    Flycheck Mode is never turned on by command
    `global-flycheck-mode'.

    Note that Flycheck is never turned on for modes whose
    `mode-class' property is `special' (see Info node `(elisp)Major
    Mode Conventions'), regardless of the value of this option.

    Only has effect when variable `global-flycheck-mode' is non-nil."
      :group 'flycheck
      :type '(choice (const :tag "none" nil)
                     (const :tag "all" t)
                     (set :menu-tag "mode specific" :tag "modes"
                          :value (not)
                          (const :tag "Except" not)
                          (repeat :inline t (symbol :tag "mode"))))
      :risky t
      :package-version '(flycheck . "0.23"))

    ;; Add built-in functions to our hooks, via `add-hook', to make sure that our
    ;; functions are really present, even if the variable was implicitly defined by
    ;; another call to `add-hook' that occurred before Flycheck was loaded.  See
    ;; http://lists.gnu.org/archive/html/emacs-devel/2015-02/msg01271.html for why
    ;; we don't initialize the hook variables right away.  We append our own
    ;; functions, because a user likely expects that their functions come first,
    ;; even if the added them before Flycheck was loaded.
    (dolist (hook (list #'flycheck-locate-config-file-by-path
                        #'flycheck-locate-config-file-ancestor-directories
                        #'flycheck-locate-config-file-home))
      (add-hook 'flycheck-locate-config-file-functions hook 'append))

    (add-hook 'flycheck-process-error-functions #'flycheck-add-overlay 'append)


    ;;; Global Flycheck menu
    (defvar flycheck-mode-menu-map
      (easy-menu-create-menu
       "Syntax Checking"
       '(["Enable on-the-fly syntax checking" flycheck-mode
          :style toggle :selected flycheck-mode
          :enable (or flycheck-mode
                      ;; Don't let users toggle the mode if there is no syntax
                      ;; checker for this buffer
                      (seq-find #'flycheck-checker-supports-major-mode-p
                                flycheck-checkers))]
         ["Check current buffer" flycheck-buffer flycheck-mode]
         ["Clear errors in buffer" flycheck-clear t]
         "---"
         ["Go to next error" flycheck-next-error flycheck-mode]
         ["Go to previous error" flycheck-previous-error flycheck-mode]
         ["Show all errors" flycheck-list-errors flycheck-mode]
         "---"
         ["Copy messages at point" flycheck-copy-errors-as-kill
          (flycheck-overlays-at (point))]
         ["Explain error at point" flycheck-explain-error-at-point]
         "---"
         ["Select syntax checker" flycheck-select-checker flycheck-mode]
         ["Disable syntax checker" flycheck-disable-checker flycheck-mode]
         ["Set executable of syntax checker" flycheck-set-checker-executable
          flycheck-mode]
         "---"
         ["Describe syntax checker" flycheck-describe-checker t]
         ["Show Flycheck version" flycheck-version t]
         ["Read the Flycheck manual" flycheck-info t]))
      "Menu of command `flycheck-mode'.")

    (easy-menu-add-item nil '("Tools") flycheck-mode-menu-map "Spell Checking")


    ;;; Version information, manual and loading of Flycheck
    (defun flycheck-version (&optional show-version)
      "Get the Flycheck version as string.

    If called interactively or if SHOW-VERSION is non-nil, show the
    version in the echo area and the messages buffer.

    The returned string includes both, the version from package.el
    and the library version, if both a present and different.

    If the version number could not be determined, signal an error,
    if called interactively, or if SHOW-VERSION is non-nil, otherwise
    just return nil."
      (interactive (list t))
      (let ((version (pkg-info-version-info 'flycheck)))
        (when show-version
          (message "Flycheck version: %s" version))
        version))

    (defun flycheck-unload-function ()
      "Unload function for Flycheck."
      (global-flycheck-mode -1)
      (easy-menu-remove-item nil '("Tools") (cadr flycheck-mode-menu-map))
      (remove-hook 'kill-emacs-hook #'flycheck-global-teardown)
      (setq find-function-regexp-alist
            (assq-delete-all 'flycheck-checker find-function-regexp-alist)))

    ;;;###autoload
    (defun flycheck-manual ()
      "Open the Flycheck manual."
      (interactive)
      (browse-url "http://www.flycheck.org"))

    (define-obsolete-function-alias 'flycheck-info
      'flycheck-manual "26" "Open the Flycheck manual.")


    ;;; Utility functions
    (defun flycheck-sexp-to-string (sexp)
      "Convert SEXP to a string.

    Like `prin1-to-string' but ensure that the returned string
    is loadable."
      (let ((print-quoted t)
            (print-length nil)
            (print-level nil))
        (prin1-to-string sexp)))

    (defun flycheck-string-to-number-safe (string)
      "Safely convert STRING to a number.

    If STRING is of string type and a numeric string, convert STRING
    to a number and return it.  Otherwise return nil."
      (let ((number-re (rx string-start (one-or-more (any digit)) string-end)))
        (when (and (stringp string) (string-match-p number-re string))
          (string-to-number string))))

    (defun flycheck-string-list-p (obj)
      "Determine if OBJ is a list of strings."
      (and (listp obj) (seq-every-p #'stringp obj)))

    (defun flycheck-symbol-list-p (obj)
      "Determine if OBJ is a list of symbols."
      (and (listp obj) (seq-every-p #'symbolp obj)))

    (defun flycheck-same-files-p (file-a file-b)
      "Determine whether FILE-A and FILE-B refer to the same file."
      (let ((file-a (expand-file-name file-a))
            (file-b (expand-file-name file-b)))
        ;; We must resolve symbolic links here, since some syntax checker always
        ;; output canonical file names with all symbolic links resolved.  However,
        ;; we still do a simple path compassion first, to avoid the comparatively
        ;; expensive file system call if possible.  See
        ;; https://github.com/flycheck/flycheck/issues/561
        (or (string= (directory-file-name file-a) (directory-file-name file-b))
            (string= (directory-file-name (file-truename file-a))
                     (directory-file-name (file-truename file-b))))))

    (defvar-local flycheck-temporaries nil
      "Temporary files and directories created by Flycheck.")

    (defun flycheck-temp-dir-system ()
      "Create a unique temporary directory.

    Use `flycheck-temp-prefix' as prefix, and add the directory to
    `flycheck-temporaries'.

    Return the path of the directory"
      (let* ((tempdir (make-temp-file flycheck-temp-prefix 'directory)))
        (push tempdir flycheck-temporaries)
        tempdir))

    (defun flycheck-temp-file-system (filename)
      "Create a temporary file named after FILENAME.

    If FILENAME is non-nil, this function creates a temporary
    directory with `flycheck-temp-dir-system', and creates a file
    with the same name as FILENAME in this directory.

    Otherwise this function creates a temporary file with
    `flycheck-temp-prefix' and a random suffix.  The path of the file
    is added to `flycheck-temporaries'.

    Add the path of the file to `flycheck-temporaries'.

    Return the path of the file."
      (let ((tempfile (convert-standard-filename
                       (if filename
                           (expand-file-name (file-name-nondirectory filename)
                                             (flycheck-temp-dir-system))
                         (make-temp-file flycheck-temp-prefix)))))
        (push tempfile flycheck-temporaries)
        tempfile))

    (defun flycheck-temp-file-inplace (filename)
      "Create an in-place copy of FILENAME.

    Prefix the file with `flycheck-temp-prefix' and add the path of
    the file to `flycheck-temporaries'.

    If FILENAME is nil, fall back to `flycheck-temp-file-system'.

    Return the path of the file."
      (if filename
          (let* ((tempname (format "%s_%s"
                                   flycheck-temp-prefix
                                   (file-name-nondirectory filename)))
                 (tempfile (convert-standard-filename
                            (expand-file-name tempname
                                              (file-name-directory filename)))))
            (push tempfile flycheck-temporaries)
            tempfile)
        (flycheck-temp-file-system filename)))

    (defun flycheck-save-buffer-to-file (file-name)
      "Save the contents of the current buffer to FILE-NAME."
      (make-directory (file-name-directory file-name) t)
      (let ((jka-compr-inhibit t))
        (write-region nil nil file-name nil 0)))

    (defun flycheck-save-buffer-to-temp (temp-file-fn)
      "Save buffer to temp file returned by TEMP-FILE-FN.

    Return the name of the temporary file."
      (let ((filename (funcall temp-file-fn (buffer-file-name))))
        ;; Do not flush short-lived temporary files onto disk
        (let ((write-region-inhibit-fsync t))
          (flycheck-save-buffer-to-file filename))
        filename))

    (defun flycheck-prepend-with-option (option items &optional prepend-fn)
      "Prepend OPTION to each item in ITEMS, using PREPEND-FN.

    Prepend OPTION to each item in ITEMS.

    ITEMS is a list of strings to pass to the syntax checker.  OPTION
    is the option, as string.  PREPEND-FN is a function called to
    prepend OPTION to each item in ITEMS.  It receives the option and
    a single item from ITEMS as argument, and must return a string or
    a list of strings with OPTION prepended to the item.  If
    PREPEND-FN is nil or omitted, use `list'.

    Return a list of strings where OPTION is prepended to each item
    in ITEMS using PREPEND-FN.  If PREPEND-FN returns a list, it is
    spliced into the resulting list."
      (unless (stringp option)
        (error "Option %S is not a string" option))
      (unless prepend-fn
        (setq prepend-fn #'list))
      (let ((prepend
             (lambda (item)
               (let ((result (funcall prepend-fn option item)))
                 (cond
                  ((and (listp result) (seq-every-p #'stringp result)) result)
                  ((stringp result) (list result))
                  (t (error "Invalid result type for option: %S" result)))))))
        (apply #'append (seq-map prepend items))))

    (defun flycheck-find-in-buffer (pattern)
      "Find PATTERN in the current buffer.

    Return the result of the first matching group of PATTERN, or nil,
    if PATTERN did not match."
      (save-restriction
        (widen)
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward pattern nil 'no-error)
            (match-string-no-properties 1)))))

    (defun flycheck-buffer-empty-p (&optional buffer)
      "Whether a BUFFER is empty.

    If buffer is nil or omitted check the current buffer.

    Return non-nil if so, or nil if the buffer has content."
      (<= (buffer-size buffer) 0))

    (defun flycheck-ephemeral-buffer-p ()
      "Determine whether the current buffer is an ephemeral buffer.

    See Info node `(elisp)Buffer Names' for information about
    ephemeral buffers."
      (string-prefix-p " " (buffer-name)))

    (defun flycheck-encrypted-buffer-p ()
      "Determine whether the current buffer is an encrypted file.

    See Info node `(epa)Top' for Emacs' interface to encrypted
    files."
      ;; The EPA file handler sets this variable locally to remember the recipients
      ;; of the encrypted file for re-encryption.  Hence, a local binding of this
      ;; variable is a good indication that the buffer is encrypted.  I haven't
      ;; found any better indicator anyway.
      (local-variable-p 'epa-file-encrypt-to))

    (defun flycheck-autoloads-file-p ()
      "Determine whether the current buffer is a autoloads file.

    Autoloads are generated by package.el during installation."
      (string-suffix-p "-autoloads.el" (buffer-name)))

    (defun flycheck-in-user-emacs-directory-p (filename)
      "Whether FILENAME is in `user-emacs-directory'."
      (string-prefix-p (file-name-as-directory (file-truename user-emacs-directory))
                       (file-truename filename)))

    (defun flycheck-safe-delete (file-or-dir)
      "Safely delete FILE-OR-DIR."
      (ignore-errors
        (if (file-directory-p file-or-dir)
            (delete-directory file-or-dir 'recursive)
          (delete-file file-or-dir))))

    (defun flycheck-safe-delete-temporaries ()
      "Safely delete all temp files and directories of Flycheck.

    Safely delete all files and directories listed in
    `flycheck-temporaries' and set the variable's value to nil."
      (seq-do #'flycheck-safe-delete flycheck-temporaries)
      (setq flycheck-temporaries nil))

    (defun flycheck-rx-file-name (form)
      "Translate the `(file-name)' FORM into a regular expression."
      (let ((body (or (cdr form) '((minimal-match
                                    (one-or-more not-newline))))))
        (rx-submatch-n `(group-n 1 ,@body))))

    (defun flycheck-rx-message (form)
      "Translate the `(message)' FORM into a regular expression."
      (let ((body (or (cdr form) '((one-or-more not-newline)))))
        (rx-submatch-n `(group-n 4 ,@body))))

    (defun flycheck-rx-id (form)
      "Translate the `(id)' FORM into a regular expression."
      (rx-submatch-n `(group-n 5 ,@(cdr form))))

    (defun flycheck-rx-to-string (form &optional no-group)
      "Like `rx-to-string' for FORM, but with special keywords:

    `line'
         matches the line number.

    `column'
         matches the column number.

    `(file-name SEXP ...)'
         matches the file name.  SEXP describes the file name.  If no
         SEXP is given, use a default body of `(minimal-match
         (one-or-more not-newline))'.

    `(message SEXP ...)'
         matches the message.  SEXP constitutes the body of the
         message.  If no SEXP is given, use a default body
         of `(one-or-more not-newline)'.

    `(id SEXP ...)'
         matches an error ID.  SEXP describes the ID.

    NO-GROUP is passed to `rx-to-string'.

    See `rx' for a complete list of all built-in `rx' forms."
      (let ((rx-constituents
             (append
              `((line . ,(rx (group-n 2 (one-or-more digit))))
                (column . ,(rx (group-n 3 (one-or-more digit))))
                (file-name flycheck-rx-file-name 0 nil)
                (message flycheck-rx-message 0 nil)
                (id flycheck-rx-id 0 nil))
              rx-constituents nil)))
        (rx-to-string form no-group)))

    (defun flycheck-current-load-file ()
      "Get the source file currently being loaded.

    Always return the name of the corresponding source file, never
    any byte-compiled file.

    Return nil, if the currently loaded file cannot be determined."
      (-when-let* ((this-file (cond
                               (load-in-progress load-file-name)
                               ((bound-and-true-p byte-compile-current-file))
                               (t (buffer-file-name))))
                   ;; A best guess for the source file of a compiled library. Works
                   ;; well in most cases, and especially for ELPA packages
                   (source-file (concat (file-name-sans-extension this-file)
                                        ".el")))
        (when (file-exists-p source-file)
          source-file)))

    (defun flycheck-module-root-directory (module &optional file-name)
      "Get the root directory for a MODULE in FILE-NAME.

    MODULE is a qualified module name, either a string with
    components separated by a dot, or as list of components.
    FILE-NAME is the name of the file or directory containing the
    module as string.  When nil or omitted, defaults to the return
    value of function `buffer-file-name'.

    Return the root directory of the module, that is, the directory,
    from which FILE-NAME can be reached by descending directories
    along each part of MODULE.

    If the MODULE name does not match the directory hierarchy upwards
    from FILE-NAME, return the directory containing FILE-NAME.  When
    FILE-NAME is nil, return `default-directory'."
      (let ((file-name (or file-name (buffer-file-name)))
            (module-components (if (stringp module)
                                   (split-string module (rx "."))
                                 (copy-sequence module))))
        (if (and module-components file-name)
            (let ((parts (nreverse module-components))
                  (base-directory (directory-file-name
                                   (file-name-sans-extension file-name))))
              (while (and parts
                          (string= (file-name-nondirectory base-directory)
                                   (car parts)))
                (pop parts)
                (setq base-directory (directory-file-name
                                      (file-name-directory base-directory))))
              (file-name-as-directory base-directory))
          (if file-name
              (file-name-directory file-name)
            (expand-file-name default-directory)))))


    ;;; Minibuffer tools
    (defvar read-flycheck-checker-history nil
      "`completing-read' history of `read-flycheck-checker'.")

    (defun flycheck-completing-read (prompt candidates default &optional history)
      "Read a value from the minibuffer.

    Use `flycheck-completing-read-function' to read input from the
    minibuffer with completion.

    Show PROMPT and read one of CANDIDATES, defaulting to DEFAULT.
    HISTORY is passed to `flycheck-completing-read-function'."
      (funcall flycheck-completing-read-function
               prompt candidates nil 'require-match nil history default))

    (defun read-flycheck-checker (prompt &optional default property candidates)
      "Read a flycheck checker from minibuffer with PROMPT and DEFAULT.

    PROMPT is a string to show in the minibuffer as prompt.  It
    should end with a single space.  DEFAULT is a symbol denoting the
    default checker to use, if the user did not select any checker.
    PROPERTY is a symbol denoting a syntax checker property.  If
    non-nil, only complete syntax checkers which have a non-nil value
    for PROPERTY.  CANDIDATES is an optional list of all syntax
    checkers available for completion, defaulting to all defined
    checkers.  If given, PROPERTY is ignored.

    Return the checker as symbol, or DEFAULT if no checker was
    chosen.  If DEFAULT is nil and no checker was chosen, signal a
    `user-error' if the underlying completion system does not provide
    a default on its own."
      (when (and default (not (flycheck-valid-checker-p default)))
        (error "%S is no valid Flycheck checker" default))
      (let* ((candidates (seq-map #'symbol-name
                                  (or candidates
                                      (flycheck-defined-checkers property))))
             (default (and default (symbol-name default)))
             (input (flycheck-completing-read
                     prompt candidates default
                     'read-flycheck-checker-history)))
        (when (string-empty-p input)
          (unless default
            (user-error "No syntax checker selected"))
          (setq input default))
        (let ((checker (intern input)))
          (unless (flycheck-valid-checker-p checker)
            (error "%S is not a valid Flycheck syntax checker" checker))
          checker)))

    (defun read-flycheck-error-level (prompt)
      "Read an error level from the user with PROMPT.

    Only offers level for which errors currently exist, in addition
    to the default levels."
      (let* ((levels (seq-map #'flycheck-error-level
                              (flycheck-error-list-current-errors)))
             (levels-with-defaults (append '(info warning error) levels))
             (uniq-levels (seq-uniq levels-with-defaults))
             (level (flycheck-completing-read prompt uniq-levels nil)))
        (and (stringp level) (intern level))))


    ;;; Checker API
    (defun flycheck-defined-checkers (&optional property)
      "Find all defined syntax checkers, optionally with PROPERTY.

    PROPERTY is a symbol.  If given, only return syntax checkers with
    a non-nil value for PROPERTY.

    The returned list is sorted alphapetically by the symbol name of
    the syntax checkers."
      (let (defined-checkers)
        (mapatoms (lambda (symbol)
                    (when (and (flycheck-valid-checker-p symbol)
                               (or (null property)
                                   (flycheck-checker-get symbol property)))
                      (push symbol defined-checkers))))
        (sort defined-checkers #'string<)))

    (defun flycheck-registered-checker-p (checker)
      "Determine whether CHECKER is registered.

    A checker is registered if it is contained in
    `flycheck-checkers'."
      (and (flycheck-valid-checker-p checker)
           (memq checker flycheck-checkers)))

    (defun flycheck-disabled-checker-p (checker)
      "Determine whether CHECKER is disabled.

    A checker is disabled if it is contained in
    `flycheck-disabled-checkers'."
      (memq checker flycheck-disabled-checkers))


    ;;; Generic syntax checkers
    (defconst flycheck-generic-checker-version 2
      "The internal version of generic syntax checker declarations.

    Flycheck will not use syntax checkers whose generic version is
    less than this constant.")

    (defsubst flycheck--checker-property-name (property)
      "Return the SYMBOL property for checker PROPERTY."
      (intern (concat "flycheck-" (symbol-name property))))

    (defun flycheck-checker-get (checker property)
      "Get the value of CHECKER's PROPERTY."
      (get checker (flycheck--checker-property-name property)))

    (gv-define-setter flycheck-checker-get (value checker property)
      `(setf (get ,checker (flycheck--checker-property-name ,property)) ,value))

    (defun flycheck-validate-next-checker (next &optional strict)
      "Validate NEXT checker.

    With STRICT non-nil, also check whether the syntax checker and
    the error level in NEXT are valid.  Otherwise just check whether
    these are symbols.

    Signal an error if NEXT is not a valid entry for
    `:next-checkers'."
      (when (symbolp next)
        (setq next (cons t next)))
      (pcase next
        (`(,level . ,checker)
         (if strict
             (progn
               (unless (or (eq level t) (flycheck-error-level-p level))
                 (error "%S is not a valid Flycheck error level" level))
               (unless (flycheck-valid-checker-p checker)
                 (error "%s is not a valid Flycheck syntax checker" checker)))
           (unless (symbolp level)
             (error "Error level %S must be a symbol" level))
           (unless (symbolp checker)
             (error "Checker %S must be a symbol" checker))))
        (_ (error "%S must be a symbol or cons cell" next)))
      t)

    (defun flycheck-define-generic-checker (symbol docstring &rest properties)
      "Define SYMBOL as generic syntax checker.

    Any syntax checker defined with this macro is eligible for manual
    syntax checker selection with `flycheck-select-checker'.  To make
    the new syntax checker available for automatic selection, it must
    be registered in `flycheck-checkers'.

    DOCSTRING is the documentation of the syntax checker, for
    `flycheck-describe-checker'.  The following PROPERTIES constitute
    a generic syntax checker.  Unless otherwise noted, all properties
    are mandatory.

    `:start FUNCTION'
         A function to start the syntax checker.

         FUNCTION shall take two arguments and return a context
         object if the checker is started successfully.  Otherwise it
         shall signal an error.

         The first argument is the syntax checker being started.  The
         second is a callback function to report state changes to
         Flycheck.  The callback takes two arguments STATUS DATA,
         where STATUS is a symbol denoting the syntax checker status
         and DATA an optional argument with additional data for the
         status report.  See `flycheck-report-buffer-checker-status'
         for more information about STATUS and DATA.

         FUNCTION may be synchronous or asynchronous, i.e. it may
         call the given callback either immediately, or at some later
         point (e.g. from a process sentinel).

         A syntax checker _must_ call CALLBACK at least once with a
         STATUS that finishes the current syntax checker.  Otherwise
         Flycheck gets stuck at the current syntax check with this
         syntax checker.

         The context object returned by FUNCTION is passed to
         `:interrupt'.

    `:interrupt FUNCTION'
         A function to interrupt the syntax check.

         FUNCTION is called with the syntax checker and the context
         object returned by the `:start' function and shall try to
         interrupt the syntax check.  The context may be nil, if the
         syntax check is interrupted before actually started.
         FUNCTION should handle this situation.

         If it cannot interrupt the syntax check, it may either
         signal an error or silently ignore the attempt to interrupt
         the syntax checker, depending on the severity of the
         situation.

         If interrupting the syntax check failed, Flycheck will let
         the syntax check continue, but ignore any status reports.
         Notably, it won't highlight any errors reported by the
         syntax check in the buffer.

         This property is optional.  If omitted, Flycheck won't
         attempt to interrupt syntax checks wit this syntax checker,
         and simply ignore their results.

    `:print-doc FUNCTION'
         A function to print additional documentation into the Help
         buffer of this checker.

         FUNCTION is called when creating the Help buffer for the
         syntax checker, with the syntax checker as single argument,
         after printing the name of the syntax checker and its modes
         and predicate, but before printing DOCSTRING.  It may insert
         additional documentation into the current buffer.

         The call occurs within `with-help-window'.  Hence
         `standard-output' points to the current buffer, so you may
         use `princ' and friends to add content.  Also, the current
         buffer is put into Help mode afterwards, which automatically
         turns symbols into references, if possible.

         This property is optional.  If omitted, no additional
         documentation is printed for this syntax checker.

    :verify FUNCTION
         A function to verify the checker for the current buffer.

         FUNCTION is called with the syntax checker as single
         argument, and shall return a list of
         `flycheck-verification-result' objects indicating whether
         the syntax checker could be used in the current buffer, and
         highlighting potential setup problems.

         This property is optional.  If omitted, no additional
         verification occurs for this syntax checker.  It is however
         absolutely recommended that you add a `:verify' function to
         your syntax checker, because it will help users to spot
         potential setup problems.

    `:modes MODES'
         A major mode symbol or a list thereof, denoting major modes
         to use this syntax checker in.

         This syntax checker will only be used in buffers whose
         `major-mode' is contained in MODES.

         If `:predicate' is also given the syntax checker will only
         be used in buffers for which the `:predicate' returns
         non-nil.

    `:predicate FUNCTION'
         A function to determine whether to use the syntax checker in
         the current buffer.

         FUNCTION is called without arguments and shall return
         non-nil if this syntax checker shall be used to check the
         current buffer.  Otherwise it shall return nil.

         If this checker has a `:working-directory' FUNCTION is
         called with `default-directory' bound to the checker's
         working directory.

         FUNCTION is only called in matching major modes.

         This property is optional.

    `:enabled FUNCTION'
         A function to determine whether to use the syntax checker in
         the current buffer.

         This property behaves as `:predicate', except that it's only
         called the first time a syntax checker is to be used in a buffer.

         FUNCTION is called without arguments and shall return
         non-nil if this syntax checker shall be used to check the
         current buffer.  Otherwise it shall return nil.

         If FUNCTION returns a non-nil value the checker is put in a
         whitelist in `flycheck-enabled-checkers' to prevent further
         invocations of `:enabled'.  Otherwise it is disabled via
         `flycheck-disabled-checkers' to prevent any further use of
         it.

         If this checker has a `:working-directory' FUNCTION is
         called with `default-directory' bound to the checker's
         working directory.

         FUNCTION is only called in matching major modes.

         This property is optional.

    `:error-filter FUNCTION'
         A function to filter the errors returned by this checker.

         FUNCTION is called with the list of `flycheck-error' objects
         returned by the syntax checker and shall return another list
         of `flycheck-error' objects, which is considered the final
         result of this syntax checker.

         FUNCTION is free to add, remove or modify errors, whether in
         place or by copying.

         This property is optional.  The default filter is
         `identity'.

    `:error-explainer FUNCTION'
         A function to return an explanation text for errors
         generated by this checker.

         FUNCTION is called with a `flycheck-error' object and shall
         return an explanation message for this error as a string, or
         nil if there is no explanation for this error.

         This property is optional.

    `:next-checkers NEXT-CHECKERS'
         A list denoting syntax checkers to apply after this syntax
         checker, in what we call \"chaining\" of syntax checkers.

         Each ITEM is a cons cell `(LEVEL . CHECKER)'.  CHECKER is a
         syntax checker to run after this syntax checker.  LEVEL is
         an error level.  CHECKER will only be used if there are no
         current errors of at least LEVEL.  LEVEL may also be t, in
         which case CHECKER is used regardless of the current errors.

         ITEM may also be a syntax checker symbol, which is
         equivalent to `(t . ITEM)'.

         Flycheck tries all items in order of declaration, and uses
         the first whose LEVEL matches and whose CHECKER is
         registered and can be used for the current buffer.

         This feature is typically used to apply more than one syntax
         checker to a buffer.  For instance, you might first use a
         compiler to check a buffer for syntax and type errors, and
         then run a linting tool that checks for insecure code, or
         questionable style.

         This property is optional.  If omitted, it defaults to the
         nil, i.e. no other syntax checkers are applied after this
         syntax checker.

    `:working-directory FUNCTION'
         The value of `default-directory' when invoking `:start'.

         FUNCTION is a function taking the syntax checker as sole
         argument.  It shall return the absolute path to an existing
         directory to use as `default-directory' for `:start' or
         nil to fall back to the `default-directory' of the current
         buffer.

         This property is optional.  If omitted invoke `:start'
         from the `default-directory' of the buffer being checked.

    Signal an error, if any property has an invalid value."
      (declare (indent 1)
               (doc-string 2))
      (let ((start (plist-get properties :start))
            (interrupt (plist-get properties :interrupt))
            (print-doc (plist-get properties :print-doc))
            (modes (plist-get properties :modes))
            (predicate (plist-get properties :predicate))
            (verify (plist-get properties :verify))
            (enabled (plist-get properties :enabled))
            (filter (or (plist-get properties :error-filter) #'identity))
            (explainer (plist-get properties :error-explainer))
            (next-checkers (plist-get properties :next-checkers))
            (file (flycheck-current-load-file))
            (working-directory (plist-get properties :working-directory)))

        (unless (listp modes)
          (setq modes (list modes)))

        (unless (functionp start)
          (error ":start %S of syntax checker %s is not a function" start symbol))
        (unless (or (null interrupt) (functionp interrupt))
          (error ":interrupt %S of syntax checker %s is not a function"
                 interrupt symbol))
        (unless (or (null print-doc) (functionp print-doc))
          (error ":print-doc %S of syntax checker %s is not a function"
                 print-doc symbol))
        (unless (or (null verify) (functionp verify))
          (error ":verify %S of syntax checker %S is not a function"
                 verify symbol))
        (unless (or (null enabled) (functionp enabled))
          (error ":enabled %S of syntax checker %S is not a function"
                 enabled symbol))
        (unless modes
          (error "Missing :modes in syntax checker %s" symbol))
        (dolist (mode modes)
          (unless (symbolp mode)
            (error "Invalid :modes %s in syntax checker %s, %s must be a symbol"
                   modes symbol mode)))
        (unless (or (null predicate) (functionp predicate))
          (error ":predicate %S of syntax checker %s  is not a function"
                 predicate symbol))
        (unless (functionp filter)
          (error ":error-filter %S of syntax checker %s is not a function"
                 filter symbol))
        (unless (or (null explainer) (functionp explainer))
          (error ":error-explainer %S of syntax checker %S is not a function"
                 explainer symbol))
        (dolist (checker next-checkers)
          (flycheck-validate-next-checker checker))

        (let ((real-predicate
               (and predicate
                    (lambda ()
                      ;; Run predicate in the checker's default directory
                      (let ((default-directory
                              (flycheck-compute-working-directory symbol)))
                        (funcall predicate)))))
              (real-enabled
               (lambda ()
                 (if (flycheck-valid-checker-p symbol)
                     (or (null enabled)
                         ;; Run enabled in the checker's default directory
                         (let ((default-directory
                                 (flycheck-compute-working-directory symbol)))
                           (funcall enabled)))
                   (lwarn 'flycheck :warning "%S is no valid Flycheck syntax checker.
    Try to reinstall the package defining this syntax checker." symbol)
                   nil))))
          (pcase-dolist (`(,prop . ,value)
                         `((start             . ,start)
                           (interrupt         . ,interrupt)
                           (print-doc         . ,print-doc)
                           (modes             . ,modes)
                           (predicate         . ,real-predicate)
                           (verify            . ,verify)
                           (enabled           . ,real-enabled)
                           (error-filter      . ,filter)
                           (error-explainer   . ,explainer)
                           (next-checkers     . ,next-checkers)
                           (documentation     . ,docstring)
                           (file              . ,file)
                           (working-directory . ,working-directory)))
            (setf (flycheck-checker-get symbol prop) value)))

        ;; Track the version, to avoid breakage if the internal format changes
        (setf (flycheck-checker-get symbol 'generic-checker-version)
              flycheck-generic-checker-version)))

    (defun flycheck-valid-checker-p (checker)
      "Check whether a CHECKER is valid.

    A valid checker is a symbol defined as syntax checker with
    `flycheck-define-checker'."
      (and (symbolp checker)
           (= (or (get checker 'flycheck-generic-checker-version) 0)
              flycheck-generic-checker-version)))

    (defun flycheck-checker-supports-major-mode-p (checker &optional mode)
      "Whether CHECKER supports the given major MODE.

    CHECKER is a syntax checker symbol and MODE a major mode symbol.
    Look at the `modes' property of CHECKER to determine whether
    CHECKER supports buffers in the given major MODE.

    MODE defaults to the value of `major-mode' if omitted or nil.

    Return non-nil if CHECKER supports MODE and nil otherwise."
      (let ((mode (or mode major-mode)))
        (memq mode (flycheck-checker-get checker 'modes))))

    (defvar-local flycheck-enabled-checkers nil
      "Syntax checkers included in automatic selection.

    A list of Flycheck syntax checkers included in automatic
    selection for current buffer.")

    (defun flycheck-may-enable-checker (checker)
      "Whether a generic CHECKER may be enabled for current buffer.

    Return non-nil if CHECKER may be used for the current buffer, and
    nil otherwise."
      (let* ((enabled (flycheck-checker-get checker 'enabled))
             (shall-enable (and (not (flycheck-disabled-checker-p checker))
                                (or (memq checker flycheck-enabled-checkers)
                                    (null enabled)
                                    (funcall enabled)))))
        (if shall-enable
            (cl-pushnew checker flycheck-enabled-checkers)
          (cl-pushnew checker flycheck-disabled-checkers))
        shall-enable))

    (defun flycheck-may-use-checker (checker)
      "Whether a generic CHECKER may be used.

    Return non-nil if CHECKER may be used for the current buffer, and
    nil otherwise."
      (let ((predicate (flycheck-checker-get checker 'predicate)))
        (and (flycheck-valid-checker-p checker)
             (flycheck-checker-supports-major-mode-p checker)
             (flycheck-may-enable-checker checker)
             (or (null predicate) (funcall predicate)))))

    (defun flycheck-may-use-next-checker (next-checker)
      "Determine whether NEXT-CHECKER may be used."
      (when (symbolp next-checker)
        (push t next-checker))
      (let ((level (car next-checker))
            (next-checker (cdr next-checker)))
        (and (or (eq level t)
                 (flycheck-has-max-current-errors-p level))
             (flycheck-registered-checker-p next-checker)
             (flycheck-may-use-checker next-checker))))


    ;;; Help for generic syntax checkers
    (define-button-type 'help-flycheck-checker-def
      :supertype 'help-xref
      'help-function #'flycheck-goto-checker-definition
      'help-echo "mouse-2, RET: find Flycheck checker definition")

    (defconst flycheck-find-checker-regexp
      (rx line-start (zero-or-more (syntax whitespace))
          "(" symbol-start "flycheck-define-checker" symbol-end
          (eval (list 'regexp find-function-space-re))
          symbol-start
          "%s"
          symbol-end
          (or (syntax whitespace) line-end))
      "Regular expression to find a checker definition.")

    (add-to-list 'find-function-regexp-alist
                 '(flycheck-checker . flycheck-find-checker-regexp))

    (defun flycheck-goto-checker-definition (checker file)
      "Go to to the definition of CHECKER in FILE."
      (let ((location (find-function-search-for-symbol
                       checker 'flycheck-checker file)))
        (pop-to-buffer (car location))
        (if (cdr location)
            (goto-char (cdr location))
          (message "Unable to find checker location in file"))))

    (defun flycheck-checker-at-point ()
      "Return the Flycheck checker found at or before point.

    Return nil if there is no checker."
      (let ((symbol (variable-at-point 'any-symbol)))
        (when (flycheck-valid-checker-p symbol)
          symbol)))

    (defun flycheck-describe-checker (checker)
      "Display the documentation of CHECKER.

    CHECKER is a checker symbol.

    Pop up a help buffer with the documentation of CHECKER."
      (interactive
       (let* ((enable-recursive-minibuffers t)
              (default (or (flycheck-checker-at-point)
                           (ignore-errors (flycheck-get-checker-for-buffer))))
              (prompt (if default
                          (format "Describe syntax checker (default %s): " default)
                        "Describe syntax checker: ")))
         (list (read-flycheck-checker prompt default))))
      (unless (flycheck-valid-checker-p checker)
        (user-error "You didn't specify a Flycheck syntax checker"))
      (help-setup-xref (list #'flycheck-describe-checker checker)
                       (called-interactively-p 'interactive))
      (save-excursion
        (with-help-window (help-buffer)
          (let ((filename (flycheck-checker-get checker 'file))
                (modes (flycheck-checker-get checker 'modes))
                (predicate (flycheck-checker-get checker 'predicate))
                (print-doc (flycheck-checker-get checker 'print-doc))
                (next-checkers (flycheck-checker-get checker 'next-checkers)))
            (princ (format "%s is a Flycheck syntax checker" checker))
            (when filename
              (princ (format " in `%s'" (file-name-nondirectory filename)))
              (with-current-buffer standard-output
                (save-excursion
                  (re-search-backward "`\\([^`']+\\)'" nil t)
                  (help-xref-button 1 'help-flycheck-checker-def checker filename))))
            (princ ".\n\n")

            (let ((modes-start (with-current-buffer standard-output (point-max))))
              ;; Track the start of the modes documentation, to properly re-fill
              ;; it later
              (princ "  This syntax checker checks syntax in the major mode(s) ")
              (princ (string-join
                      (seq-map (apply-partially #'format "`%s'") modes)
                      ", "))
              (when predicate
                (princ ", and uses a custom predicate"))
              (princ ".")
              (when next-checkers
                (princ "  It runs the following checkers afterwards:"))
              (with-current-buffer standard-output
                (save-excursion
                  (fill-region-as-paragraph modes-start (point-max))))
              (princ "\n")

              ;; Print the list of next checkers
              (when next-checkers
                (princ "\n")
                (let ((beg-checker-list (with-current-buffer standard-output
                                          (point))))
                  (dolist (next-checker next-checkers)
                    (if (symbolp next-checker)
                        (princ (format "     * `%s'\n" next-checker))
                      (princ (format "     * `%s' (maximum level `%s')\n"
                                     (cdr next-checker) (car next-checker)))))
                  ;;
                  (with-current-buffer standard-output
                    (save-excursion
                      (while (re-search-backward "`\\([^`']+\\)'"
                                                 beg-checker-list t)
                        (when (flycheck-valid-checker-p
                               (intern-soft (match-string 1)))
                          (help-xref-button 1 'help-flycheck-checker-def checker
                                            filename))))))))
            ;; Call the custom print-doc function of the checker, if present
            (when print-doc
              (funcall print-doc checker))
            ;; Ultimately, print the docstring
            (princ "\nDocumentation:\n")
            (princ (flycheck-checker-get checker 'documentation))))))


    ;;; Syntax checker verification
    (cl-defstruct (flycheck-verification-result
                   (:constructor flycheck-verification-result-new))
      "Structure for storing a single verification result.

    Slots:

    `label'
         A label for this result, as string

    `message'
         A message for this result, as string

    `face'
         The face to use for the `message'.

         You can either use a face symbol, or a list of face symbols."
      label message face)

    (defun flycheck-verify-generic-checker (checker)
      "Verify a generic CHECKER in the current buffer.

    Return a list of `flycheck-verification-result' objects."
      (let (results
            (predicate (flycheck-checker-get checker 'predicate))
            (enabled (flycheck-checker-get checker 'enabled))
            (verify (flycheck-checker-get checker 'verify)))
        (when enabled
          (let ((result (funcall enabled)))
            (push (flycheck-verification-result-new
                   :label "may enable"
                   :message (if result "yes" "Automatically disabled!")
                   :face (if result 'success '(bold warning)))
                  results)))
        (when predicate
          (let ((result (funcall predicate)))
            (push (flycheck-verification-result-new
                   :label "predicate"
                   :message (prin1-to-string (not (null result)))
                   :face (if result 'success '(bold warning)))
                  results)))
        (append (nreverse results)
                (and verify (funcall verify checker)))))

    (define-button-type 'help-flycheck-checker-doc
      :supertype 'help-xref
      'help-function #'flycheck-describe-checker
      'help-echo "mouse-2, RET: describe Flycheck checker")

    (defun flycheck--verify-princ-checker (checker buffer &optional with-mm)
      "Print verification result of CHECKER for BUFFER.

    When WITH-MM is given and non-nil, also include the major mode
    into the verification results."
      (princ "  ")
      (insert-button (symbol-name checker)
                     'type 'help-flycheck-checker-doc
                     'help-args (list checker))
      (when (with-current-buffer buffer (flycheck-disabled-checker-p checker))
        (insert (propertize " (disabled)" 'face '(bold error))))
      (princ "\n")
      (let ((results (with-current-buffer buffer
                       (flycheck-verify-generic-checker checker))))
        (when with-mm
          (with-current-buffer buffer
            (let ((message-and-face
                   (if (flycheck-checker-supports-major-mode-p checker)
                       (cons (format "`%s' supported" major-mode) 'success)
                     (cons (format "`%s' not supported" major-mode) 'error))))
              (push (flycheck-verification-result-new
                     :label "major mode"
                     :message (car message-and-face)
                     :face (cdr message-and-face))
                    results))))
        (let* ((label-length
                (seq-max (mapcar
                          (lambda (res)
                            (length (flycheck-verification-result-label res)))
                          results)))
               (message-column (+ 8 label-length)))
          (dolist (result results)
            (princ "    - ")
            (princ (flycheck-verification-result-label result))
            (princ ": ")
            (princ (make-string (- message-column (current-column)) ?\ ))
            (let ((message (flycheck-verification-result-message result))
                  (face (flycheck-verification-result-face result)))
              (insert (propertize message 'face face)))
            (princ "\n"))))
      (princ "\n"))

    (defun flycheck--verify-print-header (desc buffer)
      "Print a title with DESC for BUFFER in the current buffer.

    DESC is an arbitrary string containing a description, and BUFFER
    is the buffer being verified.  The name and the major mode mode
    of BUFFER are printed.

    DESC and information about BUFFER are printed in the current
    buffer."
      (princ desc)
      (insert (propertize (buffer-name buffer) 'face 'bold))
      (princ " in ")
      (let ((mode (buffer-local-value 'major-mode buffer)))
        (insert-button (symbol-name mode)
                       'type 'help-function
                       'help-args (list mode)))
      (princ ":\n\n"))

    (defun flycheck--verify-print-footer (buffer)
      "Print a footer for BUFFER in the current buffer.

    BUFFER is the buffer being verified."
      (princ "Flycheck Mode is ")
      (let ((enabled (buffer-local-value 'flycheck-mode buffer)))
        (insert (propertize (if enabled "enabled" "disabled")
                            'face (if enabled 'success '(warning bold)))))
      (princ
       (with-current-buffer buffer
         ;; Use key binding state in the verified buffer to print the help.
         (substitute-command-keys
          ".  Use \\[universal-argument] \\[flycheck-disable-checker] to enable disabled checkers.")))
      (save-excursion
        (let ((end (point)))
          (backward-paragraph)
          (fill-region-as-paragraph (point) end)))

      (princ "\n\n--------------------\n\n")
      (princ (format "Flycheck version: %s\n" (flycheck-version)))
      (princ (format "Emacs version:    %s\n" emacs-version))
      (princ (format "System:           %s\n" system-configuration))
      (princ (format "Window system:    %S\n" window-system)))

    (defun flycheck-verify-checker (checker)
      "Check whether a CHECKER can be used in this buffer.

    Show a buffer listing possible problems that prevent CHECKER from
    being used for the current buffer.

    Note: Do not use this function to check whether a syntax checker
    is applicable from Emacs Lisp code.  Use
    `flycheck-may-use-checker' instead."
      (interactive (list (read-flycheck-checker "Checker to verify: ")))
      (unless (flycheck-valid-checker-p checker)
        (user-error "%s is not a syntax checker" checker))

      ;; Save the buffer to make sure that all predicates are good
      (when (and (buffer-file-name) (buffer-modified-p))
        (save-buffer))

      (let ((buffer (current-buffer)))
        (with-help-window (get-buffer-create " *Flycheck checker*")
          (with-current-buffer standard-output
            (flycheck--verify-print-header "Syntax checker in buffer " buffer)
            (flycheck--verify-princ-checker checker buffer 'with-mm)
            (if (with-current-buffer buffer (flycheck-may-use-checker checker))
                (insert (propertize "Flycheck can use this syntax checker for this buffer.\n"
                                    'face 'success))
              (insert (propertize "Flycheck cannot use this syntax checker for this buffer.\n"
                                  'face 'error)))
            (insert "\n")
            (flycheck--verify-print-footer buffer)))))

    (defun flycheck-verify-setup ()
      "Check whether Flycheck can be used in this buffer.

    Display a new buffer listing all syntax checkers that could be
    applicable in the current buffer.  For each syntax checkers,
    possible problems are shown."
      (interactive)
      (when (and (buffer-file-name) (buffer-modified-p))
        ;; Save the buffer
        (save-buffer))

      (let ((buffer (current-buffer))
            ;; Get all checkers that support the current major mode
            (checkers (seq-filter #'flycheck-checker-supports-major-mode-p
                                  flycheck-checkers))
            (help-buffer (get-buffer-create " *Flycheck checkers*")))

        ;; Now print all applicable checkers
        (with-help-window help-buffer
          (with-current-buffer standard-output
            (flycheck--verify-print-header "Syntax checkers for buffer " buffer)
            (unless checkers
              (insert (propertize "There are no syntax checkers for this buffer!\n\n"
                                  'face '(bold error))))
            (dolist (checker checkers)
              (flycheck--verify-princ-checker checker buffer))

            (-when-let (selected-checker (buffer-local-value 'flycheck-checker buffer))
              (insert (propertize "The following checker is explicitly selected for this buffer:\n\n"
                                  'face 'bold))
              (flycheck--verify-princ-checker selected-checker buffer 'with-mm))

            (let ((unregistered-checkers (seq-difference (flycheck-defined-checkers)
                                                         flycheck-checkers)))
              (when unregistered-checkers
                (insert (propertize "\nThe following syntax checkers are not registered:\n\n"
                                    'face '(bold warning)))
                (dolist (checker unregistered-checkers)
                  (princ "  - ")
                  (princ checker)
                  (princ "\n"))
                (princ "\nTry adding these syntax checkers to `flycheck-checkers'.\n")))

            (flycheck--verify-print-footer buffer)))

        (with-current-buffer help-buffer
          (setq-local revert-buffer-function
                      (lambda (_ignore-auto _noconfirm)
                        (with-current-buffer buffer (flycheck-verify-setup)))))))


    ;;; Predicates for generic syntax checkers
    (defun flycheck-buffer-saved-p (&optional buffer)
      "Determine whether BUFFER is saved to a file.

    BUFFER is the buffer to check.  If omitted or nil, use the
    current buffer as BUFFER.

    Return non-nil if the BUFFER is backed by a file, and not
    modified, or nil otherwise."
      (let ((file-name (buffer-file-name buffer)))
        (and file-name (file-exists-p file-name) (not (buffer-modified-p buffer)))))


    ;;; Extending generic checkers
    (defun flycheck-add-next-checker (checker next &optional append)
      "After CHECKER add a NEXT checker.

    CHECKER is a syntax checker symbol, to which to add NEXT checker.

    NEXT is a cons cell `(LEVEL . NEXT-CHECKER)'.  NEXT-CHECKER is a
    symbol denoting the syntax checker to run after CHECKER.  LEVEL
    is an error level.  NEXT-CHECKER will only be used if there is no
    current error whose level is more severe than LEVEL.  LEVEL may
    also be t, in which case NEXT-CHECKER is used regardless of the
    current errors.

    NEXT can also be a syntax checker symbol only, which is
    equivalent to `(t . NEXT)'.

    NEXT-CHECKER is prepended before other next checkers, unless
    APPEND is non-nil."
      (unless (flycheck-valid-checker-p checker)
        (error "%s is not a valid syntax checker" checker))
      (flycheck-validate-next-checker next 'strict)
      (if append
          (setf (flycheck-checker-get checker 'next-checkers)
                (append (flycheck-checker-get checker 'next-checkers) (list next)))
        (push next (flycheck-checker-get checker 'next-checkers))))

    (defun flycheck-add-mode (checker mode)
      "To CHECKER add a new major MODE.

    CHECKER and MODE are symbols denoting a syntax checker and a
    major mode respectively.

    Add MODE to the `:modes' property of CHECKER, so that CHECKER
    will be used in buffers with MODE."
      (unless (flycheck-valid-checker-p checker)
        (error "%s is not a valid syntax checker" checker))
      (unless (symbolp mode)
        (error "%s is not a symbol" mode))
      (push mode (flycheck-checker-get checker 'modes)))


    ;;; Generic syntax checks
    (cl-defstruct (flycheck-syntax-check
                   (:constructor flycheck-syntax-check-new))
      "Structure for storing syntax check state.

    Slots:

    `buffer'
         The buffer being checked.

    `checker'
         The syntax checker being used.

    `context'
         The context object.

    `working-directory'
         Working directory for the syntax checker. Serve as a value for
         `default-directory' for a checker."
      buffer checker context working-directory)

    (defun flycheck-syntax-check-start (syntax-check callback)
      "Start a SYNTAX-CHECK with CALLBACK."
      (let ((checker (flycheck-syntax-check-checker syntax-check))
            (default-directory (flycheck-syntax-check-working-directory syntax-check)))
        (setf (flycheck-syntax-check-context syntax-check)
              (funcall (flycheck-checker-get checker 'start) checker callback))))

    (defun flycheck-syntax-check-interrupt (syntax-check)
      "Interrupt a SYNTAX-CHECK."
      (let* ((checker (flycheck-syntax-check-checker syntax-check))
             (interrupt-fn (flycheck-checker-get checker 'interrupt))
             (context (flycheck-syntax-check-context syntax-check)))
        (when interrupt-fn
          (funcall interrupt-fn checker context))))


    ;;; Syntax checking mode

    (defvar flycheck-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map flycheck-keymap-prefix flycheck-command-map)
        ;; We place the menu under a custom menu key.  Since this menu key is not
        ;; present in the menu of the global map, no top-level menu entry is added
        ;; to the global menu bar.  However, it still appears on the mode line
        ;; lighter.
        (define-key map [menu-bar flycheck] flycheck-mode-menu-map)
        map)
      "Keymap of command `flycheck-mode'.")

    (defvar-local flycheck-old-next-error-function nil
      "Remember the old `next-error-function'.")

    (defconst flycheck-hooks-alist
      '(
        ;; Handle events that may start automatic syntax checks
        (after-save-hook        . flycheck-handle-save)
        (after-change-functions . flycheck-handle-change)
        ;; Handle events that may triggered pending deferred checks
        (window-configuration-change-hook . flycheck-perform-deferred-syntax-check)
        (post-command-hook                . flycheck-perform-deferred-syntax-check)
        ;; Teardown Flycheck whenever the buffer state is about to get lost, to
        ;; clean up temporary files and directories.
        (kill-buffer-hook       . flycheck-teardown)
        (change-major-mode-hook . flycheck-teardown)
        (before-revert-hook     . flycheck-teardown)
        ;; Update the error list if necessary
        (post-command-hook . flycheck-error-list-update-source)
        (post-command-hook . flycheck-error-list-highlight-errors)
        ;; Display errors.  Show errors at point after commands (like movements) and
        ;; when Emacs gets focus.  Cancel the display timer when Emacs looses focus
        ;; (as there's no need to display errors if the user can't see them), and
        ;; hide the error buffer (for large error messages) if necessary.  Note that
        ;; the focus hooks only work on Emacs 24.4 and upwards, but since undefined
        ;; hooks are perfectly ok we don't need a version guard here.  They'll just
        ;; not work silently.
        (post-command-hook . flycheck-display-error-at-point-soon)
        (focus-in-hook     . flycheck-display-error-at-point-soon)
        (focus-out-hook    . flycheck-cancel-error-display-error-at-point-timer)
        (post-command-hook . flycheck-hide-error-buffer)
        ;; Immediately show error popups when navigating to an error
        (next-error-hook . flycheck-display-error-at-point))
      "Hooks which Flycheck needs to hook in.

    The `car' of each pair is a hook variable, the `cdr' a function
    to be added or removed from the hook variable if Flycheck mode is
    enabled and disabled respectively.")

    ;;;###autoload
    (define-minor-mode flycheck-mode
      "Minor mode for on-the-fly syntax checking.

    When called interactively, toggle `flycheck-mode'.  With prefix
    ARG, enable `flycheck-mode' if ARG is positive, otherwise disable
    it.

    When called from Lisp, enable `flycheck-mode' if ARG is omitted,
    nil or positive.  If ARG is `toggle', toggle `flycheck-mode'.
    Otherwise behave as if called interactively.

    In `flycheck-mode' the buffer is automatically syntax-checked
    using the first suitable syntax checker from `flycheck-checkers'.
    Use `flycheck-select-checker' to select a checker for the current
    buffer manually.

    \\{flycheck-mode-map}"
      :init-value nil
      :keymap flycheck-mode-map
      :lighter flycheck-mode-line
      :after-hook (flycheck-buffer-automatically 'mode-enabled 'force-deferred)
      (cond
       (flycheck-mode
        (flycheck-clear)

        (pcase-dolist (`(,hook . ,fn) flycheck-hooks-alist)
          (add-hook hook fn nil 'local))

        (setq flycheck-old-next-error-function (if flycheck-standard-error-navigation
                                                   next-error-function
                                                 :unset))
        (when flycheck-standard-error-navigation
          (setq next-error-function #'flycheck-next-error-function)))
       (t
        (unless (eq flycheck-old-next-error-function :unset)
          (setq next-error-function flycheck-old-next-error-function))

        (pcase-dolist (`(,hook . ,fn) flycheck-hooks-alist)
          (remove-hook hook fn 'local))

        (flycheck-teardown))))


    ;;; Syntax checker selection for the current buffer
    (defun flycheck-get-checker-for-buffer ()
      "Find the checker for the current buffer.

    Use the selected checker for the current buffer, if any,
    otherwise search for the best checker from `flycheck-checkers'.

    Return checker if there is a checker for the current buffer, or
    nil otherwise."
      (if flycheck-checker
          (if (flycheck-may-use-checker flycheck-checker)
              flycheck-checker
            (error "Flycheck cannot use %s in this buffer, type M-x flycheck-verify-setup for more details"
                   flycheck-checker))
        (seq-find #'flycheck-may-use-checker flycheck-checkers)))

    (defun flycheck-get-next-checker-for-buffer (checker)
      "Get the checker to run after CHECKER for the current buffer."
      (let ((next (seq-find #'flycheck-may-use-next-checker
                            (flycheck-checker-get checker 'next-checkers))))
        (when next
          (if (symbolp next) next (cdr next)))))

    (defun flycheck-select-checker (checker)
      "Select CHECKER for the current buffer.

    CHECKER is a syntax checker symbol (see `flycheck-checkers') or
    nil.  In the former case, use CHECKER for the current buffer,
    otherwise deselect the current syntax checker (if any) and use
    automatic checker selection via `flycheck-checkers'.

    If called interactively prompt for CHECKER.  With prefix arg
    deselect the current syntax checker and enable automatic
    selection again.

    Set `flycheck-checker' to CHECKER and automatically start a new
    syntax check if the syntax checker changed.

    CHECKER will be used, even if it is not contained in
    `flycheck-checkers', or if it is disabled via
    `flycheck-disabled-checkers'."
      (interactive
       (if current-prefix-arg
           (list nil)
         (list (read-flycheck-checker "Select checker: "
                                      (flycheck-get-checker-for-buffer)))))
      (when (not (eq checker flycheck-checker))
        (unless (or (not checker) (flycheck-may-use-checker checker))
          (flycheck-verify-checker checker)
          (user-error "Can't use syntax checker %S in this buffer" checker))
        (setq flycheck-checker checker)
        (when flycheck-mode
          (flycheck-buffer))))

    (defun flycheck-disable-checker (checker &optional enable)
      "Interactively disable CHECKER for the current buffer.

    Interactively, prompt for a syntax checker to disable, and add
    the syntax checker to the buffer-local value of
    `flycheck-disabled-checkers'.

    With non-nil ENABLE or with prefix arg, prompt for a disabled
    syntax checker and re-enable it by removing it from the
    buffer-local value of `flycheck-disabled-checkers'."
      (declare (interactive-only "Directly set `flycheck-disabled-checkers' instead"))
      (interactive
       (let* ((enable current-prefix-arg)
              (candidates (if enable flycheck-disabled-checkers flycheck-checkers))
              (prompt (if enable "Enable syntax checker: "
                        "Disable syntax checker: ")))
         (when (and enable (not candidates))
           (user-error "No syntax checkers disabled in this buffer"))
         (list (read-flycheck-checker prompt nil nil candidates) enable)))
      (unless checker
        (user-error "No syntax checker given"))
      (if enable
          ;; We must use `remq' instead of `delq', because we must _not_ modify the
          ;; list.  Otherwise we could potentially modify the global default value,
          ;; in case the list is the global default.
          (when (memq checker flycheck-disabled-checkers)
            (setq flycheck-disabled-checkers
                  (remq checker flycheck-disabled-checkers))
            (flycheck-buffer))
        (unless (memq checker flycheck-disabled-checkers)
          (push checker flycheck-disabled-checkers)
          (flycheck-buffer))))


    ;;; Syntax checks for the current buffer
    (defvar-local flycheck-current-syntax-check nil
      "The current syntax check in the this buffer.")
    (put 'flycheck-current-syntax-check 'permanent-local t)

    (defun flycheck-start-current-syntax-check (checker)
      "Start a syntax check in the current buffer with CHECKER.

    Set `flycheck-current-syntax-check' accordingly."
      ;; Allocate the current syntax check *before* starting it.  This allows for
      ;; synchronous checks, which call the status callback immediately in their
      ;; start function.
      (let* ((check (flycheck-syntax-check-new
                     :buffer (current-buffer)
                     :checker checker
                     :context nil
                     :working-directory (flycheck-compute-working-directory checker)))
             (callback (flycheck-buffer-status-callback check)))
        (setq flycheck-current-syntax-check check)
        (flycheck-report-status 'running)
        (flycheck-syntax-check-start check callback)))

    (defun flycheck-running-p ()
      "Determine whether a syntax check is running in the current buffer."
      (not (null flycheck-current-syntax-check)))

    (defun flycheck-stop ()
      "Stop any ongoing syntax check in the current buffer."
      (when (flycheck-running-p)
        (flycheck-syntax-check-interrupt flycheck-current-syntax-check)
        ;; Remove the current syntax check, to reset Flycheck into a non-running
        ;; state, and to make `flycheck-report-buffer-checker-status' ignore any
        ;; status reports from the current syntax check.
        (setq flycheck-current-syntax-check nil)
        (flycheck-report-status 'interrupted)))

    (defun flycheck-buffer-status-callback (syntax-check)
      "Create a status callback for SYNTAX-CHECK in the current buffer."
      (lambda (&rest args)
        (apply #'flycheck-report-buffer-checker-status
               syntax-check args)))

    (defun flycheck-buffer ()
      "Start checking syntax in the current buffer.

    Get a syntax checker for the current buffer with
    `flycheck-get-checker-for-buffer', and start it."
      (interactive)
      (flycheck-clean-deferred-check)
      (if flycheck-mode
          (unless (flycheck-running-p)
            ;; Clear error list and mark all overlays for deletion.  We do not
            ;; delete all overlays immediately to avoid excessive re-displays and
            ;; flickering, if the same errors gets highlighted again after the check
            ;; completed.
            (run-hooks 'flycheck-before-syntax-check-hook)
            (flycheck-clear-errors)
            (flycheck-mark-all-overlays-for-deletion)
            (condition-case err
                (let* ((checker (flycheck-get-checker-for-buffer)))
                  (if checker
                      (flycheck-start-current-syntax-check checker)
                    (flycheck-clear)
                    (flycheck-report-status 'no-checker)))
              (error
               (flycheck-report-failed-syntax-check)
               (signal (car err) (cdr err)))))
        (user-error "Flycheck mode disabled")))

    (defun flycheck-report-buffer-checker-status
        (syntax-check status &optional data)
      "In BUFFER, report a SYNTAX-CHECK STATUS with DATA.

    SYNTAX-CHECK is the `flycheck-syntax-check' which reported
    STATUS.  STATUS denotes the status of CHECKER, with an optional
    DATA.  STATUS may be one of the following symbols:

    `errored'
         The syntax checker has errored.  DATA is an optional error
         message.

         This report finishes the current syntax check.

    `interrupted'
         The syntax checker was interrupted.  DATA is ignored.

         This report finishes the current syntax check.

    `finished'
         The syntax checker has finished with a proper error report
         for the current buffer.  DATA is the (potentially empty)
         list of `flycheck-error' objects reported by the syntax
         check.

         This report finishes the current syntax check.

    `suspicious'
         The syntax checker encountered a suspicious state, which the
         user needs to be informed about.  DATA is an optional
         message.

    A syntax checker _must_ report a status at least once with any
    symbol that finishes the current syntax checker.  Otherwise
    Flycheck gets stuck with the current syntax check.

    If CHECKER is not the currently used syntax checker in
    `flycheck-current-syntax-check', the status report is largely
    ignored.  Notably, any errors reported by the checker are
    discarded."
      (let ((buffer (flycheck-syntax-check-buffer syntax-check)))
        ;; Ignore the status report if the buffer is gone, or if this syntax check
        ;; isn't the current one in buffer (which can happen if this is an old
        ;; report of an interrupted syntax check, and a new syntax check was started
        ;; since this check was interrupted)
        (when (and (buffer-live-p buffer)
                   (eq syntax-check
                       (buffer-local-value 'flycheck-current-syntax-check buffer)))
          (with-current-buffer buffer
            (let ((checker (flycheck-syntax-check-checker syntax-check)))
              (pcase status
                ((or `errored `interrupted)
                 (flycheck-report-failed-syntax-check status)
                 (when (eq status 'errored)
                   ;; In case of error, show the error message
                   (message "Error from syntax checker %s: %s"
                            checker (or data "UNKNOWN!"))))
                (`suspicious
                 (when flycheck-mode
                   (message "Suspicious state from syntax checker %s: %s"
                            checker (or data "UNKNOWN!")))
                 (flycheck-report-status 'suspicious))
                (`finished
                 (when flycheck-mode
                   ;; Only report errors from the checker if Flycheck Mode is
                   ;; still enabled.
                   (flycheck-finish-current-syntax-check
                    data
                    (flycheck-syntax-check-working-directory syntax-check))))
                (_
                 (error "Unknown status %s from syntax checker %s"
                        status checker))))))))

    (defun flycheck-finish-current-syntax-check (errors working-dir)
      "Finish the current syntax-check in the current buffer with ERRORS.

    ERRORS is a list of `flycheck-error' objects reported by the
    current syntax check in `flycheck-current-syntax-check'.

    Report all ERRORS and potentially start any next syntax checkers.

    If the current syntax checker reported excessive errors, it is
    disabled via `flycheck-disable-excessive-checker' for subsequent
    syntax checks.

    Relative file names in ERRORS will be expanded relative to
    WORKING-DIR."
      (let* ((syntax-check flycheck-current-syntax-check)
             (checker (flycheck-syntax-check-checker syntax-check))
             (errors (flycheck-relevant-errors
                      (flycheck-fill-and-expand-error-file-names
                       (flycheck-filter-errors
                        (flycheck-assert-error-list-p errors) checker)
                       working-dir))))
        (unless (flycheck-disable-excessive-checker checker errors)
          (flycheck-report-current-errors errors))
        (let ((next-checker (flycheck-get-next-checker-for-buffer checker)))
          (if next-checker
              (flycheck-start-current-syntax-check next-checker)
            (setq flycheck-current-syntax-check nil)
            (flycheck-report-status 'finished)
            ;; Delete overlays only after the very last checker has run, to avoid
            ;; flickering on intermediate re-displays
            (flycheck-delete-marked-overlays)
            (flycheck-error-list-refresh)
            (run-hooks 'flycheck-after-syntax-check-hook)
            (when (eq (current-buffer) (window-buffer))
              (flycheck-display-error-at-point))
            ;; Immediately try to run any pending deferred syntax check, which
            ;; were triggered by intermediate automatic check event, to make sure
            ;; that we quickly refine outdated error information
            (flycheck-perform-deferred-syntax-check)))))

    (defun flycheck-disable-excessive-checker (checker errors)
      "Disable CHECKER if it reported excessive ERRORS.

    If ERRORS has more items than `flycheck-checker-error-threshold',
    add CHECKER to `flycheck-disabled-checkers', and show a warning.

    Return t when CHECKER was disabled, or nil otherwise."
      (when (and flycheck-checker-error-threshold
                 (> (length errors) flycheck-checker-error-threshold))
        ;; Disable CHECKER for this buffer (`flycheck-disabled-checkers' is a local
        ;; variable).
        (lwarn '(flycheck syntax-checker) :warning
               "Syntax checker %s reported too many errors (%s) and is disabled."
               checker (length errors))
        (push checker flycheck-disabled-checkers)
        t))

    (defun flycheck-clear (&optional shall-interrupt)
      "Clear all errors in the current buffer.

    With prefix arg or SHALL-INTERRUPT non-nil, also interrupt the
    current syntax check."
      (interactive "P")
      (when shall-interrupt
        (flycheck-stop))
      (flycheck-delete-all-overlays)
      (flycheck-clear-errors)
      (flycheck-error-list-refresh)
      (flycheck-hide-error-buffer))

    (defun flycheck-teardown ()
      "Teardown Flycheck in the current buffer..

    Completely clear the whole Flycheck state.  Remove overlays, kill
    running checks, and empty all variables used by Flycheck."
      (flycheck-safe-delete-temporaries)
      (flycheck-stop)
      (flycheck-clean-deferred-check)
      (flycheck-clear)
      (flycheck-cancel-error-display-error-at-point-timer))


    ;;; Automatic syntax checking in a buffer
    (defun flycheck-may-check-automatically (&optional condition)
      "Determine whether the buffer may be checked under CONDITION.

    Read-only buffers may never be checked automatically.

    If CONDITION is non-nil, determine whether syntax may checked
    automatically according to
    `flycheck-check-syntax-automatically'."
      (and (not (or buffer-read-only (flycheck-ephemeral-buffer-p)))
           (file-exists-p default-directory)
           (or (not condition)
               (memq condition flycheck-check-syntax-automatically))))

    (defun flycheck-buffer-automatically (&optional condition force-deferred)
      "Automatically check syntax at CONDITION.

    Syntax is not checked if `flycheck-may-check-automatically'
    returns nil for CONDITION.

    The syntax check is deferred if FORCE-DEFERRED is non-nil, or if
    `flycheck-must-defer-check' returns t."
      (when (and flycheck-mode (flycheck-may-check-automatically condition))
        (if (or force-deferred (flycheck-must-defer-check))
            (flycheck-buffer-deferred)
          (with-demoted-errors "Error while checking syntax automatically: %S"
            (flycheck-buffer)))))

    (defvar-local flycheck-idle-change-timer nil
      "Timer to mark the idle time since the last change.")

    (defun flycheck-clear-idle-change-timer ()
      "Clear the idle change timer."
      (when flycheck-idle-change-timer
        (cancel-timer flycheck-idle-change-timer)
        (setq flycheck-idle-change-timer nil)))

    (defun flycheck-handle-change (beg end _len)
      "Handle a buffer change between BEG and END.

    BEG and END mark the beginning and end of the change text.  _LEN
    is ignored.

    Start a syntax check if a new line has been inserted into the
    buffer."
      ;; Save and restore the match data, as recommended in (elisp)Change Hooks
      (save-match-data
        (when flycheck-mode
          ;; The buffer was changed, thus clear the idle timer
          (flycheck-clear-idle-change-timer)
          (if (string-match-p (rx "\n") (buffer-substring beg end))
              (flycheck-buffer-automatically 'new-line 'force-deferred)
            (setq flycheck-idle-change-timer
                  (run-at-time flycheck-idle-change-delay nil
                               #'flycheck--handle-idle-change-in-buffer
                               (current-buffer)))))))

    (defun flycheck--handle-idle-change-in-buffer (buffer)
      "Handle an expired idle timer in BUFFER since the last change.
    This thin wrapper around `flycheck-handle-idle-change' is needed
    because some users override that function, as described in URL
    `https://github.com/flycheck/flycheck/pull/1305'."
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (flycheck-handle-idle-change))))

    (defun flycheck-handle-idle-change ()
      "Handle an expired idle timer since the last change."
      (flycheck-clear-idle-change-timer)
      (flycheck-buffer-automatically 'idle-change))

    (defun flycheck-handle-save ()
      "Handle a save of the buffer."
      (flycheck-buffer-automatically 'save))


    ;;; Deferred syntax checking
    (defvar-local flycheck-deferred-syntax-check nil
      "If non-nil, a deferred syntax check is pending.")

    (defun flycheck-must-defer-check ()
      "Determine whether the syntax check has to be deferred.

    A check has to be deferred if the buffer is not visible, or if the buffer is
    currently being reverted.

    Return t if the check is to be deferred, or nil otherwise."
      (or (not (get-buffer-window))
          ;; We defer the syntax check if Flycheck is already running, to
          ;; immediately start a new syntax check after the current one finished,
          ;; because the result of the current check will most likely be outdated by
          ;; the time it is finished.
          (flycheck-running-p)
          ;; We must defer checks while a buffer is being reverted, to avoid race
          ;; conditions while the buffer contents are being restored.
          revert-buffer-in-progress-p))

    (defun flycheck-deferred-check-p ()
      "Determine whether the current buffer has a deferred check.

    Return t if so, or nil otherwise."
      flycheck-deferred-syntax-check)

    (defun flycheck-buffer-deferred ()
      "Defer syntax check for the current buffer."
      (setq flycheck-deferred-syntax-check t))

    (defun flycheck-clean-deferred-check ()
      "Clean an deferred syntax checking state."
      (setq flycheck-deferred-syntax-check nil))

    (defun flycheck-perform-deferred-syntax-check ()
      "Perform the deferred syntax check."
      (when (flycheck-deferred-check-p)
        (flycheck-clean-deferred-check)
        (flycheck-buffer-automatically)))


    ;;; Syntax checking in all buffers
    (defun flycheck-may-enable-mode ()
      "Determine whether Flycheck mode may be enabled.

    Flycheck mode is not enabled for

    - the minibuffer,
    - `fundamental-mode'
    - major modes whose `mode-class' property is `special',
    - ephemeral buffers (see `flycheck-ephemeral-buffer-p'),
    - encrypted buffers (see `flycheck-encrypted-buffer-p'),
    - remote files (see `file-remote-p'),
    - and major modes excluded by `flycheck-global-modes'.

    Return non-nil if Flycheck mode may be enabled, and nil
    otherwise."
      (and (pcase flycheck-global-modes
             ;; Whether `major-mode' is disallowed by `flycheck-global-modes'
             (`t t)
             (`(not . ,modes) (not (memq major-mode modes)))
             (modes (memq major-mode modes)))
           (not (or (minibufferp)
                    (eq major-mode 'fundamental-mode)
                    (eq (get major-mode 'mode-class) 'special)
                    (flycheck-ephemeral-buffer-p)
                    (flycheck-encrypted-buffer-p)
                    (and (buffer-file-name)
                         (file-remote-p (buffer-file-name) 'method))))))

    (defun flycheck-mode-on-safe ()
      "Enable command `flycheck-mode' if it is safe to do so.

    Command `flycheck-mode' is only enabled if
    `flycheck-may-enable-mode' returns a non-nil result."
      (when (flycheck-may-enable-mode)
        (flycheck-mode)))

    ;;;###autoload
    (define-globalized-minor-mode global-flycheck-mode flycheck-mode
      flycheck-mode-on-safe
      :init-value nil
      ;; Do not expose Global Flycheck Mode on customize interface, because the
      ;; interaction between package.el and customize is currently broken.  See
      ;; https://github.com/flycheck/flycheck/issues/595

      ;; :require 'flycheck :group
      ;; 'flycheck
      )

    (defun flycheck-global-teardown ()
      "Teardown Flycheck in all buffers.

    Completely clear the whole Flycheck state in all buffers, stop
    all running checks, remove all temporary files, and empty all
    variables of Flycheck."
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when flycheck-mode
            (flycheck-teardown)))))

    ;; Clean up the entire state of Flycheck when Emacs is killed, to get rid of any
    ;; pending temporary files.
    (add-hook 'kill-emacs-hook #'flycheck-global-teardown)


    ;;; Errors from syntax checks
    (cl-defstruct (flycheck-error
                   (:constructor flycheck-error-new)
                   (:constructor flycheck-error-new-at (line column
                                                             &optional level message
                                                             &key checker id group
                                                             (filename (buffer-file-name))
                                                             (buffer (current-buffer)))))
      "Structure representing an error reported by a syntax checker.
    Slots:

    `buffer'
         The buffer that the error was reported for, as buffer object.

    `checker'
         The syntax checker which reported this error, as symbol.

    `filename'
         The file name the error refers to, as string.

    `line'
         The line number the error refers to, as number.

    `column' (optional)
         The column number the error refers to, as number.

         For compatibility with external tools and unlike Emacs
         itself (e.g. in Compile Mode) Flycheck uses _1-based_
         columns: The first character on a line is column 1.

         Occasionally some tools try to proactively adapt to Emacs
         and emit 0-based columns automatically.  In these cases, the
         columns must be adjusted for Flycheck, see
         `flycheck-increment-error-columns'.

    `message' (optional)
         The error message as a string, if any.

    `level'
         The error level, as either `info', `warning' or `error'.

    `id' (optional)
         An ID identifying the kind of error.

    `group` (optional)
         A symbol identifying the group the error belongs to.

         Some tools will emit multiple errors that relate to the same
         issue (e.g., lifetime errors in Rust).  All related errors
         collected by a checker should have the same `group` value,
         in order to be able to present them to the user.

         See `flycheck-errors-from-group`."
      buffer checker filename line column message level id group)

    (defmacro flycheck-error-with-buffer (err &rest forms)
      "Switch to the buffer of ERR and evaluate FORMS.

    If the buffer of ERR is not live, FORMS are not evaluated."
      (declare (indent 1) (debug t))
      `(when (buffer-live-p (flycheck-error-buffer ,err))
         (with-current-buffer (flycheck-error-buffer ,err)
           ,@forms)))

    (defun flycheck-error-line-region (err)
      "Get the line region of ERR.

    ERR is a Flycheck error whose region to get.

    Return a cons cell `(BEG . END)' where BEG is the first
    non-whitespace character on the line ERR refers to, and END the
    end of the line."
      (flycheck-error-with-buffer err
        (save-restriction
          (save-excursion
            (widen)
            (goto-char (point-min))
            (forward-line (- (flycheck-error-line err) 1))
            ;; We are at the beginning of the line now, so move to the beginning of
            ;; its indentation, similar to `back-to-indentation'
            (let ((end (line-end-position)))
              (skip-syntax-forward " " end)
              (backward-prefix-chars)
              ;; If the current line is empty, include the previous line break
              ;; character(s) to have any region at all.  When called with 0,
              ;; `line-end-position' gives us the end of the previous line
              (cons (if (eolp) (line-end-position 0) (point)) end))))))

    (defun flycheck-error-column-region (err)
      "Get the error column region of ERR.

    ERR is a Flycheck error whose region to get.

    Return a cons cell `(BEG . END)' where BEG is the character
    before the error column, and END the actual error column, or nil
    if ERR has no column."
      (flycheck-error-with-buffer err
        (save-restriction
          (save-excursion
            (-when-let (column (flycheck-error-column err))
              (widen)
              (goto-char (point-min))
              (forward-line (- (flycheck-error-line err) 1))
              (cond
               ((eobp)                    ; Line beyond EOF
                ;; If we are at the end of the file (i.e. the line was beyond the
                ;; end of the file), use the very last column in the file.
                (cons (- (point-max) 1) (point-max)))
               ((eolp)                    ; Empty line
                ;; If the target line is empty, there's no column to highlight on
                ;; this line, so return the last column of the previous line.
                (cons (line-end-position 0) (point)))
               (t
                ;; The end is either the column offset of the line, or the end of
                ;; the line, if the column offset points beyond the end of the
                ;; line.
                (let ((end (min (+ (point) column)
                                (+ (line-end-position) 1))))
                  (cons (- end 1) end)))))))))

    (defun flycheck-error-thing-region (thing err)
      "Get the region of THING at the column of ERR.

    ERR is a Flycheck error whose region to get.  THING is a
    understood by `thing-at-point'.

    Return a cons cell `(BEG . END)' where BEG is the beginning of
    the THING at the error column, and END the end of the symbol.  If
    ERR has no error column, or if there is no THING at this column,
    return nil."
      (-when-let (column (car (flycheck-error-column-region err)))
        (flycheck-error-with-buffer err
          (save-excursion
            (save-restriction
              (widen)
              (goto-char column)
              (bounds-of-thing-at-point thing))))))

    (defun flycheck-error-region-for-mode (err mode)
      "Get the region of ERR for the highlighting MODE.

    ERR is a Flycheck error.  MODE may be one of the following symbols:

    `columns'
         Get the column region of ERR, or the line region if ERR
         has no column.

    `symbols'
         Get the symbol region of ERR, or the result of `columns', if
         there is no sexp at the error column.

    `sexps'
         Get the sexp region of ERR, or the result of `columns', if
         there is no sexp at the error column.

    `lines'
         Return the line region.

    Otherwise signal an error."
      ;; Ignoring fields speeds up calls to `line-end-position' in
      ;; `flycheck-error-column-region' and `flycheck-error-line-region'.
      (let ((inhibit-field-text-motion t))
        (pcase mode
          (`columns (or (flycheck-error-column-region err)
                        (flycheck-error-line-region err)))
          (`symbols (or (flycheck-error-thing-region 'symbol err)
                        (flycheck-error-region-for-mode err 'columns)))
          (`sexps (or (flycheck-error-thing-region 'sexp err)
                      (flycheck-error-region-for-mode err 'columns)))
          (`lines (flycheck-error-line-region err))
          (_ (error "Invalid mode %S" mode)))))

    (defun flycheck-error-pos (err)
      "Get the buffer position of ERR.

    ERR is a Flycheck error whose position to get.

    The error position is the error column, or the first
    non-whitespace character of the error line, if ERR has no error column."
      (car (or (flycheck-error-column-region err)
               (flycheck-error-line-region err))))

    (defun flycheck-error-format-message-and-id (err)
      "Format the message and id of ERR as human-readable string."
      (let ((id (flycheck-error-id err))
            (message (flycheck-error-message err)))
        (if id (format "%s [%s]" message id) message)))

    (defun flycheck-error-format (err &optional with-file-name)
      "Format ERR as human-readable string, optionally WITH-FILE-NAME.

    Return a string that represents the given ERR.  If WITH-FILE-NAME
    is given and non-nil, include the file-name as well, otherwise
    omit it."
      (let* ((line (flycheck-error-line err))
             (column (flycheck-error-column err))
             (level (symbol-name (flycheck-error-level err)))
             (checker (symbol-name (flycheck-error-checker err)))
             (format `(,@(when with-file-name
                           (list (flycheck-error-filename err) ":"))
                       ,(number-to-string line) ":"
                       ,@(when column (list (number-to-string column) ":"))
                       ,level ": "
                       ,(flycheck-error-format-message-and-id err)
                       " (" ,checker ")")))
        (apply #'concat format)))

    (defun flycheck-error-< (err1 err2)
      "Determine whether ERR1 is less than ERR2 by location.

    Compare by line numbers and then by column numbers."
      (let ((line1 (flycheck-error-line err1))
            (line2 (flycheck-error-line err2)))
        (if (= line1 line2)
            (let ((col1 (flycheck-error-column err1))
                  (col2 (flycheck-error-column err2)))
              (and col2
                   ;; Sort errors for the whole line first
                   (or (not col1) (< col1 col2))))
          (< line1 line2))))

    (defun flycheck-error-level-< (err1 err2)
      "Determine whether ERR1 is less than ERR2 by error level.

    Like `flycheck-error-<', but compares by error level severity
    first.  Levels of the same severity are compared by name."
      (let* ((level1 (flycheck-error-level err1))
             (level2 (flycheck-error-level err2))
             (severity1 (flycheck-error-level-severity level1))
             (severity2 (flycheck-error-level-severity level2)))
        (cond
         ((= severity1 severity2)
          (if (string= level1 level2)
              (flycheck-error-< err1 err2)
            (string< level1 level2)))
         (t (< severity1 severity2)))))

    (defun flycheck-assert-error-list-p (errors)
      "Assert that all items in ERRORS are of `flycheck-error' type.

    Signal an error if any item in ERRORS is not a `flycheck-error'
    object, as by `flycheck-error-p'.  Otherwise return ERRORS
    again."
      (unless (listp errors)
        (signal 'wrong-type-argument (list 'listp errors)))
      (dolist (err errors)
        (unless (flycheck-error-p err)
          (signal 'wrong-type-argument (list 'flycheck-error-p err))))
      errors)


    ;;; Errors in the current buffer
    (defvar-local flycheck-current-errors nil
      "A list of all errors and warnings in the current buffer.")

    (defun flycheck-report-current-errors (errors)
      "Report ERRORS in the current buffer.

    Add ERRORS to `flycheck-current-errors' and process each error
    with `flycheck-process-error-functions'."
      (setq flycheck-current-errors (sort (append errors flycheck-current-errors)
                                          #'flycheck-error-<))
      (overlay-recenter (point-max))
      (seq-do (lambda (err)
                (run-hook-with-args-until-success 'flycheck-process-error-functions
                                                  err))
              errors))

    (defun flycheck-clear-errors ()
      "Remove all error information from the current buffer."
      (setq flycheck-current-errors nil)
      (flycheck-report-status 'not-checked))

    (defun flycheck-fill-and-expand-error-file-names (errors directory)
      "Fill and expand file names in ERRORS relative to DIRECTORY.

    Expand all file names of ERRORS against DIRECTORY.  If the file
    name of an error is nil fill in the result of function
    `buffer-file-name' in the current buffer.

    Return ERRORS, modified in-place."
      (seq-do (lambda (err)
                (setf (flycheck-error-filename err)
                      (-if-let (filename (flycheck-error-filename err))
                          (expand-file-name filename directory)
                        (buffer-file-name))))
              errors)
      errors)

    (defun flycheck-relevant-error-p (err)
      "Determine whether ERR is relevant for the current buffer.

    Return t if ERR may be shown for the current buffer, or nil
    otherwise."
      (flycheck-error-with-buffer err
        (let ((file-name (flycheck-error-filename err))
              (message (flycheck-error-message err)))
          (and
           ;; The error is relevant for the current buffer if it's got no file-name
           ;; and the current buffer has no file name, too, or if it refers to the
           ;; same file as the current buffer.
           (or (and (not file-name) (not buffer-file-name))
               (and buffer-file-name file-name
                    (flycheck-same-files-p file-name buffer-file-name)))
           message
           (not (string-empty-p message))
           (flycheck-error-line err)))))

    (defun flycheck-relevant-errors (errors)
      "Filter the relevant errors from ERRORS.

    Return a list of all errors that are relevant for their
    corresponding buffer."
      (seq-filter #'flycheck-relevant-error-p errors))

    (defun flycheck-related-errors (err)
      "Get all the errors that are in the same group as ERR.

    Return a list of all errors (in `flycheck-current-errors') that
    have the same `flycheck-error-group' as ERR, including ERR
    itself."
      (-if-let (group (flycheck-error-group err))
          (seq-filter (lambda (e)
                        (eq (flycheck-error-group e) group))
                      flycheck-current-errors)
        (list err)))


    ;;; Status reporting for the current buffer
    (defvar-local flycheck-last-status-change 'not-checked
      "The last status change in the current buffer.")

    (defun flycheck-report-failed-syntax-check (&optional status)
      "Report a failed Flycheck syntax check with STATUS.

    STATUS is a status symbol for `flycheck-report-status',
    defaulting to `errored'.

    Clear Flycheck state, run `flycheck-syntax-check-failed-hook' and
    report an error STATUS."
      (flycheck-clear)
      (setq flycheck-current-syntax-check nil)
      (run-hooks 'flycheck-syntax-check-failed-hook)
      (flycheck-report-status (or status 'errored)))

    (defun flycheck-report-status (status)
      "Report Flycheck STATUS.

    STATUS is one of the following symbols:

    `not-checked'
         The current buffer was not checked.

    `no-checker'
         Automatic syntax checker selection did not find a suitable
         syntax checker.

    `running'
         A syntax check is now running in the current buffer.

    `errored'
         The current syntax check has errored.

    `finished'
         The current syntax check was finished normally.

    `interrupted'
         The current syntax check was interrupted.

    `suspicious'
         The last syntax check had a suspicious result.

    Set `flycheck-last-status-change' and call
    `flycheck-status-changed-functions' with STATUS.  Afterwards
    refresh the mode line."
      (setq flycheck-last-status-change status)
      (run-hook-with-args 'flycheck-status-changed-functions status)
      (force-mode-line-update))

    (defun flycheck-mode-line-status-text (&optional status)
      "Get a text describing STATUS for use in the mode line.

    STATUS defaults to `flycheck-last-status-change' if omitted or
    nil."
      (let ((text (pcase (or status flycheck-last-status-change)
                    (`not-checked "")
                    (`no-checker "-")
                    (`running "*")
                    (`errored "!")
                    (`finished
                     (let-alist (flycheck-count-errors flycheck-current-errors)
                       (if (or .error .warning)
                           (format ":%s/%s" (or .error 0) (or .warning 0))
                         "")))
                    (`interrupted "-")
                    (`suspicious "?"))))
        (concat " " flycheck-mode-line-prefix text)))


    ;;; Error levels
    ;;;###autoload
    (defun flycheck-define-error-level (level &rest properties)
      "Define a new error LEVEL with PROPERTIES.

    The following PROPERTIES constitute an error level:

    `:severity SEVERITY'
         A number denoting the severity of this level.  The higher
         the number, the more severe is this level compared to other
         levels.  Defaults to 0.

         The severity is used by `flycheck-error-level-<' to
         determine the ordering of errors according to their levels.

    `:compilation-level LEVEL'

         A number indicating the broad class of messages that errors
         at this level belong to: one of 0 (info), 1 (warning), or
         2 or nil (error).  Defaults to nil.

         This is used by `flycheck-checker-pattern-to-error-regexp'
         to map error levels into `compilation-mode''s hierarchy and
         to get proper highlighting of errors in `compilation-mode'.

    `:overlay-category CATEGORY'
         A symbol denoting the overlay category to use for error
         highlight overlays for this level.  See Info
         node `(elisp)Overlay Properties' for more information about
         overlay categories.

         A category for an error level overlay should at least define
         the `face' property, for error highlighting.  Another useful
         property for error level categories is `priority', to
         influence the stacking of multiple error level overlays.

    `:fringe-bitmap BITMAP'
         A fringe bitmap symbol denoting the bitmap to use for fringe
         indicators for this level.  See Info node `(elisp)Fringe
         Bitmaps' for more information about fringe bitmaps,
         including a list of built-in fringe bitmaps.

    `:fringe-face FACE'
         A face symbol denoting the face to use for fringe indicators
         for this level.

    `:error-list-face FACE'
         A face symbol denoting the face to use for messages of this
         level in the error list.  See `flycheck-list-errors'."
      (declare (indent 1))
      (setf (get level 'flycheck-error-level) t)
      (setf (get level 'flycheck-error-severity)
            (or (plist-get properties :severity) 0))
      (setf (get level 'flycheck-compilation-level)
            (plist-get properties :compilation-level))
      (setf (get level 'flycheck-overlay-category)
            (plist-get properties :overlay-category))
      (setf (get level 'flycheck-fringe-bitmap-double-arrow)
            (plist-get properties :fringe-bitmap))
      (setf (get level 'flycheck-fringe-face)
            (plist-get properties :fringe-face))
      (setf (get level 'flycheck-error-list-face)
            (plist-get properties :error-list-face)))

    (defun flycheck-error-level-p (level)
      "Determine whether LEVEL is a Flycheck error level."
      (get level 'flycheck-error-level))

    (defun flycheck-error-level-severity (level)
      "Get the numeric severity of LEVEL."
      (or (get level 'flycheck-error-severity) 0))

    (defun flycheck-error-level-compilation-level (level)
      "Get the compilation level for LEVEL."
      (get level 'flycheck-compilation-level))

    (defun flycheck-error-level-overlay-category (level)
      "Get the overlay category for LEVEL."
      (get level 'flycheck-overlay-category))

    (defun flycheck-error-level-fringe-bitmap (level)
      "Get the fringe bitmap for LEVEL."
      (get level 'flycheck-fringe-bitmap-double-arrow))

    (defun flycheck-error-level-fringe-face (level)
      "Get the fringe face for LEVEL."
      (get level 'flycheck-fringe-face))

    (defun flycheck-error-level-error-list-face (level)
      "Get the error list face for LEVEL."
      (get level 'flycheck-error-list-face))

    (defun flycheck-error-level-make-fringe-icon (level side)
      "Create the fringe icon for LEVEL at SIDE.

    Return a propertized string that shows a fringe bitmap according
    to LEVEL and the given fringe SIDE.

    LEVEL is a Flycheck error level defined with
    `flycheck-define-error-level', and SIDE is either `left-fringe'
    or `right-fringe'.

    Return a propertized string representing the fringe icon,
    intended for use as `before-string' of an overlay to actually
    show the icon."
      (unless (memq side '(left-fringe right-fringe))
        (error "Invalid fringe side: %S" side))
      (propertize "!" 'display
                  (list side
                        (flycheck-error-level-fringe-bitmap level)
                        (flycheck-error-level-fringe-face level))))


    ;;; Built-in error levels
    (when (fboundp 'define-fringe-bitmap)
      (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
        (vector #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b10011000
                #b01101100
                #b00110110
                #b00011011
                #b00110110
                #b01101100
                #b10011000
                #b00000000
                #b00000000
                #b00000000
                #b00000000
                #b00000000)))

    (setf (get 'flycheck-error-overlay 'face) 'flycheck-error)
    (setf (get 'flycheck-error-overlay 'priority) 110)

    (flycheck-define-error-level 'error
      :severity 100
      :compilation-level 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      :fringe-face 'flycheck-fringe-error
      :error-list-face 'flycheck-error-list-error)

    (setf (get 'flycheck-warning-overlay 'face) 'flycheck-warning)
    (setf (get 'flycheck-warning-overlay 'priority) 100)

    (flycheck-define-error-level 'warning
      :severity 10
      :compilation-level 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      :fringe-face 'flycheck-fringe-warning
      :error-list-face 'flycheck-error-list-warning)

    (setf (get 'flycheck-info-overlay 'face) 'flycheck-info)
    (setf (get 'flycheck-info-overlay 'priority) 90)

    (flycheck-define-error-level 'info
      :severity -10
      :compilation-level 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      :fringe-face 'flycheck-fringe-info
      :error-list-face 'flycheck-error-list-info)


    ;;; Error filtering
    (defun flycheck-filter-errors (errors checker)
      "Filter ERRORS from CHECKER.

    Apply the error filter of CHECKER to ERRORs and return the
    result.  If CHECKER has no error filter, fall back to
    `flycheck-sanitize-errors'."
      (let ((filter (or (flycheck-checker-get checker 'error-filter)
                        #'flycheck-sanitize-errors)))
        (funcall filter errors)))

    (defun flycheck-sanitize-errors (errors)
      "Sanitize ERRORS.

    Sanitize ERRORS by trimming leading and trailing whitespace in
    all error messages, and by replacing 0 columns and empty error
    messages with nil.

    Returns sanitized ERRORS."
      (dolist (err errors)
        (flycheck-error-with-buffer err
          (let ((message (flycheck-error-message err))
                (column (flycheck-error-column err))
                (id (flycheck-error-id err)))
            (when message
              (setq message (string-trim message))
              (setf (flycheck-error-message err)
                    (if (string-empty-p message) nil message)))
            (when (and id (string-empty-p id))
              (setf (flycheck-error-id err) nil))
            (when (eq column 0)
              (setf (flycheck-error-column err) nil)))))
      errors)

    (defun flycheck-remove-error-file-names (file-name errors)
      "Remove matching FILE-NAME from ERRORS.

    Use as `:error-filter' for syntax checkers that output faulty
    filenames.  Flycheck will later fill in the buffer file name.

    Return ERRORS."
      (seq-do (lambda (err)
                (when (and (flycheck-error-filename err)
                           (string= (flycheck-error-filename err) file-name))
                  (setf (flycheck-error-filename err) nil)))
              errors)
      errors)

    (defun flycheck-increment-error-columns (errors &optional offset)
      "Increment all columns of ERRORS by OFFSET.

    Use this as `:error-filter' if a syntax checker outputs 0-based
    columns."
      (seq-do (lambda (err)
                (let ((column (flycheck-error-column err)))
                  (when column
                    (setf (flycheck-error-column err)
                          (+ column (or offset 1))))))
              errors)
      errors)

    (defun flycheck-collapse-error-message-whitespace (errors)
      "Collapse whitespace in all messages of ERRORS.

    Return ERRORS."
      (dolist (err errors)
        (-when-let (message (flycheck-error-message err))
          (setf (flycheck-error-message err)
                (replace-regexp-in-string (rx (one-or-more (any space "\n" "\r")))
                                          " " message 'fixed-case 'literal))))
      errors)

    (defun flycheck-dedent-error-messages (errors)
      "Dedent all messages of ERRORS.

    For each error in ERRORS, determine the indentation offset from
    the leading whitespace of the first line, and dedent all further
    lines accordingly.

    Return ERRORS, with in-place modifications."
      (dolist (err errors)
        (-when-let (message (flycheck-error-message err))
          (with-temp-buffer
            (insert message)
            ;; Determine the indentation offset
            (goto-char (point-min))
            (back-to-indentation)
            (let* ((indent-offset (- (point) (point-min))))
              ;; Now iterate over all lines and dedent each according to
              ;; `indent-offset'
              (while (not (eobp))
                (back-to-indentation)
                ;; If the current line starts with sufficient whitespace, delete the
                ;; indendation offset.  Otherwise keep the line intact, as we might
                ;; loose valuable information
                (when (>= (- (point) (line-beginning-position)) indent-offset)
                  (delete-char (- indent-offset)))
                (forward-line 1)))
            (delete-trailing-whitespace (point-min) (point-max))
            (setf (flycheck-error-message err)
                  (buffer-substring-no-properties (point-min) (point-max))))))
      errors)

    (defun flycheck-fold-include-levels (errors sentinel-message)
      "Fold levels of ERRORS from included files.

    ERRORS is a list of `flycheck-error' objects.  SENTINEL-MESSAGE
    is a regular expression matched against the error message to
    determine whether the errror denotes errors from an included
    file.  Alternatively, it is a function that is given an error and
    shall return non-nil, if the error denotes errors from an
    included file."
      (unless (or (stringp sentinel-message) (functionp sentinel-message))
        (error "Sentinel must be string or function: %S" sentinel-message))
      (let ((sentinel (if (functionp sentinel-message)
                          sentinel-message
                        (lambda (err)
                          (string-match-p sentinel-message
                                          (flycheck-error-message err)))))
            (remaining-errors errors))
        (while remaining-errors
          (let* ((current-error (pop remaining-errors)))
            (when (funcall sentinel current-error)
              ;; We found an error denoting errors in the included file:
              ;; 1. process all subsequent errors until faulty include file is found
              ;; 2. process again all subsequent errors until an error has the
              ;;    current file name again
              ;; 3. find the most severe error level
              (let ((current-filename (flycheck-error-filename current-error))
                    (current-level nil)
                    (faulty-include-filename nil)
                    (filename nil)
                    (done (null remaining-errors)))

                (while (not done)
                  (setq filename (flycheck-error-filename (car remaining-errors)))
                  (unless faulty-include-filename
                    (unless (string= filename current-filename)
                      (setq faulty-include-filename filename)))

                  (let* ((error-in-include (pop remaining-errors))
                         (in-include-level (flycheck-error-level error-in-include)))
                    (unless (funcall sentinel error-in-include)
                      ;; Ignore nested "included file" errors, we are only
                      ;; interested in real errors because these define our level
                      (when (or (not current-level)
                                (> (flycheck-error-level-severity in-include-level)
                                   (flycheck-error-level-severity current-level)))
                        (setq current-level in-include-level))))

                  (setq done (or (null remaining-errors)
                                 (and faulty-include-filename
                                      (string= filename current-filename)))))

                (setf (flycheck-error-level current-error) current-level
                      (flycheck-error-message current-error)
                      (format "In include %s" faulty-include-filename))))))
        errors))

    (defun flycheck-dequalify-error-ids (errors)
      "De-qualify error ids in ERRORS.

    Remove all qualifications from error ids in ERRORS, by stripping
    all leading dotted components from error IDs.  For instance, if
    the error ID is com.foo.E100, replace it with E100.

    This error filter is mainly useful to simplify error IDs obtained
    from parsing Checkstyle XML, which frequently has very verbose
    IDs, that include the name of the tool."
      (seq-do (lambda (err)
                (let ((id (flycheck-error-id err)))
                  (when id
                    (setf (flycheck-error-id err)
                          (replace-regexp-in-string
                           (rx string-start
                               (group
                                (optional (zero-or-more not-newline) "."))
                               (one-or-more (not (any ".")))
                               string-end)
                           "" id 'fixedcase 'literal 1)))))
              errors)
      errors)

    (defun flycheck-remove-error-ids (errors)
      "Remove all error ids from ERRORS."
      (seq-do (lambda (err) (setf (flycheck-error-id err) nil)) errors)
      errors)

    (defun flycheck-fill-empty-line-numbers (errors)
      "Set ERRORS without lines to line 0.

    Use as `:error-filter' for syntax checkers that output errors
    without line numbers.

    Return ERRORS."
      (seq-do (lambda (err)
                (unless (flycheck-error-line err)
                  (setf (flycheck-error-line err) 0)))
              errors)
      errors)


    ;;; Error analysis
    (defun flycheck-count-errors (errors)
      "Count the number of ERRORS, grouped by level..

    Return an alist, where each ITEM is a cons cell whose `car' is an
    error level, and whose `cdr' is the number of errors of that
    level."
      (let (counts-by-level)
        (dolist (err errors)
          (let* ((level (flycheck-error-level err))
                 (item (assq level counts-by-level)))
            (if item
                (cl-incf (cdr item))
              (push (cons level 1) counts-by-level))))
        counts-by-level))

    (defun flycheck-has-max-errors-p (errors level)
      "Check if there is no error in ERRORS more severe than LEVEL."
      (let ((severity (flycheck-error-level-severity level)))
        (seq-every-p (lambda (e) (<= (flycheck-error-level-severity
                                      (flycheck-error-level e))
                                     severity))
                     errors)))

    (defun flycheck-has-max-current-errors-p (level)
      "Check if there is no current error more severe than LEVEL."
      (flycheck-has-max-errors-p flycheck-current-errors level))

    (defun flycheck-has-errors-p (errors level)
      "Determine if there are any ERRORS with LEVEL."
      (seq-some (lambda (e) (eq (flycheck-error-level e) level)) errors))

    (defun flycheck-has-current-errors-p (&optional level)
      "Determine if the current buffer has errors with LEVEL.

    If LEVEL is omitted if the current buffer has any errors at all."
      (if level
          (flycheck-has-errors-p flycheck-current-errors level)
        (and flycheck-current-errors t)))


    ;;; Error overlays in the current buffer
    (defun flycheck-add-overlay (err)
      "Add overlay for ERR.

    Return the created overlay."
      ;; We must have a proper error region for the sake of fringe indication,
      ;; error display and error navigation, even if the highlighting is disabled.
      ;; We erase the highlighting later on in this case
      (pcase-let* ((`(,beg . ,end) (flycheck-error-region-for-mode
                                    err (or flycheck-highlighting-mode 'lines)))
                   (overlay (make-overlay beg end))
                   (level (flycheck-error-level err))
                   (category (flycheck-error-level-overlay-category level)))
        (unless (flycheck-error-level-p level)
          (error "Undefined error level: %S" level))
        (setf (overlay-get overlay 'flycheck-overlay) t)
        (setf (overlay-get overlay 'flycheck-error) err)
        (setf (overlay-get overlay 'category) category)
        (unless flycheck-highlighting-mode
          ;; Erase the highlighting from the overlay if requested by the user
          (setf (overlay-get overlay 'face) nil))
        (when flycheck-indication-mode
          (setf (overlay-get overlay 'before-string)
                (flycheck-error-level-make-fringe-icon
                 level flycheck-indication-mode)))
        (setf (overlay-get overlay 'help-echo) #'flycheck-help-echo)
        overlay))

    (defun flycheck-help-echo (_window object pos)
      "Construct a tooltip message.

    Most of the actual work is done by calling
    `flycheck-help-echo-function' with the appropriate list of
    errors.  Arguments WINDOW, OBJECT and POS are as described in
    info node `(elisp)Special properties', as this function is
    intended to be used as the 'help-echo property of flycheck error
    overlays."
      (-when-let (buf (cond ((bufferp object) object)
                            ((overlayp object) (overlay-buffer object))))
        (with-current-buffer buf
          (-when-let* ((fn flycheck-help-echo-function)
                       (errs (flycheck-overlay-errors-at pos)))
            (funcall fn errs)))))

    (defun flycheck-help-echo-all-error-messages (errs)
      "Concatenate error messages and ids from ERRS."
      (mapconcat
       (lambda (err)
         (when err
           (if (flycheck-error-message err)
               (flycheck-error-format-message-and-id err)
             (format "Unknown %s" (flycheck-error-level err)))))
       (reverse errs) "\n\n"))

    (defun flycheck-filter-overlays (overlays)
      "Get all Flycheck overlays from OVERLAYS."
      (seq-filter (lambda (o) (overlay-get o 'flycheck-overlay)) overlays))

    (defun flycheck-overlays-at (pos)
      "Get all Flycheck overlays at POS."
      (flycheck-filter-overlays (overlays-at pos)))

    (defun flycheck-overlays-in (beg end)
      "Get all Flycheck overlays between BEG and END."
      (flycheck-filter-overlays (overlays-in beg end)))

    (defun flycheck-overlay-errors-at (pos)
      "Return a list of all flycheck errors overlayed at POS."
      (seq-map (lambda (o) (overlay-get o 'flycheck-error))
               (flycheck-overlays-at pos)))

    (defun flycheck-overlay-errors-in (beg end)
      "Return a list of all flycheck errors overlayed between BEG and END."
      (seq-map (lambda (o) (overlay-get o 'flycheck-error))
               (flycheck-overlays-in beg end)))

    (defvar-local flycheck-overlays-to-delete nil
      "Overlays mark for deletion after all syntax checks completed.")
    (put 'flycheck-overlays-to-delete 'permanent-local t)

    (defun flycheck-delete-all-overlays ()
      "Remove all flycheck overlays in the current buffer."
      (overlay-recenter (point-max))
      (flycheck-delete-marked-overlays)
      (save-restriction
        (widen)
        (seq-do #'delete-overlay (flycheck-overlays-in (point-min) (point-max)))))

    (defun flycheck-mark-all-overlays-for-deletion ()
      "Mark all current overlays for deletion."
      (setq flycheck-overlays-to-delete
            (append (flycheck-overlays-in (point-min) (point-max))
                    flycheck-overlays-to-delete)))

    (defun flycheck-delete-marked-overlays ()
      "Delete all overlays marked for deletion."
      (overlay-recenter (point-max))
      (seq-do #'delete-overlay flycheck-overlays-to-delete)
      (setq flycheck-overlays-to-delete nil))


    ;;; Error navigation in the current buffer
    (defun flycheck-error-level-interesting-at-pos-p (pos)
      "Check if error severity at POS passes `flycheck-error-level-interesting-p'."
      (flycheck-error-level-interesting-p (get-char-property pos 'flycheck-error)))

    (defun flycheck-error-level-interesting-p (err)
      "Check if ERR severity is >= `flycheck-navigation-minimum-level'."
      (when (flycheck-error-p err)
        (-if-let (min-level flycheck-navigation-minimum-level)
            (<= (flycheck-error-level-severity min-level)
                (flycheck-error-level-severity (flycheck-error-level err)))
          t)))

    (defun flycheck-next-error-pos (n &optional reset)
      "Get the position of the N-th next error.

    With negative N, get the position of the (-N)-th previous error
    instead.  With non-nil RESET, search from `point-min', otherwise
    search from the current point.

    Return the position of the next or previous error, or nil if
    there is none."
      (let ((n (or n 1))
            (pos (if reset (point-min) (point))))
        (if (>= n 0)
            ;; Search forwards
            (while (and pos (> n 0))
              (setq n (1- n))
              (when (get-char-property pos 'flycheck-error)
                ;; Move beyond from the current error if any
                (setq pos (next-single-char-property-change pos 'flycheck-error)))
              (while (not (or (= pos (point-max))
                              (flycheck-error-level-interesting-at-pos-p pos)))
                ;; Scan for the next error
                (setq pos (next-single-char-property-change pos 'flycheck-error)))
              (when (and (= pos (point-max))
                         (not (flycheck-error-level-interesting-at-pos-p pos)))
                ;; If we reached the end of the buffer, but no error, we didn't find
                ;; any
                (setq pos nil)))
          ;; Search backwards
          (while (and pos (< n 0))
            (setq n (1+ n))
            ;; Loop until we find an error.  We need to check the position *before*
            ;; the current one, because `previous-single-char-property-change'
            ;; always moves to the position *of* the change.
            (while (not (or (= pos (point-min))
                            (flycheck-error-level-interesting-at-pos-p (1- pos))))
              (setq pos (previous-single-char-property-change pos 'flycheck-error)))
            (when (and (= pos (point-min))
                       (not (flycheck-error-level-interesting-at-pos-p pos)))
              ;; We didn't find any error.
              (setq pos nil))
            (when pos
              ;; We found an error, so move to its beginning
              (setq pos (previous-single-char-property-change pos
                                                              'flycheck-error)))))
        pos))

    (defun flycheck-next-error-function (n reset)
      "Visit the N-th error from the current point.

    N is the number of errors to advance by, where a negative N
    advances backwards.  With non-nil RESET, advance from the
    beginning of the buffer, otherwise advance from the current
    position.

    Intended for use with `next-error-function'."
      (let ((pos (flycheck-next-error-pos n reset)))
        (if pos
            (goto-char pos)
          (user-error "No more Flycheck errors"))))

    (defun flycheck-next-error (&optional n reset)
      "Visit the N-th error from the current point.

    N is the number of errors to advance by, where a negative N
    advances backwards.  With non-nil RESET, advance from the
    beginning of the buffer, otherwise advance from the current
    position."
      (interactive "P")
      (when (consp n)
        ;; Universal prefix argument means reset
        (setq reset t n nil))
      (flycheck-next-error-function n reset)
      (flycheck-display-error-at-point))

    (defun flycheck-previous-error (&optional n)
      "Visit the N-th previous error.

    If given, N specifies the number of errors to move backwards by.
    If N is negative, move forwards instead."
      (interactive "P")
      (flycheck-next-error (- (or n 1))))

    (defun flycheck-first-error (&optional n)
      "Visit the N-th error from beginning of the buffer.

    If given, N specifies the number of errors to move forward from
    the beginning of the buffer."
      (interactive "P")
      (flycheck-next-error n 'reset))


    ;;; Listing errors in buffers
    (defconst flycheck-error-list-buffer "*Flycheck errors*"
      "The name of the buffer to show error lists.")

    (defvar flycheck-error-list-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "f") #'flycheck-error-list-set-filter)
        (define-key map (kbd "F") #'flycheck-error-list-reset-filter)
        (define-key map (kbd "n") #'flycheck-error-list-next-error)
        (define-key map (kbd "p") #'flycheck-error-list-previous-error)
        (define-key map (kbd "g") #'flycheck-error-list-check-source)
        (define-key map (kbd "e") #'flycheck-error-list-explain-error)
        (define-key map (kbd "RET") #'flycheck-error-list-goto-error)
        map)
      "The keymap of `flycheck-error-list-mode'.")

    (defun flycheck-error-list-make-last-column (message checker)
      "Compute contents of the last error list cell.

    MESSAGE and CHECKER are displayed in a single column to allow the
    message to stretch arbitrarily far."
      (let ((checker-name (propertize (symbol-name checker)
                                      'face 'flycheck-error-list-checker-name)))
        (format (propertize "%s (%s)" 'face 'default)
                message checker-name)))

    (defconst flycheck-error-list-format
      `[("Line" 5 flycheck-error-list-entry-< :right-align t)
        ("Col" 3 nil :right-align t)
        ("Level" 8 flycheck-error-list-entry-level-<)
        ("ID" 6 t)
        (,(flycheck-error-list-make-last-column "Message" 'Checker) 0 t)]
      "Table format for the error list.")

    (defconst flycheck-error-list-padding 1
      "Padding used in error list.")

    (defconst flycheck--error-list-msg-offset
      (seq-reduce
       (lambda (offset fmt)
         (pcase-let* ((`(,_ ,width ,_ . ,props) fmt)
                      (padding (or (plist-get props :pad-right) 1)))
           (+ offset width padding)))
       (seq-subseq flycheck-error-list-format 0 -1)
       flycheck-error-list-padding)
      "Amount of space to use in `flycheck-flush-multiline-message'.")

    (define-derived-mode flycheck-error-list-mode tabulated-list-mode "Flycheck errors"
      "Major mode for listing Flycheck errors.

    \\{flycheck-error-list-mode-map}"
      (setq tabulated-list-format flycheck-error-list-format
            ;; Sort by location initially
            tabulated-list-sort-key (cons "Line" nil)
            tabulated-list-padding flycheck-error-list-padding
            tabulated-list-entries #'flycheck-error-list-entries
            ;; `revert-buffer' updates the mode line for us, so all we need to do is
            ;; set the corresponding mode line construct.
            mode-line-buffer-identification flycheck-error-list-mode-line)
      ;; Guard `truncate-string-ellipsis' for Emacs 24.
      ;; TODO: Remove when dropping Emacs 24 compatibility
      (when (boundp 'truncate-string-ellipsis)
        ;; See https://github.com/flycheck/flycheck/issues/1101
        (setq-local truncate-string-ellipsis "…"))
      (tabulated-list-init-header))

    (defvar-local flycheck-error-list-source-buffer nil
      "The current source buffer of the error list.")
    ;; Needs to permanently local to preserve the source buffer across buffer
    ;; reversions
    (put 'flycheck-error-list-source-buffer 'permanent-local t)

    (defun flycheck-error-list-set-source (buffer)
      "Set BUFFER as the source buffer of the error list."
      (when (get-buffer flycheck-error-list-buffer)
        (with-current-buffer flycheck-error-list-buffer
          ;; Only update the source when required
          (unless (eq buffer flycheck-error-list-source-buffer)
            (setq flycheck-error-list-source-buffer buffer)
            (flycheck-error-list-refresh)))))

    (defun flycheck-error-list-update-source ()
      "Update the source buffer of the error list."
      (when (not (eq (current-buffer) (get-buffer flycheck-error-list-buffer)))
        ;; We must not update the source buffer, if the current buffer is the error
        ;; list itself.
        (flycheck-error-list-set-source (current-buffer))))

    (defun flycheck-error-list-check-source ()
      "Trigger a syntax check in the source buffer of the error list."
      (interactive)
      (let ((buffer (get-buffer flycheck-error-list-source-buffer)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (flycheck-buffer)))))

    (define-button-type 'flycheck-error-list
      'action #'flycheck-error-list-button-goto-error
      'help-echo "mouse-2, RET: goto error")

    (defun flycheck-error-list-button-goto-error (button)
      "Go to the error at BUTTON."
      (flycheck-error-list-goto-error (button-start button)))

    (define-button-type 'flycheck-error-list-explain-error
      'action #'flycheck-error-list-button-explain-error
      'help-echo "mouse-2, RET: explain error")

    (defun flycheck-error-list-button-explain-error (button)
      "Explain the error at BUTTON."
      (flycheck-error-list-explain-error (button-start button)))

    (defsubst flycheck-error-list-make-cell (text &optional face help-echo type)
      "Make an error list cell with TEXT and FACE.

    If FACE is nil don't set a FACE on TEXT.  If TEXT already has
    face properties, do not specify a FACE.  Note though, that if
    TEXT gets truncated it will not inherit any previous face
    properties.  If you expect TEXT to be truncated in the error
    list, do specify a FACE explicitly!

    If HELP-ECHO is non-nil, set a help-echo property on TEXT, with
    value HELP-ECHO.  This is convenient if you expect TEXT to be
    truncated.

    The cell will have the type TYPE unless TYPE is nil, and the
    default type `flycheck-error-list' will be used instead."
      (append (list text 'type (if type type
                                 'flycheck-error-list))
              (and face (list 'face face))
              (and help-echo (list 'help-echo help-echo))))

    (defsubst flycheck-error-list-make-number-cell (number face)
      "Make a table cell for a NUMBER with FACE.

    Convert NUMBER to string, fontify it with FACE and return the
    string with attached text properties."
      (flycheck-error-list-make-cell
       (if (numberp number) (number-to-string number) "")
       face))

    (defun flycheck-error-list-make-entry (error)
      "Make a table cell for the given ERROR.

    Return a list with the contents of the table cell."
      (let* ((level (flycheck-error-level error))
             (level-face (flycheck-error-level-error-list-face level))
             (line (flycheck-error-line error))
             (column (flycheck-error-column error))
             (message (or (flycheck-error-message error)
                          (format "Unknown %s" (symbol-name level))))
             (flushed-msg (flycheck-flush-multiline-message message))
             (id (flycheck-error-id error))
             (id-str (if id (format "%s" id) ""))
             (checker (flycheck-error-checker error))
             (msg-and-checker (flycheck-error-list-make-last-column flushed-msg checker))
             (explainer (flycheck-checker-get checker 'error-explainer)))
        (list error
              (vector (flycheck-error-list-make-number-cell
                       line 'flycheck-error-list-line-number)
                      (flycheck-error-list-make-number-cell
                       column 'flycheck-error-list-column-number)
                      (flycheck-error-list-make-cell
                       (symbol-name (flycheck-error-level error)) level-face)
                      ;; Error ID use a different face when an error-explainer is present
                      (flycheck-error-list-make-cell
                       id-str (if explainer 'flycheck-error-list-id-with-explainer
                                'flycheck-error-list-id)
                       id-str 'flycheck-error-list-explain-error)
                      (flycheck-error-list-make-cell
                       msg-and-checker nil msg-and-checker)))))

    (defun flycheck-flush-multiline-message (msg)
      "Prepare error message MSG for display in the error list.

    Prepend all lines of MSG except the first with enough space to
    ensure that they line up properly once the message is displayed."
      (let* ((spc-spec `(space . (:width ,flycheck--error-list-msg-offset)))
             (spc (propertize " " 'display spc-spec))
             (rep (concat "\\1" spc "\\2")))
        (replace-regexp-in-string "\\([\r\n]+\\)\\(.\\)" rep msg)))

    (defun flycheck-error-list-current-errors ()
      "Read the list of errors in `flycheck-error-list-source-buffer'."
      (when (buffer-live-p flycheck-error-list-source-buffer)
        (buffer-local-value 'flycheck-current-errors
                            flycheck-error-list-source-buffer)))

    (defun flycheck-error-list-entries ()
      "Create the entries for the error list."
      (-when-let* ((errors (flycheck-error-list-current-errors))
                   (filtered (flycheck-error-list-apply-filter errors)))
        (seq-map #'flycheck-error-list-make-entry filtered)))

    (defun flycheck-error-list-entry-< (entry1 entry2)
      "Determine whether ENTRY1 is before ENTRY2 by location.

    See `flycheck-error-<'."
      (flycheck-error-< (car entry1) (car entry2)))

    (defun flycheck-error-list-entry-level-< (entry1 entry2)
      "Determine whether ENTRY1 is before ENTRY2 by level.

    See `flycheck-error-level-<'."
      (not (flycheck-error-level-< (car entry1) (car entry2))))

    (defvar flycheck-error-list-mode-line-map
      (let ((map (make-sparse-keymap)))
        (define-key map [mode-line mouse-1]
          #'flycheck-error-list-mouse-switch-to-source)
        map)
      "Keymap for error list mode line.")

    (defun flycheck-error-list-propertized-source-name ()
      "Get the name of the current source buffer for the mode line.

    Propertize the name of the current source buffer for use in the
    mode line indication of `flycheck-error-list-mode'."
      (let ((name (replace-regexp-in-string
                   (rx "%") "%%"
                   (buffer-name flycheck-error-list-source-buffer)
                   'fixed-case 'literal)))
        (propertize name 'face 'mode-line-buffer-id
                    'mouse-face 'mode-line-highlight
                    'help-echo "mouse-1: switch to source"
                    'local-map flycheck-error-list-mode-line-map)))

    (defun flycheck-error-list-mouse-switch-to-source (event)
      "Switch to the error list source buffer of the EVENT window."
      (interactive "e")
      (save-selected-window
        (when (eventp event)
          (select-window (posn-window (event-start event))))
        (when (buffer-live-p flycheck-error-list-source-buffer)
          (switch-to-buffer flycheck-error-list-source-buffer))))

    (defun flycheck-get-error-list-window-list (&optional all-frames)
      "Get all windows displaying the error list.

    ALL-FRAMES specifies the frames to consider, as in
    `get-buffer-window-list'."
      (-when-let (buf (get-buffer flycheck-error-list-buffer))
        (get-buffer-window-list buf nil all-frames)))

    (defun flycheck-get-error-list-window (&optional all-frames)
      "Get a window displaying the error list, or nil if none.

    ALL-FRAMES specifies the frames to consider, as in
    `get-buffer-window'."
      (-when-let (buf (get-buffer flycheck-error-list-buffer))
        (get-buffer-window buf all-frames)))

    (defun flycheck-error-list-recenter-at (pos)
      "Recenter the error list at POS."
      (dolist (window (flycheck-get-error-list-window-list t))
        (with-selected-window window
          (goto-char pos)
          (recenter))))

    (defun flycheck-error-list-refresh ()
      "Refresh the current error list.

    Add all errors currently reported for the current
    `flycheck-error-list-source-buffer', and recenter the error
    list."
      ;; We only refresh the error list, when it is visible in a window, and we
      ;; select this window while reverting, because Tabulated List mode attempts to
      ;; recenter the error at the old location, so it must have the proper window
      ;; selected.
      (-when-let (window (flycheck-get-error-list-window t))
        (with-selected-window window
          (revert-buffer))
        (run-hooks 'flycheck-error-list-after-refresh-hook)
        (let ((preserve-pos (eq (current-buffer)
                                (get-buffer flycheck-error-list-buffer))))
          ;; If the error list is the current buffer, don't recenter when
          ;; highlighting
          (flycheck-error-list-highlight-errors preserve-pos))))

    (defun flycheck-error-list-mode-line-filter-indicator ()
      "Create a string representing the current error list filter."
      (if flycheck-error-list-minimum-level
          (format " [>= %s]" flycheck-error-list-minimum-level)
        ""))

    (defun flycheck-error-list-set-filter (level)
      "Restrict the error list to errors at level LEVEL or higher.

    LEVEL is either an error level symbol, or nil, to remove the filter."
      (interactive
       (list (read-flycheck-error-level
              "Minimum error level (errors at lower levels will be hidden): ")))
      (when (and level (not (flycheck-error-level-p level)))
        (user-error "Invalid level: %s" level))
      (-when-let (buf (get-buffer flycheck-error-list-buffer))
        (with-current-buffer buf
          (setq-local flycheck-error-list-minimum-level level))
        (flycheck-error-list-refresh)
        (flycheck-error-list-recenter-at (point-min))))

    (defun flycheck-error-list-reset-filter ()
      "Remove filters and show all errors in the error list."
      (interactive)
      (kill-local-variable 'flycheck-error-list-minimum-level))

    (defun flycheck-error-list-apply-filter (errors)
      "Filter ERRORS according to `flycheck-error-list-minimum-level'."
      (-if-let* ((min-level flycheck-error-list-minimum-level)
                 (min-severity (flycheck-error-level-severity min-level)))
          (seq-filter (lambda (err) (>= (flycheck-error-level-severity
                                         (flycheck-error-level err))
                                        min-severity))
                      errors)
        errors))

    (defun flycheck-error-list-goto-error (&optional pos)
      "Go to the location of the error at POS in the error list.

    POS defaults to `point'."
      (interactive)
      (-when-let* ((error (tabulated-list-get-id pos))
                   (buffer (flycheck-error-buffer error)))
        (when (buffer-live-p buffer)
          (if (eq (window-buffer) (get-buffer flycheck-error-list-buffer))
              ;; When called from within the error list, keep the error list,
              ;; otherwise replace the current buffer.
              (pop-to-buffer buffer 'other-window)
            (switch-to-buffer buffer))
          (let ((pos (flycheck-error-pos error)))
            (unless (eq (goto-char pos) (point))
              ;; If widening gets in the way of moving to the right place, remove it
              ;; and try again
              (widen)
              (goto-char pos)))
          ;; Re-highlight the errors
          (flycheck-error-list-highlight-errors 'preserve-pos))))

    (defun flycheck-error-list-explain-error (&optional pos)
      "Explain the error at POS in the error list.

    POS defaults to `point'."
      (interactive)
      (-when-let* ((error (tabulated-list-get-id pos))
                   (explainer (flycheck-checker-get (flycheck-error-checker error)
                                                    'error-explainer))
                   (explanation (funcall explainer error)))
        (flycheck-display-error-explanation explanation)))

    (defun flycheck-error-list-next-error-pos (pos &optional n)
      "Starting from POS get the N'th next error in the error list.

    N defaults to 1.  If N is negative, search for the previous error
    instead.

    Get the beginning position of the N'th next error from POS, or
    nil, if there is no next error."
      (let ((n (or n 1)))
        (if (>= n 0)
            ;; Search forward
            (while (and pos (/= n 0))
              (setq n (1- n))
              (setq pos (next-single-property-change pos 'tabulated-list-id)))
          ;; Search backwards
          (while (/= n 0)
            (setq n (1+ n))
            ;; We explicitly give the limit here to explicitly have the minimum
            ;; point returned, to be able to move to the first error (which starts
            ;; at `point-min')
            (setq pos (previous-single-property-change pos 'tabulated-list-id
                                                       nil (point-min)))))
        pos))

    (defun flycheck-error-list-previous-error (n)
      "Go to the N'th previous error in the error list."
      (interactive "P")
      (flycheck-error-list-next-error (- (or n 1))))

    (defun flycheck-error-list-next-error (n)
      "Go to the N'th next error in the error list."
      (interactive "P")
      (let ((pos (flycheck-error-list-next-error-pos (point) n)))
        (when (and pos (/= pos (point)))
          (goto-char pos)
          (save-selected-window
            ;; Keep the error list selected, so that the user can navigate errors by
            ;; repeatedly pressing n/p, without having to re-select the error list
            ;; window.
            (flycheck-error-list-goto-error)))))

    (defvar-local flycheck-error-list-highlight-overlays nil
      "Error highlight overlays in the error list buffer.")
    (put 'flycheck-error-list-highlight-overlays 'permanent-local t)

    (defun flycheck-error-list-highlight-errors (&optional preserve-pos)
      "Highlight errors in the error list.

    Highlight all errors in the error lists that are at point in the
    source buffer, and on the same line as point.  Then recenter the
    error list to the highlighted error, unless PRESERVE-POS is
    non-nil."
      (when (get-buffer flycheck-error-list-buffer)
        (let ((current-errors (flycheck-overlay-errors-in (line-beginning-position)
                                                          (line-end-position))))
          (with-current-buffer flycheck-error-list-buffer
            (let ((old-overlays flycheck-error-list-highlight-overlays)
                  (min-point (point-max))
                  (max-point (point-min)))
              ;; Display the new overlays first, to avoid re-display flickering
              (setq flycheck-error-list-highlight-overlays nil)
              (when current-errors
                (let ((next-error-pos (point-min)))
                  (while next-error-pos
                    (let* ((beg next-error-pos)
                           (end (flycheck-error-list-next-error-pos beg))
                           (err (tabulated-list-get-id beg)))
                      (when (member err current-errors)
                        (setq min-point (min min-point beg)
                              max-point (max max-point beg))
                        (let ((ov (make-overlay beg
                                                ;; Extend overlay to the beginning of
                                                ;; the next line, to highlight the
                                                ;; whole line
                                                (or end (point-max)))))
                          (push ov flycheck-error-list-highlight-overlays)
                          (setf (overlay-get ov 'flycheck-error-highlight-overlay)
                                t)
                          (setf (overlay-get ov 'face)
                                'flycheck-error-list-highlight)))
                      (setq next-error-pos end)))))
              ;; Delete the old overlays
              (seq-do #'delete-overlay old-overlays)
              (when (and (not preserve-pos) current-errors)
                ;; Move point to the middle error
                (goto-char (+ min-point (/ (- max-point min-point) 2)))
                (beginning-of-line)
                ;; And recenter the error list at this position
                (flycheck-error-list-recenter-at (point))))))))

    (defun flycheck-list-errors ()
      "Show the error list for the current buffer."
      (interactive)
      (unless flycheck-mode
        (user-error "Flycheck mode not enabled"))
      ;; Create and initialize the error list
      (unless (get-buffer flycheck-error-list-buffer)
        (with-current-buffer (get-buffer-create flycheck-error-list-buffer)
          (flycheck-error-list-mode)))
      (flycheck-error-list-set-source (current-buffer))
      ;; Reset the error filter
      (flycheck-error-list-reset-filter)
      ;; Show the error list in a window, and re-select the old window
      (display-buffer flycheck-error-list-buffer)
      ;; Finally, refresh the error list to show the most recent errors
      (flycheck-error-list-refresh))

    (defalias 'list-flycheck-errors 'flycheck-list-errors)


    ;;; Displaying errors in the current buffer
    (defun flycheck-display-errors (errors)
      "Display ERRORS using `flycheck-display-errors-function'."
      (when flycheck-display-errors-function
        (funcall flycheck-display-errors-function errors)))

    (defvar-local flycheck-display-error-at-point-timer nil
      "Timer to automatically show the error at point in minibuffer.")

    (defun flycheck-cancel-error-display-error-at-point-timer ()
      "Cancel the error display timer for the current buffer."
      (when flycheck-display-error-at-point-timer
        (cancel-timer flycheck-display-error-at-point-timer)
        (setq flycheck-display-error-at-point-timer nil)))

    (defun flycheck-display-error-at-point ()
      "Display the all error messages at point in minibuffer."
      (interactive)
      ;; This function runs from a timer, so we must take care to not ignore any
      ;; errors
      (with-demoted-errors "Flycheck error display error: %s"
        (flycheck-cancel-error-display-error-at-point-timer)
        (when flycheck-mode
          (-when-let (errors (flycheck-overlay-errors-at (point)))
            (flycheck-display-errors errors)))))

    (defun flycheck-display-error-at-point-soon ()
      "Display the first error message at point in minibuffer delayed."
      (flycheck-cancel-error-display-error-at-point-timer)
      (when (flycheck-overlays-at (point))
        (setq flycheck-display-error-at-point-timer
              (run-at-time flycheck-display-errors-delay nil 'flycheck-display-error-at-point))))


    ;;; Functions to display errors
    (defconst flycheck-error-message-buffer "*Flycheck error messages*"
      "The name of the buffer to show long error messages in.")

    (defun flycheck-error-message-buffer ()
      "Get the buffer object to show long error messages in.

    Get the buffer named by variable `flycheck-error-message-buffer',
    or nil if the buffer does not exist."
      (get-buffer flycheck-error-message-buffer))

    (defun flycheck-may-use-echo-area-p ()
      "Determine whether the echo area may be used.

    The echo area may be used if the cursor is not in the echo area,
    and if the echo area is not occupied by minibuffer input."
      (not (or cursor-in-echo-area (active-minibuffer-window))))

    (defun flycheck-display-error-messages (errors)
      "Display the messages of ERRORS.

    Concatenate all non-nil messages of ERRORS separated by empty
    lines, and display them with `display-message-or-buffer', which
    shows the messages either in the echo area or in a separate
    buffer, depending on the number of lines.  See Info
    node `(elisp)Displaying Messages' for more information.

    In the latter case, show messages in the buffer denoted by
    variable `flycheck-error-message-buffer'."
      (when (and errors (flycheck-may-use-echo-area-p))
        (let ((messages (seq-map #'flycheck-error-format-message-and-id errors)))
          (display-message-or-buffer (string-join messages "\n\n")
                                     flycheck-error-message-buffer
                                     'not-this-window))))

    (defun flycheck-display-error-messages-unless-error-list (errors)
      "Show messages of ERRORS unless the error list is visible.

    Like `flycheck-display-error-messages', but only if the error
    list (see `flycheck-list-errors') is not visible in any window in
    the current frame."
      (unless (flycheck-get-error-list-window 'current-frame)
        (flycheck-display-error-messages errors)))

    (defun flycheck-hide-error-buffer ()
      "Hide the Flycheck error buffer if necessary.

    Hide the error buffer if there is no error under point."
      (-when-let* ((buffer (flycheck-error-message-buffer))
                   (window (get-buffer-window buffer)))
        (unless (flycheck-overlays-at (point))
          ;; save-selected-window prevents `quit-window' from changing the current
          ;; buffer (see https://github.com/flycheck/flycheck/issues/648).
          (save-selected-window
            (quit-window nil window)))))


    ;;; Working with errors
    (defun flycheck-copy-errors-as-kill (pos &optional formatter)
      "Copy each error at POS into kill ring, using FORMATTER.

    FORMATTER is a function to turn an error into a string,
    defaulting to `flycheck-error-message'.

    Interactively, use `flycheck-error-format-message-and-id' as
    FORMATTER with universal prefix arg, and `flycheck-error-id' with
    normal prefix arg, i.e. copy the message and the ID with
    universal prefix arg, and only the id with normal prefix arg."
      (interactive (list (point)
                         (pcase current-prefix-arg
                           ((pred not) #'flycheck-error-message)
                           ((pred consp) #'flycheck-error-format-message-and-id)
                           (_ #'flycheck-error-id))))
      (let ((messages (delq nil (seq-map (or formatter #'flycheck-error-message)
                                         (flycheck-overlay-errors-at pos)))))
        (when messages
          (seq-do #'kill-new (reverse messages))
          (message (string-join messages "\n")))))

    (defun flycheck-explain-error-at-point ()
      "Display an explanation for the first explainable error at point.

    The first explainable error at point is the first error at point
    with a non-nil `:error-explainer' function defined in its
    checker.  The `:error-explainer' function is then called with
    this error to produce the explanation to display."
      (interactive)
      (-when-let* ((first-error
                    ;; Get the first error at point that has an `error-explainer'.
                    (seq-find (lambda (error)
                                (flycheck-checker-get
                                 (flycheck-error-checker error) 'error-explainer))
                              (flycheck-overlay-errors-at (point))))
                   (explainer
                    (flycheck-checker-get (flycheck-error-checker first-error)
                                          'error-explainer))
                   (explanation (funcall explainer first-error)))
        (flycheck-display-error-explanation explanation)))

    (defconst flycheck-explain-error-buffer "*Flycheck error explanation*"
      "The name of the buffer to show error explanations.")

    (defun flycheck-display-error-explanation (explanation)
      "Display the EXPLANATION string in a help buffer."
      (with-help-window (get-buffer-create flycheck-explain-error-buffer)
        (princ explanation)))


    ;;; Syntax checkers using external commands
    (defun flycheck-command-argument-p (arg)
      "Check whether ARG is a valid command argument."
      (pcase arg
        ((pred stringp) t)
        ((or `source `source-inplace `source-original) t)
        ((or `temporary-directory `temporary-file-name) t)
        (`null-device t)
        (`(config-file ,option-name ,config-file-var)
         (and (stringp option-name)
              (symbolp config-file-var)))
        (`(config-file ,option-name ,config-file-var ,prepender)
         (and (stringp option-name)
              (symbolp config-file-var)
              (symbolp prepender)))
        (`(,(or `option `option-list) ,option-name ,option-var)
         (and (stringp option-name)
              (symbolp option-var)))
        (`(,(or `option `option-list) ,option-name ,option-var ,prepender)
         (and (stringp option-name)
              (symbolp option-var)
              (symbolp prepender)))
        (`(,(or `option `option-list) ,option-name ,option-var ,prepender ,filter)
         (and (stringp option-name)
              (symbolp option-var)
              (symbolp prepender)
              (symbolp filter)))
        (`(option-flag ,option-name ,option-var)
         (and (stringp option-name)
              (symbolp option-var)))
        (`(eval ,_) t)
        (_ nil)))

    (defun flycheck-compute-working-directory (checker)
      "Get the default working directory for CHECKER.

    Compute the value of `default-directory' for the invocation of
    the syntax checker command, by calling the function in the
    `working-directory' property of CHECKER, with CHECKER as sole
    argument, and returning its value.  Signal an error if the
    function returns a non-existing working directory.

    If the property is undefined or if the function returns nil
    return the `default-directory' of the current buffer."
      (let* ((def-directory-fn (flycheck-checker-get checker 'working-directory))
             (directory (or (and def-directory-fn
                                 (funcall def-directory-fn checker))
                            ;; Default to the `default-directory' of the current
                            ;; buffer
                            default-directory)))
        (unless (file-exists-p directory)
          (error ":working-directory %s of syntax checker %S does not exist"
                 directory checker))
        directory))

    ;;;###autoload
    (defun flycheck-define-command-checker (symbol docstring &rest properties)
      "Define SYMBOL as syntax checker to run a command.

    Define SYMBOL as generic syntax checker via
    `flycheck-define-generic-checker', which uses an external command
    to check the buffer.  SYMBOL and DOCSTRING are the same as for
    `flycheck-define-generic-checker'.

    In addition to the properties understood by
    `flycheck-define-generic-checker', the following PROPERTIES
    constitute a command syntax checker.  Unless otherwise noted, all
    properties are mandatory.  Note that the default `:error-filter'
    of command checkers is `flycheck-sanitize-errors'.

    `:command COMMAND'
         The command to run for syntax checking.

         COMMAND is a list of the form `(EXECUTABLE [ARG ...])'.

         EXECUTABLE is a string with the executable of this syntax
         checker.  It can be overridden with the variable
         `flycheck-SYMBOL-executable'.  Note that this variable is
         NOT implicitly defined by this function.  Use
         `flycheck-def-executable-var' to define this variable.

         Each ARG is an argument to the executable, either as string,
         or as special symbol or form for
         `flycheck-substitute-argument', which see.

    `:error-patterns PATTERNS'
         A list of patterns to parse the output of the `:command'.

         Each ITEM in PATTERNS is a list `(LEVEL SEXP ...)', where
         LEVEL is a Flycheck error level (see
         `flycheck-define-error-level'), followed by one or more RX
         `SEXP's which parse an error of that level and extract line,
         column, file name and the message.

         See `rx' for general information about RX, and
         `flycheck-rx-to-string' for some special RX forms provided
         by Flycheck.

         All patterns are applied in the order of declaration to the
         whole output of the syntax checker.  Output already matched
         by a pattern will not be matched by subsequent patterns.  In
         other words, the first pattern wins.

         This property is optional.  If omitted, however, an
         `:error-parser' is mandatory.

    `:error-parser FUNCTION'
         A function to parse errors with.

         The function shall accept three arguments OUTPUT CHECKER
         BUFFER.  OUTPUT is the syntax checker output as string,
         CHECKER the syntax checker that was used, and BUFFER a
         buffer object representing the checked buffer.  The function
         must return a list of `flycheck-error' objects parsed from
         OUTPUT.

         This property is optional.  If omitted, it defaults to
         `flycheck-parse-with-patterns'.  In this case,
         `:error-patterns' is mandatory.

    `:standard-input t'
         Whether to send the buffer contents on standard input.

         If this property is given and has a non-nil value, send the
         contents of the buffer on standard input.

         Defaults to nil.

    Note that you may not give `:start', `:interrupt', and
    `:print-doc' for a command checker.  You can give a custom
    `:verify' function, though, whose results will be appended to the
    default `:verify' function of command checkers."
      (declare (indent 1)
               (doc-string 2))
      (dolist (prop '(:start :interrupt :print-doc))
        (when (plist-get properties prop)
          (error "%s not allowed in definition of command syntax checker %s"
                 prop symbol)))

      (unless (plist-get properties :error-filter)
        ;; Default to `flycheck-sanitize-errors' as error filter
        (setq properties (plist-put properties :error-filter
                                    #'flycheck-sanitize-errors)))
      (let ((verify-fn (plist-get properties :verify)))
        (setq properties
              (plist-put properties :verify
                         (lambda (checker)
                           (append (flycheck-verify-command-checker checker)
                                   (and verify-fn
                                        (funcall verify-fn checker)))))))

      (let ((command (plist-get properties :command))
            (patterns (plist-get properties :error-patterns))
            (parser (or (plist-get properties :error-parser)
                        #'flycheck-parse-with-patterns))
            (enabled (plist-get properties :enabled))
            (standard-input (plist-get properties :standard-input)))
        (unless command
          (error "Missing :command in syntax checker %s" symbol))
        (unless (stringp (car command))
          (error "Command executable for syntax checker %s must be a string: %S"
                 symbol (car command)))
        (dolist (arg (cdr command))
          (unless (flycheck-command-argument-p arg)
            (error "Invalid command argument %S in syntax checker %s" arg symbol)))
        (when (and (eq parser 'flycheck-parse-with-patterns)
                   (not patterns))
          (error "Missing :error-patterns in syntax checker %s" symbol))

        (setq properties
              ;; Automatically disable command checkers if the executable does not
              ;; exist.
              (plist-put properties :enabled
                         (lambda ()
                           (and (flycheck-find-checker-executable symbol)
                                (or (not enabled) (funcall enabled))))))

        (apply #'flycheck-define-generic-checker symbol docstring
               :start #'flycheck-start-command-checker
               :interrupt #'flycheck-interrupt-command-checker
               :print-doc #'flycheck-command-checker-print-doc
               properties)

        ;; Pre-compile all errors patterns into strings, so that we don't need to do
        ;; that on each error parse
        (let ((patterns (seq-map (lambda (p)
                                   (cons (flycheck-rx-to-string `(and ,@(cdr p))
                                                                'no-group)
                                         (car p)))
                                 patterns)))
          (pcase-dolist (`(,prop . ,value)
                         `((command        . ,command)
                           (error-parser   . ,parser)
                           (error-patterns . ,patterns)
                           (standard-input . ,standard-input)))
            (setf (flycheck-checker-get symbol prop) value)))))

    (eval-and-compile
      ;; Make this function available during byte-compilation, since we need it
      ;; at macro expansion of `flycheck-def-executable-var'.
      (defun flycheck-checker-executable-variable (checker)
        "Get the executable variable of CHECKER.

    The executable variable is named `flycheck-CHECKER-executable'."
        (intern (format "flycheck-%s-executable" checker))))

    (defun flycheck-checker-default-executable (checker)
      "Get the default executable of CHECKER."
      (car (flycheck-checker-get checker 'command)))

    (defun flycheck-checker-executable (checker)
      "Get the command executable of CHECKER.

    The executable is either the value of the variable
    `flycheck-CHECKER-executable', or the default executable given in
    the syntax checker definition, if the variable is nil."
      (let ((var (flycheck-checker-executable-variable checker)))
        (or (and (boundp var) (symbol-value var))
            (flycheck-checker-default-executable checker))))

    (defun flycheck-find-checker-executable (checker)
      "Get the full path of the executbale of CHECKER.

    Return the full absolute path to the executable of CHECKER, or
    nil if the executable does not exist."
      (funcall flycheck-executable-find (flycheck-checker-executable checker)))

    (defun flycheck-checker-arguments (checker)
      "Get the command arguments of CHECKER."
      (cdr (flycheck-checker-get checker 'command)))

    (defun flycheck-substitute-argument (arg checker)
      "Substitute ARG for CHECKER.

    Return a list of real arguments for the executable of CHECKER,
    substituted for the symbolic argument ARG.  Single arguments,
    e.g. if ARG is a literal strings, are wrapped in a list.

    ARG may be one of the following forms:

    STRING
         Return ARG unchanged.

    `source', `source-inplace'
         Create a temporary file to check and return its path.  With
         `source-inplace' create the temporary file in the same
         directory as the original file.  The value of
         `flycheck-temp-prefix' is used as prefix of the file name.

         With `source', try to retain the non-directory component of
         the buffer's file name in the temporary file.

         `source' is the preferred way to pass the input file to a
         syntax checker.  `source-inplace' should only be used if the
         syntax checker needs other files from the source directory,
         such as include files in C.

    `source-original'
         Return the path of the actual file to check, or an empty
         string if the buffer has no file name.

         Note that the contents of the file may not be up to date
         with the contents of the buffer to check.  Do not use this
         as primary input to a checker, unless absolutely necessary.

         When using this symbol as primary input to the syntax
         checker, add `flycheck-buffer-saved-p' to the `:predicate'.

    `temporary-directory'
         Create a unique temporary directory and return its path.

    `temporary-file-name'
         Return a unique temporary filename.  The file is *not*
         created.

         To ignore the output of syntax checkers, try `null-device'
         first.

    `null-device'
         Return the value of `null-device', i.e the system null
         device.

         Use this option to ignore the output of a syntax checker.
         If the syntax checker cannot handle the null device, or
         won't write to an existing file, try `temporary-file-name'
         instead.

    `(config-file OPTION VARIABLE [PREPEND-FN])'
         Search the configuration file bound to VARIABLE with
         `flycheck-locate-config-file' and return a list of arguments
         that pass this configuration file to the syntax checker, or
         nil if the configuration file was not found.

         PREPEND-FN is called with the OPTION and the located
         configuration file, and should return OPTION prepended
         before the file, either a string or as list.  If omitted,
         PREPEND-FN defaults to `list'.

    `(option OPTION VARIABLE [PREPEND-FN [FILTER]])'
         Retrieve the value of VARIABLE and return a list of
         arguments that pass this value as value for OPTION to the
         syntax checker.

         PREPEND-FN is called with the OPTION and the value of
         VARIABLE, and should return OPTION prepended before the
         file, either a string or as list.  If omitted, PREPEND-FN
         defaults to `list'.

         FILTER is an optional function to be applied to the value of
         VARIABLE before prepending.  This function must return nil
         or a string.  In the former case, return nil.  In the latter
         case, return a list of arguments as described above.

    `(option-list OPTION VARIABLE [PREPEND-FN [FILTER]])'
         Retrieve the value of VARIABLE, which must be a list,
         and prepend OPTION before each item in this list, using
         PREPEND-FN.

         PREPEND-FN is called with the OPTION and each item of the
         list as second argument, and should return OPTION prepended
         before the item, either as string or as list.  If omitted,
         PREPEND-FN defaults to `list'.

         FILTER is an optional function to be applied to each item in
         the list before prepending OPTION.  It shall return the
         option value for each item as string, or nil, if the item is
         to be ignored.

    `(option-flag OPTION VARIABLE)'
         Retrieve the value of VARIABLE and return OPTION, if the
         value is non-nil.  Otherwise return nil.

    `(eval FORM)'
         Return the result of evaluating FORM in the buffer to be
         checked.  FORM must either return a string or a list of
         strings, or nil to indicate that nothing should be
         substituted for CELL.  For all other return types, signal an
         error

         _No_ further substitutions are performed, neither in FORM
         before it is evaluated, nor in the result of evaluating
         FORM.

    In all other cases, signal an error.

    Note that substitution is *not* recursive.  No symbols or cells
    are substituted within the body of cells!"
      (pcase arg
        ((pred stringp) (list arg))
        (`source
         (list (flycheck-save-buffer-to-temp #'flycheck-temp-file-system)))
        (`source-inplace
         (list (flycheck-save-buffer-to-temp #'flycheck-temp-file-inplace)))
        (`source-original (list (or (buffer-file-name) "")))
        (`temporary-directory (list (flycheck-temp-dir-system)))
        (`temporary-file-name
         (let ((directory (flycheck-temp-dir-system)))
           (list (make-temp-name (expand-file-name "flycheck" directory)))))
        (`null-device (list null-device))
        (`(config-file ,option-name ,file-name-var)
         (-when-let* ((value (symbol-value file-name-var))
                      (file-name (flycheck-locate-config-file value checker)))
           (flycheck-prepend-with-option option-name (list file-name))))
        (`(config-file ,option-name ,file-name-var ,prepend-fn)
         (-when-let* ((value (symbol-value file-name-var))
                      (file-name (flycheck-locate-config-file value checker)))
           (flycheck-prepend-with-option option-name (list file-name) prepend-fn)))
        (`(option ,option-name ,variable)
         (-when-let (value (symbol-value variable))
           (unless (stringp value)
             (error "Value %S of %S for option %s is not a string"
                    value variable option-name))
           (flycheck-prepend-with-option option-name (list value))))
        (`(option ,option-name ,variable ,prepend-fn)
         (-when-let (value (symbol-value variable))
           (unless (stringp value)
             (error "Value %S of %S for option %s is not a string"
                    value variable option-name))
           (flycheck-prepend-with-option option-name (list value) prepend-fn)))
        (`(option ,option-name ,variable ,prepend-fn ,filter)
         (-when-let (value (funcall filter (symbol-value variable)))
           (unless (stringp value)
             (error "Value %S of %S (filter: %S) for option %s is not a string"
                    value variable filter option-name))
           (flycheck-prepend-with-option option-name (list value) prepend-fn)))
        (`(option-list ,option-name ,variable)
         (let ((value (symbol-value variable)))
           (unless (and (listp value) (seq-every-p #'stringp value))
             (error "Value %S of %S for option %S is not a list of strings"
                    value variable option-name))
           (flycheck-prepend-with-option option-name value)))
        (`(option-list ,option-name ,variable ,prepend-fn)
         (let ((value (symbol-value variable)))
           (unless (and (listp value) (seq-every-p #'stringp value))
             (error "Value %S of %S for option %S is not a list of strings"
                    value variable option-name))
           (flycheck-prepend-with-option option-name value prepend-fn)))
        (`(option-list ,option-name ,variable ,prepend-fn ,filter)
         (let ((value (delq nil (seq-map filter (symbol-value variable)))))
           (unless (and (listp value) (seq-every-p #'stringp value))
             (error "Value %S of %S for option %S is not a list of strings"
                    value variable option-name))
           (flycheck-prepend-with-option option-name value prepend-fn)))
        (`(option-flag ,option-name ,variable)
         (when (symbol-value variable)
           (list option-name)))
        (`(eval ,form)
         (let ((result (eval form)))
           (cond
            ((and (listp result) (seq-every-p #'stringp result)) result)
            ((stringp result) (list result))
            (t (error "Invalid result from evaluation of %S: %S" form result)))))
        (_ (error "Unsupported argument %S" arg))))

    (defun flycheck-checker-substituted-arguments (checker)
      "Get the substituted arguments of a CHECKER.

    Substitute each argument of CHECKER using
    `flycheck-substitute-argument'.  This replaces any special
    symbols in the command."
      (apply #'append
             (seq-map (lambda (arg) (flycheck-substitute-argument arg checker))
                      (flycheck-checker-arguments checker))))

    (defun flycheck--process-send-buffer-contents-chunked (process)
      "Send contents of current buffer to PROCESS in small batches.

    Send the entire buffer to the standard input of PROCESS in chunks
    of 4096 characters.  Chunking is done in Emacs Lisp, hence this
    function is probably far less efficient than
    `send-process-region'.  Use only when required."
      (let ((from (point-min)))
        (while (< from (point-max))
          (let ((to (min (+ from 4096) (point-max))))
            (process-send-region process from to)
            (setq from to)))))

    (defvar flycheck-chunked-process-input
      ;; Chunk process output on Windows to work around
      ;; https://github.com/flycheck/flycheck/issues/794 and
      ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=22344.  The presence of
      ;; `w32-pipe-buffer-size' denotes an Emacs version (> Emacs 25.1 )where pipe
      ;; writes on Windows are fixed.
      ;;
      ;; TODO: Remove option and chunking when dropping Emacs 24 support, see
      ;; https://github.com/flycheck/flycheck/issues/856
      (and (eq system-type 'windows-nt) (not (boundp 'w32-pipe-buffer-size)))
      "If non-nil send process input in small chunks.

    If this variable is non-nil `flycheck-process-send-buffer' sends
    buffer contents in small chunks.

    Defaults to nil, except on Windows to work around Emacs bug
    #22344.")

    (defun flycheck-process-send-buffer (process)
      "Send all contents of current buffer to PROCESS.

    Sends all contents of the current buffer to the standard input of
    PROCESS, and terminates standard input with EOF.

    If `flycheck-chunked-process-input' is non-nil, send buffer
    contents in chunks via
    `flycheck--process-send-buffer-contents-chunked', which see.
    Otherwise use `process-send-region' to send all contents at once
    and rely on Emacs' own buffering and chunking."
      (save-restriction
        (widen)
        (if flycheck-chunked-process-input
            (flycheck--process-send-buffer-contents-chunked process)
          (process-send-region process (point-min) (point-max))))
      (process-send-eof process))

    (defun flycheck-start-command-checker (checker callback)
      "Start a command CHECKER with CALLBACK."
      (let (process)
        (condition-case err
            (let* ((program (flycheck-find-checker-executable checker))
                   (args (flycheck-checker-substituted-arguments checker))
                   (command (funcall flycheck-command-wrapper-function
                                     (cons program args)))
                   ;; Use pipes to receive output from the syntax checker.  They are
                   ;; more efficient and more robust than PTYs, which Emacs uses by
                   ;; default, and since we don't need any job control features, we
                   ;; can easily use pipes.
                   (process-connection-type nil))
              ;; We pass do not associate the process with any buffer, by
              ;; passing nil for the BUFFER argument of `start-process'.
              ;; Instead, we just remember the buffer being checked in a
              ;; process property (see below).  This neatly avoids all
              ;; side-effects implied by attached a process to a buffer, which
              ;; may cause conflicts with other packages.
              ;;
              ;; See https://github.com/flycheck/flycheck/issues/298 for an
              ;; example for such a conflict.
              (setq process (apply 'start-process (format "flycheck-%s" checker)
                                   nil command))
              (setf (process-sentinel process) #'flycheck-handle-signal)
              (setf (process-filter process) #'flycheck-receive-checker-output)
              (set-process-query-on-exit-flag process nil)
              ;; Remember the syntax checker, the buffer and the callback
              (process-put process 'flycheck-checker checker)
              (process-put process 'flycheck-callback callback)
              (process-put process 'flycheck-buffer (current-buffer))
              ;; The default directory is bound in the `flycheck-syntax-check-start' function.
              (process-put process 'flycheck-working-directory default-directory)
              ;; Track the temporaries created by argument substitution in the
              ;; process itself, to get rid of the global state ASAP.
              (process-put process 'flycheck-temporaries flycheck-temporaries)
              (setq flycheck-temporaries nil)
              ;; Send the buffer to the process on standard input, if enabled.
              (when (flycheck-checker-get checker 'standard-input)
                (flycheck-process-send-buffer process))
              ;; Return the process.
              process)
          (error
           ;; In case of error, clean up our resources, and report the error back to
           ;; Flycheck.
           (flycheck-safe-delete-temporaries)
           (when process
             ;; No need to explicitly delete the temporary files of the process,
             ;; because deleting runs the sentinel, which will delete them anyway.
             (delete-process process))
           (signal (car err) (cdr err))))))

    (defun flycheck-interrupt-command-checker (_checker process)
      "Interrupt a PROCESS."
      ;; Deleting the process always triggers the sentinel, which does the cleanup
      (when process
        (delete-process process)))

    (defun flycheck-command-checker-print-doc (checker)
      "Print additional documentation for a command CHECKER."
      (let ((executable (flycheck-checker-default-executable checker))
            (config-file-var (flycheck-checker-get checker 'config-file-var))
            (option-vars (seq-sort #'string<
                                   (flycheck-checker-get checker 'option-vars))))
        (princ "\n")

        (let ((doc-start (with-current-buffer standard-output (point-max))))
          ;; Track the start of our documentation so that we can re-indent it
          ;; properly
          (princ "  This syntax checker executes \"")
          (princ executable)
          (princ "\"")
          (when config-file-var
            (princ ", using a configuration file from `")
            (princ (symbol-name config-file-var))
            (princ "'"))
          (princ ". The executable can be overridden with `")
          (princ (symbol-name (flycheck-checker-executable-variable checker)))
          (princ "'.")

          (with-current-buffer standard-output
            (save-excursion
              (fill-region-as-paragraph doc-start (point-max)))))
        (princ "\n")
        (when option-vars
          (princ "\n  This syntax checker can be configured with these options:\n\n")
          (dolist (var option-vars)
            (princ (format "     * `%s'\n" var))))))

    (defun flycheck-verify-command-checker (checker)
      "Verify a command CHECKER in the current buffer.

    Return a list of `flycheck-verification-result' objects for
    CHECKER."
      (let ((executable (flycheck-find-checker-executable checker))
            (config-file-var (flycheck-checker-get checker 'config-file-var)))
        `(
          ,(flycheck-verification-result-new
            :label "executable"
            :message (if executable (format "Found at %s" executable) "Not found")
            :face (if executable 'success '(bold error)))
          ,@(when config-file-var
              (let* ((value (symbol-value config-file-var))
                     (path (and value (flycheck-locate-config-file value checker))))
                (list (flycheck-verification-result-new
                       :label "configuration file"
                       :message (if path (format "Found at %S" path) "Not found")
                       :face (if path 'success 'warning))))))))


    ;;; Process management for command syntax checkers
    (defun flycheck-receive-checker-output (process output)
      "Receive a syntax checking PROCESS OUTPUT."
      (push output (process-get process 'flycheck-pending-output)))

    (defun flycheck-get-output (process)
      "Get the complete output of PROCESS."
      (with-demoted-errors "Error while retrieving process output: %S"
        (let ((pending-output (process-get process 'flycheck-pending-output)))
          (apply #'concat (nreverse pending-output)))))

    (defun flycheck-handle-signal (process _event)
      "Handle a signal from the syntax checking PROCESS.

    _EVENT is ignored."
      (when (memq (process-status process) '(signal exit))
        (let ((files (process-get process 'flycheck-temporaries))
              (buffer (process-get process 'flycheck-buffer))
              (callback (process-get process 'flycheck-callback))
              (cwd (process-get process 'flycheck-working-directory)))
          ;; Delete the temporary files
          (seq-do #'flycheck-safe-delete files)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (condition-case err
                  (pcase (process-status process)
                    (`signal
                     (funcall callback 'interrupted))
                    (`exit
                     (flycheck-finish-checker-process
                      (process-get process 'flycheck-checker)
                      (process-exit-status process)
                      files
                      (flycheck-get-output process) callback cwd)))
                ((debug error)
                 (funcall callback 'errored (error-message-string err)))))))))

    (defun flycheck-finish-checker-process
        (checker exit-status files output callback cwd)
      "Finish a checker process from CHECKER with EXIT-STATUS.

    FILES is a list of files given as input to the checker.  OUTPUT
    is the output of the syntax checker.  CALLBACK is the status
    callback to use for reporting.

    Parse the OUTPUT and report an appropriate error status.

    Resolve all errors in OUTPUT using CWD as working directory."
      (let ((errors (flycheck-parse-output output checker (current-buffer))))
        (when (and (/= exit-status 0) (not errors))
          ;; Warn about a suspicious result from the syntax checker.  We do right
          ;; after parsing the errors, before filtering, because a syntax checker
          ;; might report errors from other files (e.g. includes) even if there
          ;; are no errors in the file being checked.
          (funcall callback 'suspicious
                   (format "Flycheck checker %S returned non-zero \
    exit code %s, but its output contained no errors: %s\nTry \
    installing a more recent version of %S, and please open a bug \
    report if the issue persists in the latest release.  Thanks!"
                           checker exit-status output checker)))
        (funcall callback 'finished
                 ;; Fix error file names, by substituting them backwards from the
                 ;; temporaries.
                 (seq-map (lambda (e) (flycheck-fix-error-filename e files cwd))
                          errors))))


    ;;; Executables of command checkers.
    (defmacro flycheck-def-executable-var (checker default-executable)
      "Define the executable variable for CHECKER.

    DEFAULT-EXECUTABLE is the default executable.  It is only used in
    the docstring of the variable.

    The variable is defined with `defcustom' in the
    `flycheck-executables' group.  It's also defined to be risky as
    file-local variable, to avoid arbitrary executables being used
    for syntax checking."
      (let ((executable-var (flycheck-checker-executable-variable checker)))
        `(progn
           (defcustom ,executable-var nil
             ,(format "The executable of the %s syntax checker.

    Either a string containing the name or the path of the
    executable, or nil to use the default executable from the syntax
    checker declaration.

    The default executable is %S." checker default-executable)
             :type '(choice (const :tag "Default executable" nil)
                            (string :tag "Name or path"))
             :group 'flycheck-executables
             :risky t))))

    (defun flycheck-set-checker-executable (checker &optional executable)
      "Set the executable of CHECKER in the current buffer.

    CHECKER is a syntax checker symbol.  EXECUTABLE is a string with
    the name of a executable or the path to an executable file, which
    is to be used as executable for CHECKER.  If omitted or nil,
    reset the executable of CHECKER.

    Interactively, prompt for a syntax checker and an executable
    file, and set the executable of the selected syntax checker.
    With prefix arg, prompt for a syntax checker only, and reset the
    executable of the select checker to the default.

    Set the executable variable of CHECKER, that is,
    `flycheck-CHECKER-executable' to EXECUTABLE.  Signal
    `user-error', if EXECUTABLE does not denote a command or an
    executable file.

    This command is intended for interactive use only.  In Lisp, just
    `let'-bind the corresponding variable, or set it directly.  Use
    `flycheck-checker-executable-variable' to obtain the executable
    variable symbol for a syntax checker."
      (declare (interactive-only "Set the executable variable directly instead"))
      (interactive
       (let* ((checker (read-flycheck-checker "Syntax checker: "))
              (default-executable (flycheck-checker-default-executable checker))
              (executable (if current-prefix-arg
                              nil
                            (read-file-name "Executable: " nil default-executable
                                            nil nil flycheck-executable-find))))
         (list checker executable)))
      (when (and executable (not (funcall flycheck-executable-find executable)))
        (user-error "%s is no executable" executable))
      (let ((variable (flycheck-checker-executable-variable checker)))
        (set (make-local-variable variable) executable)))


    ;;; Configuration files and options for command checkers
    (defun flycheck-register-config-file-var (var checkers)
      "Register VAR as config file var for CHECKERS.

    CHECKERS is a single syntax checker or a list thereof."
      (when (symbolp checkers)
        (setq checkers (list checkers)))
      (dolist (checker checkers)
        (setf (flycheck-checker-get checker 'config-file-var) var)))

    ;;;###autoload
    (defmacro flycheck-def-config-file-var (symbol checker &optional file-name
                                                   &rest custom-args)
      "Define SYMBOL as config file variable for CHECKER, with default FILE-NAME.

    SYMBOL is declared as customizable variable using `defcustom', to
    provide a configuration file for the given syntax CHECKER.
    CUSTOM-ARGS are forwarded to `defcustom'.

    FILE-NAME is the initial value of the new variable.  If omitted,
    the default value is nil.

    Use this together with the `config-file' form in the `:command'
    argument to `flycheck-define-checker'."
      ;; FIXME: We should allow multiple config files per checker as well as
      ;; multiple checkers per config file
      (declare (indent 3))
      `(progn
         (defcustom ,symbol ,file-name
           ,(format "Configuration file for `%s'.

    If set to a string, locate the configuration file using the
    functions from `flycheck-locate-config-file-functions'.  If the
    file is found pass it to the syntax checker as configuration
    file.

    If no configuration file is found, or if this variable is set to
    nil, invoke the syntax checker without a configuration file.

    Use this variable as file-local variable if you need a specific
    configuration file a buffer." checker)
           :type '(choice (const :tag "No configuration file" nil)
                          (string :tag "File name or path"))
           :group 'flycheck-config-files
           ,@custom-args)
         (flycheck-register-config-file-var ',symbol ',checker)))

    (defun flycheck-locate-config-file (filename checker)
      "Locate the configuration file FILENAME for CHECKER.

    Locate the configuration file using
    `flycheck-locate-config-file-functions'.

    Return the absolute path of the configuration file, or nil if no
    configuration file was found."
      (-when-let (filepath (run-hook-with-args-until-success
                            'flycheck-locate-config-file-functions
                            filename checker))
        (when (file-exists-p filepath)
          filepath)))

    (defun flycheck-locate-config-file-by-path (filepath _checker)
      "Locate a configuration file by a FILEPATH.

    If FILEPATH is a contains a path separator, expand it against the
    default directory and return it if it points to an existing file.
    Otherwise return nil.

    _CHECKER is ignored."
      ;; If the path is just a plain file name, skip it.
      (unless (string= (file-name-nondirectory filepath) filepath)
        (let ((file-name (expand-file-name filepath)))
          (and (file-exists-p file-name) file-name))))

    (defun flycheck-locate-config-file-ancestor-directories (filename _checker)
      "Locate a configuration FILENAME in ancestor directories.

    If the current buffer has a file name, search FILENAME in the
    directory of the current buffer and all ancestors thereof (see
    `locate-dominating-file').  If the file is found, return its
    absolute path.  Otherwise return nil.

    _CHECKER is ignored."
      (-when-let* ((basefile (buffer-file-name))
                   (directory (locate-dominating-file basefile filename)))
        (expand-file-name filename directory)))

    (defun flycheck-locate-config-file-home (filename _checker)
      "Locate a configuration FILENAME in the home directory.

    Return the absolute path, if FILENAME exists in the user's home
    directory, or nil otherwise."
      (let ((path (expand-file-name filename "~")))
        (when (file-exists-p path)
          path)))

    (seq-do (apply-partially #'custom-add-frequent-value
                             'flycheck-locate-config-file-functions)
            '(flycheck-locate-config-file-by-path
              flycheck-locate-config-file-ancestor-directories
              flycheck-locate-config-file-home))

    (defun flycheck-register-option-var (var checkers)
      "Register an option VAR with CHECKERS.

    VAR is an option symbol, and CHECKERS a syntax checker symbol or
    a list thereof.  Register VAR with all CHECKERS so that it
    appears in the help output."
      (when (symbolp checkers)
        (setq checkers (list checkers)))
      (dolist (checker checkers)
        (cl-pushnew var (flycheck-checker-get checker 'option-vars))))

    ;;;###autoload
    (defmacro flycheck-def-option-var (symbol init-value checkers docstring
                                              &rest custom-args)
      "Define SYMBOL as option variable with INIT-VALUE for CHECKER.

    SYMBOL is declared as customizable variable using `defcustom', to
    provide an option for the given syntax CHECKERS (a checker or a
    list of checkers).  INIT-VALUE is the initial value of the
    variable, and DOCSTRING is its docstring.  CUSTOM-ARGS are
    forwarded to `defcustom'.

    Use this together with the `option', `option-list' and
    `option-flag' forms in the `:command' argument to
    `flycheck-define-checker'."
      (declare (indent 3)
               (doc-string 4))
      `(progn
         (defcustom ,symbol ,init-value
           ,(concat docstring "

    This variable is an option for the following syntax checkers:

    "
                    (mapconcat (lambda (c) (format "  - `%s'" c))
                               (if (symbolp checkers) (list checkers) checkers)
                               "\n"))
           :group 'flycheck-options
           ,@custom-args)
         (flycheck-register-option-var ',symbol ',checkers)))

    (defun flycheck-option-int (value)
      "Convert an integral option VALUE to a string.

    If VALUE is nil, return nil.  Otherwise return VALUE converted to
    a string."
      (and value (number-to-string value)))

    (defun flycheck-option-symbol (value)
      "Convert a symbol option VALUE to string.

    If VALUE is nil return nil.  Otherwise return VALUE converted to
    a string."
      (and value (symbol-name value)))

    (defun flycheck-option-comma-separated-list (value &optional separator filter)
      "Convert VALUE into a list separated by SEPARATOR.

    SEPARATOR is a string to separate items in VALUE, defaulting to
    \",\".  FILTER is an optional function, which takes a single
    argument and returns either a string or nil.

    If VALUE is a list, apply FILTER to each item in VALUE, remove
    all nil items, and return a single string of all remaining items
    separated by SEPARATOR.

    Otherwise, apply FILTER to VALUE and return the result.
    SEPARATOR is ignored in this case."
      (let ((filter (or filter #'identity))
            (separator (or separator ",")))
        (if (listp value)
            (-when-let (value (delq nil (seq-map filter value)))
              (string-join value separator))
          (funcall filter value))))

    (defmacro flycheck-def-args-var (symbol checkers &rest custom-args)
      "Define SYMBOL as argument variable for CHECKERS.

    SYMBOL is declared as customizable, risky and buffer-local
    variable using `defcustom' to provide an option for arbitrary
    arguments for the given syntax CHECKERS (either a single checker
    or a list of checkers).  CUSTOM-ARGS is forwarded to `defcustom'.

    Use the `eval' form to splice this variable into the
    `:command'."
      (declare (indent 2))
      `(flycheck-def-option-var ,symbol nil ,checkers
         "A list of additional command line arguments.

    The value of this variable is a list of strings with additional
    command line arguments."
         :risky t
         :type '(repeat (string :tag "Argument"))
         ,@custom-args))


    ;;; Command syntax checkers as compile commands
    (defun flycheck-checker-pattern-to-error-regexp (pattern)
      "Convert PATTERN into an error regexp for compile.el.

    Return a list representing PATTERN, suitable as element in
    `compilation-error-regexp-alist'."
      (let* ((regexp (car pattern))
             (level (cdr pattern))
             (level-no (flycheck-error-level-compilation-level level)))
        (list regexp 1 2 3 level-no)))

    (defun flycheck-checker-compilation-error-regexp-alist (checker)
      "Convert error patterns of CHECKER for use with compile.el.

    Return an alist of all error patterns of CHECKER, suitable for
    use with `compilation-error-regexp-alist'."
      (seq-map #'flycheck-checker-pattern-to-error-regexp
               (flycheck-checker-get checker 'error-patterns)))

    (defun flycheck-checker-shell-command (checker)
      "Get a shell command for CHECKER.

    Perform substitution in the arguments of CHECKER, but with
    `flycheck-substitute-shell-argument'.

    Return the command of CHECKER as single string, suitable for
    shell execution."
      ;; Note: Do NOT use `combine-and-quote-strings' here.  Despite it's name it
      ;; does not properly quote shell arguments, and actually breaks for special
      ;; characters.  See https://github.com/flycheck/flycheck/pull/522
      (let* ((args (apply #'append
                          (seq-map (lambda (arg)
                                     (if (memq arg '(source source-inplace source-original))
                                         (list (buffer-file-name))
                                       (flycheck-substitute-argument arg checker)))
                                   (flycheck-checker-arguments checker))))
             (command (mapconcat
                       #'shell-quote-argument
                       (funcall flycheck-command-wrapper-function
                                (cons (flycheck-checker-executable checker) args))
                       " ")))
        (if (flycheck-checker-get checker 'standard-input)
            ;; If the syntax checker expects the source from standard input add an
            ;; appropriate shell redirection
            (concat command " < " (shell-quote-argument (buffer-file-name)))
          command)))

    (defun flycheck-compile-name (_name)
      "Get a name for a Flycheck compilation buffer.

    _NAME is ignored."
      (format "*Flycheck %s*" (buffer-file-name)))

    (defun flycheck-compile (checker)
      "Run CHECKER via `compile'.

    CHECKER must be a valid syntax checker.  Interactively, prompt
    for a syntax checker to run.

    Instead of highlighting errors in the buffer, this command pops
    up a separate buffer with the entire output of the syntax checker
    tool, just like `compile' (\\[compile])."
      (interactive
       (let ((default (flycheck-get-checker-for-buffer)))
         (list (read-flycheck-checker "Run syntax checker as compile command: "
                                      (when (flycheck-checker-get default 'command)
                                        default)
                                      'command))))
      (unless (flycheck-valid-checker-p checker)
        (user-error "%S is not a valid syntax checker" checker))
      (unless (buffer-file-name)
        (user-error "Cannot compile buffers without backing file"))
      (unless (flycheck-may-use-checker checker)
        (user-error "Cannot use syntax checker %S in this buffer" checker))
      (unless (flycheck-checker-executable checker)
        (user-error "Cannot run checker %S as shell command" checker))
      (let* ((default-directory (flycheck-compute-working-directory checker))
             (command (flycheck-checker-shell-command checker))
             (buffer (compilation-start command nil #'flycheck-compile-name)))
        (with-current-buffer buffer
          (setq-local compilation-error-regexp-alist
                      (flycheck-checker-compilation-error-regexp-alist checker)))))


    ;;; General error parsing for command checkers
    (defun flycheck-parse-output (output checker buffer)
      "Parse OUTPUT from CHECKER in BUFFER.

    OUTPUT is a string with the output from the checker symbol
    CHECKER.  BUFFER is the buffer which was checked.

    Return the errors parsed with the error patterns of CHECKER."
      (funcall (flycheck-checker-get checker 'error-parser) output checker buffer))

    (defun flycheck-fix-error-filename (err buffer-files cwd)
      "Fix the file name of ERR from BUFFER-FILES.

    Resolves error file names relative to CWD directory.

    Make the file name of ERR absolute.  If the absolute file name of
    ERR is in BUFFER-FILES, replace it with the return value of the
    function `buffer-file-name'."
      (flycheck-error-with-buffer err
        (-when-let (filename (flycheck-error-filename err))
          (when (seq-some (apply-partially #'flycheck-same-files-p
                                           (expand-file-name filename cwd))
                          buffer-files)
            (setf (flycheck-error-filename err) buffer-file-name)
            (when (and buffer-file-name (flycheck-error-message err))
              (setf (flycheck-error-message err)
                    (replace-regexp-in-string
                     (regexp-quote filename) buffer-file-name
                     (flycheck-error-message err) 'fixed-case 'literal))))))
      err)


    ;;; Error parsers for command syntax checkers
    (defun flycheck-parse-xml-region (beg end)
      "Parse the xml region between BEG and END.

    Wrapper around `xml-parse-region' which transforms the return
    value of this function into one compatible to
    `libxml-parse-xml-region' by simply returning the first element
    from the node list."
      (car (xml-parse-region beg end)))

    (defvar flycheck-xml-parser
      (if (fboundp 'libxml-parse-xml-region)
          'libxml-parse-xml-region 'flycheck-parse-xml-region)
      "Parse an xml string from a region.

    Use libxml if Emacs is built with libxml support.  Otherwise fall
    back to `xml-parse-region', via `flycheck-parse-xml-region'.")

    (defun flycheck-parse-xml-string (xml)
      "Parse an XML string.

    Return the document tree parsed from XML in the form `(ROOT ATTRS
    BODY...)'.  ROOT is a symbol identifying the name of the root
    element.  ATTRS is an alist of the attributes of the root node.
    BODY is zero or more body elements, either as strings (in case of
    text nodes) or as XML nodes, in the same for as the root node."
      (with-temp-buffer
        (insert xml)
        (funcall flycheck-xml-parser (point-min) (point-max))))

    (defun flycheck-parse-checkstyle (output checker buffer)
      "Parse Checkstyle errors from OUTPUT.

    Parse Checkstyle-like XML output.  Use this error parser for
    checkers that have an option to output errors in this format.

    CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
    the BUFFER that was checked respectively.

    See URL `http://checkstyle.sourceforge.net/' for information
    about Checkstyle."
      (pcase (flycheck-parse-xml-string output)
        (`(checkstyle ,_ . ,file-nodes)
         (let (errors)
           (dolist (node file-nodes)
             (pcase node
               (`(file ,file-attrs . ,error-nodes)
                (dolist (node error-nodes)
                  (pcase node
                    (`(error ,error-attrs . ,_)
                     (let-alist error-attrs
                       (push (flycheck-error-new-at
                              (flycheck-string-to-number-safe .line)
                              (flycheck-string-to-number-safe .column)
                              (pcase .severity
                                (`"error"   'error)
                                (`"warning" 'warning)
                                (`"info"    'info)
                                ;; Default to error for unknown .severity
                                (_          'error))
                              .message
                              :checker checker :id .source
                              :buffer buffer
                              :filename (cdr (assq 'name file-attrs)))
                             errors))))))))
           (nreverse errors)))))

    (defun flycheck-parse-cppcheck (output checker buffer)
      "Parse Cppcheck errors from OUTPUT.

    Parse Cppcheck XML v2 output.

    CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
    the BUFFER that was checked respectively.

    See URL `http://cppcheck.sourceforge.net/' for more information
    about Cppcheck."
      (pcase (flycheck-parse-xml-string output)
        (`(results ,_ . ,body)
         (let (errors)
           (dolist (node body)
             (pcase node
               (`(errors ,_ . ,error-nodes)
                (dolist (node error-nodes)
                  (pcase node
                    (`(error ,error-attrs . ,loc-nodes)
                     (let ((id (cdr (assq 'id error-attrs)))
                           (message (cdr (assq 'verbose error-attrs)))
                           (level (pcase (cdr (assq 'severity error-attrs))
                                    (`"error" 'error)
                                    (`"style" 'info)
                                    (`"information" 'info)
                                    (_ 'warning))))
                       (dolist (node loc-nodes)
                         (pcase node
                           (`(location ,loc-attrs . ,_)
                            (let-alist loc-attrs
                              (push (flycheck-error-new-at
                                     (flycheck-string-to-number-safe .line)
                                     nil
                                     level
                                     ;; cppcheck return newline characters as "\012"
                                     (replace-regexp-in-string "\\\\012" "\n" message)
                                     :id id
                                     :checker checker
                                     :buffer buffer
                                     :filename .file)
                                    errors))))))))))))
           (nreverse errors)))))

    (defun flycheck-parse-phpmd (output checker buffer)
      "Parse phpmd errors from OUTPUT.

    CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
    the BUFFER that was checked respectively.

    See URL `http://phpmd.org/' for more information about phpmd."
      (pcase (flycheck-parse-xml-string output)
        (`(pmd ,_ . ,body)
         (let (errors)
           (dolist (node body)
             (pcase node
               (`(file ,file-attrs . ,violation-nodes)
                (let ((filename (cdr (assq 'name file-attrs))))
                  (dolist (node violation-nodes)
                    (pcase node
                      (`(violation ,vio-attrs ,(and message (pred stringp)))
                       (let-alist vio-attrs
                         (push
                          (flycheck-error-new-at
                           (flycheck-string-to-number-safe .beginline)
                           nil
                           'warning (string-trim message)
                           :id .rule
                           :checker checker
                           :buffer buffer
                           :filename filename)
                          errors)))))))))
           (nreverse errors)))))

    (defun flycheck-parse-reek (output checker buffer)
      "Parse Reek warnings from JSON OUTPUT.

    CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
    the BUFFER that was checked respectively.

    See URL `https://github.com/troessner/reek' for more information
    about Reek."
      (let ((errors nil))
        (dolist (message (car (flycheck-parse-json output)))
          (let-alist message
            (dolist (line (delete-dups .lines))
              (push
               (flycheck-error-new-at
                line
                nil
                'warning (concat .context " " .message)
                :id .smell_type
                :checker checker
                :buffer buffer
                :filename .source)
               errors))))
        (nreverse errors)))

    (defun flycheck-parse-tslint (output checker buffer)
      "Parse TSLint errors from JSON OUTPUT.

    CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
    the BUFFER that was checked respectively.

    See URL `https://palantir.github.io/tslint/' for more information
    about TSLint."
      (let ((json-array-type 'list))
        (seq-map (lambda (message)
                   (let-alist message
                     (flycheck-error-new-at
                      (+ 1 .startPosition.line)
                      (+ 1 .startPosition.character)
                      'warning .failure
                      :id .ruleName
                      :checker checker
                      :buffer buffer
                      :filename .name)))
                 ;; Don't try to parse empty output as JSON
                 (and (not (string-empty-p output))
                      (car (flycheck-parse-json output))))))

    (defun flycheck-parse-rust-collect-spans (span)
      "Return a list of spans contained in a SPAN object."
      (let ((spans))
        (let-alist span
          ;; With macro expansion errors, some spans will point to phony file names
          ;; to indicate an error inside the std rust lib.  We skip these spans as
          ;; they won't appear in flycheck anyway.
          (unless (string= .file_name "<std macros>")
            (push span spans))

          ;; Macro expansion errors will have a span in the 'expansion' field, so we
          ;; recursively collect it.
          (if .expansion.span
              (append (flycheck-parse-rust-collect-spans .expansion.span)
                      spans)
            spans))))

    (defun flycheck-parse-rustc-diagnostic (diagnostic checker buffer)
      "Turn a rustc DIAGNOSTIC into a `flycheck-error'.

    CHECKER and BUFFER denote the CHECKER that returned DIAGNOSTIC
    and the BUFFER that was checked respectively.

    DIAGNOSTIC should be a parsed JSON object describing a rustc
    diagnostic, following the format described there:

    https://github.com/rust-lang/rust/blob/master/src/libsyntax/json.rs#L67-L139"
      (let ((error-message)
            (error-level)
            (error-code)
            (primary-filename)
            (primary-line)
            (primary-column)
            (group (make-symbol "group"))
            (spans)
            (children)
            (errors))
        ;; The diagnostic format is described in the link above.  The gist of it is
        ;; that a diagnostic can have several causes in the source text; these
        ;; causes are represented by spans.  The diagnostic has a message and a
        ;; level (error, warning), while the spans have a filename, line, column,
        ;; and an optional label.  The primary span points to the root cause of the
        ;; error in the source text, while non-primary spans point to related
        ;; causes.  Spans may have an 'expansion' field for macro expansion errors;
        ;; these expansion fields will contain another span (and so on).  In
        ;; addition, a diagnostic can also have children diagnostics that are used
        ;; to provide additional information through their message field, but do not
        ;; seem to contain any spans (yet).
        ;;
        ;; We first gather spans in order to turn every span into a flycheck error
        ;; object, that we collect into the `errors' list.

        ;; Nested `let-alist' cause compilation warnings, hence we `setq' all
        ;; these values here first to avoid nesting.
        (let-alist diagnostic
          (setq error-message .message
                error-level (pcase .level
                              (`"error" 'error)
                              (`"warning" 'warning)
                              (`"note" 'info)
                              (_ 'error))
                ;; The 'code' field of the diagnostic contains the actual error
                ;; code and an optional explanation that we ignore
                error-code .code.code
                ;; Collect all spans recursively
                spans (seq-mapcat #'flycheck-parse-rust-collect-spans .spans)
                children .children))

        ;; Turn each span into a flycheck error
        (dolist (span spans)
          (let-alist span
            ;; Children lack any filename/line/column information, so we use
            ;; those from the primary span
            (when .is_primary
              (setq primary-filename .file_name
                    primary-line .line_start
                    primary-column .column_start))
            (push
             (flycheck-error-new-at
              .line_start
              .column_start
              ;; Non-primary spans are used for notes
              (if .is_primary error-level 'info)
              (if .is_primary
                  ;; Primary spans may have labels with additional information
                  (concat error-message (when .label
                                          (format " (%s)" .label)))
                ;; If the label is empty, fallback on the error message,
                ;; otherwise we won't be able to display anything
                (or .label error-message))
              :id error-code
              :checker checker
              :buffer buffer
              :filename .file_name
              :group group)
             errors)))

        ;; Then we turn children messages into flycheck errors pointing to the
        ;; location of the primary span.  According to the format, children
        ;; may contain spans, but they do not seem to use them in practice.
        (dolist (child children)
          (let-alist child
            (push
             (flycheck-error-new-at
              primary-line
              primary-column
              'info
              .message
              :id error-code
              :checker checker
              :buffer buffer
              :filename primary-filename
              :group group)
             errors)))

        ;; If there are no spans, the error is not associated with a specific
        ;; file but with the project as a whole.  We still need to report it to
        ;; the user by emitting a corresponding flycheck-error object.
        (unless spans
          (push (flycheck-error-new-at
                 ;; We have no specific position to attach the error to, so
                 ;; let's use the top of the file.
                 1 1
                 error-level
                 error-message
                 :id error-code
                 :checker checker
                 :buffer buffer
                 :group group)
                errors))
        (nreverse errors)))

    (defun flycheck-parse-json (output)
      "Return parsed JSON data from OUTPUT.

    OUTPUT is a string that contains JSON data.  Each line of OUTPUT
    may be either plain text, a JSON array (starting with `['), or a
    JSON object (starting with `{').

    This function ignores the plain text lines, parses the JSON
    lines, and returns the parsed JSON lines in a list."
      (let ((objects nil)
            (json-array-type 'list)
            (json-false nil))
        (with-temp-buffer
          (insert output)
          (goto-char (point-min))
          (while (not (eobp))
            (when (memq (char-after) '(?\{ ?\[))
              (push (json-read) objects))
            (forward-line)))
        (nreverse objects)))

    (defun flycheck-parse-rustc (output checker buffer)
      "Parse rustc errors from OUTPUT and return a list of `flycheck-error'.

    CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
    the BUFFER that was checked respectively.

    The expected format for OUTPUT is a mix of plain text lines and
    JSON lines.  This function ignores the plain text lines and
    parses only JSON lines.  Each JSON line is expected to be a JSON
    object that corresponds to a diagnostic from the compiler.  The
    expected diagnostic format is described there:

    https://github.com/rust-lang/rust/blob/master/src/libsyntax/json.rs#L67-L139"
      (seq-mapcat (lambda (msg)
                    (flycheck-parse-rustc-diagnostic msg checker buffer))
                  (flycheck-parse-json output)))

    (defun flycheck-parse-cargo-rustc (output checker buffer)
      "Parse Cargo errors from OUTPUT and return a list of `flycheck-error'.

    CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
    the BUFFER that was checked respectively.

    The expected format for OUTPUT is a mix of plain text lines and
    JSON lines.  This function ignores the plain text lines and
    parses only JSON lines.  Each JSON line is expected to be a JSON
    object that represents a message from Cargo.  The format of
    messages emitted by Cargo is described there:

    https://github.com/rust-lang/cargo/blob/master/src/cargo/util/machine_message.rs#L20-L31"
      (let ((errors))
        (dolist (msg (flycheck-parse-json output))
          (let-alist msg
            ;; Errors and warnings from rustc are wrapped by cargo, so we filter and
            ;; unwrap them, and delegate the actual construction of `flycheck-error'
            ;; objects to `flycheck-parse-rustc-diagnostic'.
            (when (string= .reason "compiler-message")
              (push (flycheck-parse-rustc-diagnostic .message checker buffer)
                    errors))))
        (apply #'nconc errors)))


    ;;; Error parsing with regular expressions
    (defun flycheck-get-regexp (patterns)
      "Create a single regular expression from PATTERNS."
      (rx-to-string `(or ,@(seq-map (lambda (p) (list 'regexp (car p))) patterns))
                    'no-group))

    (defun flycheck-tokenize-output-with-patterns (output patterns)
      "Tokenize OUTPUT with PATTERNS.

    Split the output into error tokens, using all regular expressions
    from the error PATTERNS.  An error token is simply a string
    containing a single error from OUTPUT.  Such a token can then be
    parsed into a structured error by applying the PATTERNS again,
    see `flycheck-parse-errors-with-patterns'.

    Return a list of error tokens."
      (let ((regexp (flycheck-get-regexp patterns))
            (last-match 0)
            errors)
        (while (string-match regexp output last-match)
          (push (match-string 0 output) errors)
          (setq last-match (match-end 0)))
        (reverse errors)))

    (defun flycheck-try-parse-error-with-pattern (err pattern checker)
      "Try to parse a single ERR with a PATTERN for CHECKER.

    Return the parsed error if PATTERN matched ERR, or nil
    otherwise."
      (let ((regexp (car pattern))
            (level (cdr pattern)))
        (when (string-match regexp err)
          (let ((filename (match-string 1 err))
                (line (match-string 2 err))
                (column (match-string 3 err))
                (message (match-string 4 err))
                (id (match-string 5 err)))
            (flycheck-error-new-at
             (flycheck-string-to-number-safe line)
             (flycheck-string-to-number-safe column)
             level
             (unless (string-empty-p message) message)
             :id (unless (string-empty-p id) id)
             :checker checker
             :filename (if (or (null filename) (string-empty-p filename))
                           (buffer-file-name)
                         filename))))))

    (defun flycheck-parse-error-with-patterns (err patterns checker)
      "Parse a gle ERR with error PATTERNS for CHECKER.

    Apply each pattern in PATTERNS to ERR, in the given order, and
    return the first parsed error."
      ;; Try to parse patterns in the order of declaration to make sure that the
      ;; first match wins.
      (let (parsed-error)
        (while (and patterns
                    (not (setq parsed-error
                               (flycheck-try-parse-error-with-pattern
                                err (car patterns) checker))))
          (setq patterns (cdr patterns)))
        parsed-error))

    (defun flycheck-parse-with-patterns (output checker buffer)
      "Parse OUTPUT from CHECKER with error patterns.

    Uses the error patterns of CHECKER to tokenize the output and
    tries to parse each error token with all patterns, in the order
    of declaration.  Hence an error is never matched twice by two
    different patterns.  The pattern declared first always wins.

    _BUFFER is ignored.

    Return a list of parsed errors and warnings (as `flycheck-error'
    objects)."
      (with-current-buffer buffer
        (let ((patterns (flycheck-checker-get checker 'error-patterns)))
          (seq-map (lambda (err)
                     (flycheck-parse-error-with-patterns err patterns checker))
                   (flycheck-tokenize-output-with-patterns output patterns)))))


    ;;; Convenience definition of command-syntax checkers
    (defmacro flycheck-define-checker (symbol docstring &rest properties)
      "Define SYMBOL as command syntax checker with DOCSTRING and PROPERTIES.

    Like `flycheck-define-command-checker', but PROPERTIES must not
    be quoted.  Also, implicitly define the executable variable for
    SYMBOL with `flycheck-def-executable-var'."
      (declare (indent 1)
               (doc-string 2))
      (let ((command (plist-get properties :command))
            (parser (plist-get properties :error-parser))
            (filter (plist-get properties :error-filter))
            (explainer (plist-get properties :error-explainer))
            (predicate (plist-get properties :predicate))
            (enabled-fn (plist-get properties :enabled))
            (verify-fn (plist-get properties :verify)))

        `(progn
           (flycheck-def-executable-var ,symbol ,(car command))

           (flycheck-define-command-checker ',symbol
             ,docstring
             :command ',command
             ,@(when parser
                 `(:error-parser #',parser))
             :error-patterns ',(plist-get properties :error-patterns)
             ,@(when filter
                 `(:error-filter #',filter))
             ,@(when explainer
                 `(:error-explainer #',explainer))
             :modes ',(plist-get properties :modes)
             ,@(when predicate
                 `(:predicate #',predicate))
             :next-checkers ',(plist-get properties :next-checkers)
             ,@(when enabled-fn
                 `(:enabled #',enabled-fn))
             ,@(when verify-fn
                 `(:verify #',verify-fn))
             :standard-input ',(plist-get properties :standard-input)
             :working-directory ',(plist-get properties :working-directory)))))


    ;;; Built-in checkers
    (flycheck-def-args-var flycheck-gnat-args ada-gnat
      :package-version '(flycheck . "0.20"))

    (flycheck-def-option-var flycheck-gnat-include-path nil ada-gnat
      "A list of include directories for GNAT.

    The value of this variable is a list of strings, where each
    string is a directory to add to the include path of gcc.
    Relative paths are relative to the file being checked."
      :type '(repeat (directory :tag "Include directory"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.20"))

    (flycheck-def-option-var flycheck-gnat-language-standard "2012" ada-gnat
      "The language standard to use in GNAT.

    The value of this variable is either a string denoting a language
    standard, or nil, to use the default standard. When non-nil, pass
    the language standard via the `-std' option."
      :type '(choice (const :tag "Default standard" nil)
                     (string :tag "Language standard"))
      :safe #'stringp
      :package-version '(flycheck . "0.20"))

    (flycheck-def-option-var flycheck-gnat-warnings
        '("wa") ada-gnat
      "A list of additional Ada warnings to enable in GNAT.

    The value of this variable is a list of strings, where each
    string is the name of a warning category to enable. By default,
    most optional warnings are recommended, as in `-gnata'.

    Refer to Info Node `(gnat_ugn_unw)Warning Message Control' for
    more information about GNAT warnings."
      :type '(repeat :tag "Warnings" (string :tag "Warning name"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.20"))

    (flycheck-define-checker ada-gnat
      "An Ada syntax checker using GNAT.

    Uses the GNAT compiler from GCC.  See URL
    `http://libre.adacore.com/tools/gnat-gpl-edition/'."
      :command ("gnatmake"
                "-c"                        ; Just compile, don't bind
                "-f"                        ; Force re-compilation
                "-u"                        ; Compile the main file only
                "-gnatf"                    ; Full error information
                "-gnatef"                   ; Full source file name
                "-D" temporary-directory
                (option-list "-gnat" flycheck-gnat-warnings concat)
                (option-list "-I" flycheck-gnat-include-path concat)
                (option "-gnat" flycheck-gnat-language-standard concat)
                (eval flycheck-gnat-args)
                source)
      :error-patterns
      ((error line-start
              (message "In file included from") " " (file-name) ":" line ":"
              column ":"
              line-end)
       (info line-start (file-name) ":" line ":" column
             ": note: " (message) line-end)
       (warning line-start (file-name) ":" line ":" column
                ": warning: " (message) line-end)
       (error line-start (file-name) ":" line ":" column ;no specific error prefix in Ada
              ": " (message) line-end))
      :modes ada-mode)

    (flycheck-define-checker asciidoc
      "A AsciiDoc syntax checker using the AsciiDoc compiler.

    See URL `http://www.methods.co.nz/asciidoc'."
      :command ("asciidoc" "-o" null-device "-")
      :standard-input t
      :error-patterns
      ((error line-start
              "asciidoc: ERROR: <stdin>: Line " line ": " (message)
              line-end)
       (warning line-start
                "asciidoc: WARNING: <stdin>: Line " line ": " (message)
                line-end)
       (info line-start
             "asciidoc: DEPRECATED: <stdin>: Line " line ": " (message)
             line-end))
      :modes adoc-mode)

    (flycheck-define-checker asciidoctor
      "An AsciiDoc syntax checker using the Asciidoctor compiler.

    See URL `http://asciidoctor.org'."
      :command ("asciidoctor" "-o" null-device "-")
      :standard-input t
      :error-patterns
      ((error line-start
              "asciidoctor: ERROR: <stdin>: Line " line ": " (message)
              line-end)
       (warning line-start
                "asciidoctor: WARNING: <stdin>: Line " line ": " (message)
                line-end))
      :modes adoc-mode)

    (flycheck-def-args-var flycheck-clang-args c/c++-clang
      :package-version '(flycheck . "0.22"))

    (flycheck-def-option-var flycheck-clang-blocks nil c/c++-clang
      "Enable blocks in Clang.

    When non-nil, enable blocks in Clang with `-fblocks'.  See URL
    `http://clang.llvm.org/docs/BlockLanguageSpec.html' for more
    information about blocks."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.20"))

    (flycheck-def-option-var flycheck-clang-definitions nil c/c++-clang
      "Additional preprocessor definitions for Clang.

    The value of this variable is a list of strings, where each
    string is an additional definition to pass to Clang, via the `-D'
    option."
      :type '(repeat (string :tag "Definition"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.15"))

    (flycheck-def-option-var flycheck-clang-include-path nil c/c++-clang
      "A list of include directories for Clang.

    The value of this variable is a list of strings, where each
    string is a directory to add to the include path of Clang.
    Relative paths are relative to the file being checked."
      :type '(repeat (directory :tag "Include directory"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.14"))

    (flycheck-def-option-var flycheck-clang-includes nil c/c++-clang
      "A list of additional include files for Clang.

    The value of this variable is a list of strings, where each
    string is a file to include before syntax checking.  Relative
    paths are relative to the file being checked."
      :type '(repeat (file :tag "Include file"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.15"))

    (flycheck-def-option-var flycheck-clang-language-standard nil c/c++-clang
      "The language standard to use in Clang.

    The value of this variable is either a string denoting a language
    standard, or nil, to use the default standard.  When non-nil,
    pass the language standard via the `-std' option."
      :type '(choice (const :tag "Default standard" nil)
                     (string :tag "Language standard"))
      :safe #'stringp
      :package-version '(flycheck . "0.15"))
    (make-variable-buffer-local 'flycheck-clang-language-standard)

    (flycheck-def-option-var flycheck-clang-ms-extensions nil c/c++-clang
      "Whether to enable Microsoft extensions to C/C++ in Clang.

    When non-nil, enable Microsoft extensions to C/C++ via
    `-fms-extensions'."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.16"))

    (flycheck-def-option-var flycheck-clang-no-exceptions nil c/c++-clang
      "Whether to disable exceptions in Clang.

    When non-nil, disable exceptions for syntax checks, via
    `-fno-exceptions'."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.20"))

    (flycheck-def-option-var flycheck-clang-no-rtti nil c/c++-clang
      "Whether to disable RTTI in Clang.

    When non-nil, disable RTTI for syntax checks, via `-fno-rtti'."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.15"))

    (flycheck-def-option-var flycheck-clang-pedantic nil c/c++-clang
      "Whether to warn about language extensions in Clang.

    For ISO C, follows the version specified by any -std option used.
    When non-nil, disable non-ISO extensions to C/C++ via
    `-pedantic'."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.23"))

    (flycheck-def-option-var flycheck-clang-pedantic-errors nil c/c++-clang
      "Whether to error on language extensions in Clang.

    For ISO C, follows the version specified by any -std option used.
    When non-nil, disable non-ISO extensions to C/C++ via
    `-pedantic-errors'."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.23"))

    (flycheck-def-option-var flycheck-clang-standard-library nil c/c++-clang
      "The standard library to use for Clang.

    The value of this variable is the name of a standard library as
    string, or nil to use the default standard library.

    Refer to the Clang manual at URL
    `http://clang.llvm.org/docs/UsersManual.html' for more
    information about the standard library."
      :type '(choice (const "libc++")
                     (const :tag "GNU libstdc++" "libstdc++")
                     (string :tag "Library name"))
      :safe #'stringp
      :package-version '(flycheck . "0.15"))

    (flycheck-def-option-var flycheck-clang-warnings '("all" "extra") c/c++-clang
      "A list of additional warnings to enable in Clang.

    The value of this variable is a list of strings, where each string
    is the name of a warning category to enable.  By default, all
    recommended warnings and some extra warnings are enabled (as by
    `-Wall' and `-Wextra' respectively).

    Refer to the Clang manual at URL
    `http://clang.llvm.org/docs/UsersManual.html' for more
    information about warnings."
      :type '(choice (const :tag "No additional warnings" nil)
                     (repeat :tag "Additional warnings"
                             (string :tag "Warning name")))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.14"))

    (defun flycheck-c/c++-quoted-include-directory ()
      "Get the directory for quoted includes.

    C/C++ compiles typicall look up includes with quotation marks in
    the directory of the file being compiled.  However, since
    Flycheck uses temporary copies for syntax checking, it needs to
    explicitly determine the directory for quoted includes.

    This function determines the directory by looking at function
    `buffer-file-name', or if that is nil, at `default-directory'."
      (-if-let (fn (buffer-file-name))
          (file-name-directory fn)
        ;; If the buffer has no file name, fall back to its default directory
        default-directory))

    (flycheck-define-checker c/c++-clang
      "A C/C++ syntax checker using Clang.

    See URL `http://clang.llvm.org/'."
      :command ("clang"
                "-fsyntax-only"
                "-fno-color-diagnostics"    ; Do not include color codes in output
                "-fno-caret-diagnostics"    ; Do not visually indicate the source
                                            ; location
                "-fno-diagnostics-show-option" ; Do not show the corresponding
                                            ; warning group
                "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
                (option "-std=" flycheck-clang-language-standard concat)
                (option-flag "-pedantic" flycheck-clang-pedantic)
                (option-flag "-pedantic-errors" flycheck-clang-pedantic-errors)
                (option "-stdlib=" flycheck-clang-standard-library concat)
                (option-flag "-fms-extensions" flycheck-clang-ms-extensions)
                (option-flag "-fno-exceptions" flycheck-clang-no-exceptions)
                (option-flag "-fno-rtti" flycheck-clang-no-rtti)
                (option-flag "-fblocks" flycheck-clang-blocks)
                (option-list "-include" flycheck-clang-includes)
                (option-list "-W" flycheck-clang-warnings concat)
                (option-list "-D" flycheck-clang-definitions concat)
                (option-list "-I" flycheck-clang-include-path)
                (eval flycheck-clang-args)
                "-x" (eval
                      (pcase major-mode
                        (`c++-mode "c++")
                        (`c-mode "c")))
                ;; Read from standard input
                "-")
      :standard-input t
      :error-patterns
      ((error line-start
              (message "In file included from") " " (or "<stdin>" (file-name))
              ":" line ":" line-end)
       (info line-start (or "<stdin>" (file-name)) ":" line ":" column
             ": note: " (optional (message)) line-end)
       (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
                ": warning: " (optional (message)) line-end)
       (error line-start (or "<stdin>" (file-name)) ":" line ":" column
              ": " (or "fatal error" "error") ": " (optional (message)) line-end))
      :error-filter
      (lambda (errors)
        (let ((errors (flycheck-sanitize-errors errors)))
          (dolist (err errors)
            ;; Clang will output empty messages for #error/#warning pragmas without
            ;; messages.  We fill these empty errors with a dummy message to get
            ;; them past our error filtering
            (setf (flycheck-error-message err)
                  (or (flycheck-error-message err) "no message")))
          (flycheck-fold-include-levels errors "In file included from")))
      :modes (c-mode c++-mode)
      :next-checkers ((warning . c/c++-cppcheck)))

    (flycheck-def-args-var flycheck-gcc-args c/c++-gcc
      :package-version '(flycheck . "0.22"))

    (flycheck-def-option-var flycheck-gcc-definitions nil c/c++-gcc
      "Additional preprocessor definitions for GCC.

    The value of this variable is a list of strings, where each
    string is an additional definition to pass to GCC, via the `-D'
    option."
      :type '(repeat (string :tag "Definition"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.20"))

    (flycheck-def-option-var flycheck-gcc-include-path nil c/c++-gcc
      "A list of include directories for GCC.

    The value of this variable is a list of strings, where each
    string is a directory to add to the include path of gcc.
    Relative paths are relative to the file being checked."
      :type '(repeat (directory :tag "Include directory"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.20"))

    (flycheck-def-option-var flycheck-gcc-includes nil c/c++-gcc
      "A list of additional include files for GCC.

    The value of this variable is a list of strings, where each
    string is a file to include before syntax checking.  Relative
    paths are relative to the file being checked."
      :type '(repeat (file :tag "Include file"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.20"))

    (flycheck-def-option-var flycheck-gcc-language-standard nil c/c++-gcc
      "The language standard to use in GCC.

    The value of this variable is either a string denoting a language
    standard, or nil, to use the default standard.  When non-nil,
    pass the language standard via the `-std' option."
      :type '(choice (const :tag "Default standard" nil)
                     (string :tag "Language standard"))
      :safe #'stringp
      :package-version '(flycheck . "0.20"))
    (make-variable-buffer-local 'flycheck-gcc-language-standard)

    (flycheck-def-option-var flycheck-gcc-no-exceptions nil c/c++-gcc
      "Whether to disable exceptions in GCC.

    When non-nil, disable exceptions for syntax checks, via
    `-fno-exceptions'."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.20"))

    (flycheck-def-option-var flycheck-gcc-no-rtti nil c/c++-gcc
      "Whether to disable RTTI in GCC.

    When non-nil, disable RTTI for syntax checks, via `-fno-rtti'."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.20"))

    (flycheck-def-option-var flycheck-gcc-openmp nil c/c++-gcc
      "Whether to enable OpenMP in GCC.

    When non-nil, enable OpenMP for syntax checkers, via
    `-fopenmp'."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.21"))

    (flycheck-def-option-var flycheck-gcc-pedantic nil c/c++-gcc
      "Whether to warn about language extensions in GCC.

    For ISO C, follows the version specified by any -std option used.
    When non-nil, disable non-ISO extensions to C/C++ via
    `-pedantic'."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.23"))

    (flycheck-def-option-var flycheck-gcc-pedantic-errors nil c/c++-gcc
      "Whether to error on language extensions in GCC.

    For ISO C, follows the version specified by any -std option used.
    When non-nil, disable non-ISO extensions to C/C++ via
    `-pedantic-errors'."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.23"))

    (flycheck-def-option-var flycheck-gcc-warnings '("all" "extra") c/c++-gcc
      "A list of additional warnings to enable in GCC.

    The value of this variable is a list of strings, where each string
    is the name of a warning category to enable.  By default, all
    recommended warnings and some extra warnings are enabled (as by
    `-Wall' and `-Wextra' respectively).

    Refer to the gcc manual at URL
    `https://gcc.gnu.org/onlinedocs/gcc/' for more information about
    warnings."
      :type '(choice (const :tag "No additional warnings" nil)
                     (repeat :tag "Additional warnings"
                             (string :tag "Warning name")))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.20"))

    (flycheck-define-checker c/c++-gcc
      "A C/C++ syntax checker using GCC.

    Requires GCC 4.4 or newer.  See URL `https://gcc.gnu.org/'."
      :command ("gcc"
                "-fshow-column"
                "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
                (option "-std=" flycheck-gcc-language-standard concat)
                (option-flag "-pedantic" flycheck-gcc-pedantic)
                (option-flag "-pedantic-errors" flycheck-gcc-pedantic-errors)
                (option-flag "-fno-exceptions" flycheck-gcc-no-exceptions)
                (option-flag "-fno-rtti" flycheck-gcc-no-rtti)
                (option-flag "-fopenmp" flycheck-gcc-openmp)
                (option-list "-include" flycheck-gcc-includes)
                (option-list "-W" flycheck-gcc-warnings concat)
                (option-list "-D" flycheck-gcc-definitions concat)
                (option-list "-I" flycheck-gcc-include-path)
                (eval flycheck-gcc-args)
                "-x" (eval
                      (pcase major-mode
                        (`c++-mode "c++")
                        (`c-mode "c")))
                ;; GCC performs full checking only when actually compiling, so
                ;; `-fsyntax-only' is not enough. Just let it generate assembly
                ;; code.
                "-S" "-o" null-device
                ;; Read from standard input
                "-")
      :standard-input t
      :error-patterns
      ((error line-start
              (message "In file included from") " " (or "<stdin>" (file-name))
              ":" line ":" column ":" line-end)
       (info line-start (or "<stdin>" (file-name)) ":" line ":" column
             ": note: " (message) line-end)
       (warning line-start (or "<stdin>" (file-name)) ":" line ":" column
                ": warning: " (message (one-or-more (not (any "\n["))))
                (optional "[" (id (one-or-more not-newline)) "]") line-end)
       (error line-start (or "<stdin>" (file-name)) ":" line ":" column
              ": " (or "fatal error" "error") ": " (message) line-end))
      :error-filter
      (lambda (errors)
        (flycheck-fold-include-levels (flycheck-sanitize-errors errors)
                                      "In file included from"))
      :modes (c-mode c++-mode)
      :next-checkers ((warning . c/c++-cppcheck)))

    (flycheck-def-option-var flycheck-cppcheck-checks '("style") c/c++-cppcheck
      "Enabled checks for Cppcheck.

    The value of this variable is a list of strings, where each
    string is the name of an additional check to enable.  By default,
    all coding style checks are enabled.

    See section \"Enable message\" in the Cppcheck manual at URL
    `http://cppcheck.sourceforge.net/manual.pdf', and the
    documentation of the `--enable' option for more information,
    including a list of supported checks."
      :type '(repeat :tag "Additional checks"
                     (string :tag "Check name"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.14"))

    (flycheck-def-option-var flycheck-cppcheck-standards nil c/c++-cppcheck
      "The standards to use in cppcheck.

    The value of this variable is either a list of strings denoting
    the standards to use, or nil to pass nothing to cppcheck.  When
    non-nil, pass the standards via one or more `--std=' options."
      :type '(choice (const :tag "Default" nil)
                     (repeat :tag "Custom standards"
                             (string :tag "Standard name")))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "28"))
    (make-variable-buffer-local 'flycheck-cppcheck-standards)

    (flycheck-def-option-var flycheck-cppcheck-suppressions nil c/c++-cppcheck
      "The suppressions to use in cppcheck.

    The value of this variable is either a list of strings denoting
    the suppressions to use, or nil to pass nothing to cppcheck.
    When non-nil, pass the suppressions via one or more `--suppress='
    options."
      :type '(choice (const :tag "Default" nil)
                     (repeat :tag "Additional suppressions"
                             (string :tag "Suppression")))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "28"))

    (flycheck-def-option-var flycheck-cppcheck-inconclusive nil c/c++-cppcheck
      "Whether to enable Cppcheck inconclusive checks.

    When non-nil, enable Cppcheck inconclusive checks.  This allows Cppcheck to
    report warnings it's not certain of, but it may result in false positives.

    This will have no effect when using Cppcheck 1.53 and older."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.19"))

    (flycheck-def-option-var flycheck-cppcheck-include-path nil c/c++-cppcheck
      "A list of include directories for cppcheck.

    The value of this variable is a list of strings, where each
    string is a directory to add to the include path of cppcheck.
    Relative paths are relative to the file being checked."
      :type '(repeat (directory :tag "Include directory"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.24"))

    (flycheck-define-checker c/c++-cppcheck
      "A C/C++ checker using cppcheck.

    See URL `http://cppcheck.sourceforge.net/'."
      :command ("cppcheck" "--quiet" "--xml-version=2" "--inline-suppr"
                (option "--enable=" flycheck-cppcheck-checks concat
                        flycheck-option-comma-separated-list)
                (option-flag "--inconclusive" flycheck-cppcheck-inconclusive)
                (option-list "-I" flycheck-cppcheck-include-path)
                (option-list "--std=" flycheck-cppcheck-standards concat)
                (option-list "--suppress=" flycheck-cppcheck-suppressions concat)
                "-x" (eval
                      (pcase major-mode
                        (`c++-mode "c++")
                        (`c-mode "c")))
                source)
      :error-parser flycheck-parse-cppcheck
      :modes (c-mode c++-mode))

    (flycheck-define-checker cfengine
      "A CFEngine syntax checker using cf-promises.

    See URL `https://cfengine.com/'."
      :command ("cf-promises" "-Wall" "-f"
                ;; We must stay in the same directory to resolve @include
                source-inplace)
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column
                ": warning: " (message) line-end)
       (error line-start (file-name) ":" line ":" column
              ": error: " (message) line-end))
      :modes (cfengine-mode cfengine3-mode))

    (flycheck-def-option-var flycheck-foodcritic-tags nil chef-foodcritic
      "A list of tags to select for Foodcritic.

    The value of this variable is a list of strings where each string
    is a tag expression describing Foodcritic rules to enable or
    disable, via the `--tags' option.  To disable a tag, prefix it
    with `~'."
      :type '(repeat :tag "Tags" (string :tag "Tag expression"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.23"))

    (flycheck-define-checker chef-foodcritic
      "A Chef cookbooks syntax checker using Foodcritic.

    See URL `http://www.foodcritic.io'."
      ;; Use `source-inplace' to allow resource discovery with relative paths.
      ;; foodcritic interprets these as relative to the source file, so we need to
      ;; stay within the source tree.  See
      ;; https://github.com/flycheck/flycheck/pull/556
      :command ("foodcritic"
                (option-list "--tags" flycheck-foodcritic-tags)
                source-inplace)
      :error-patterns
      ((error line-start (message) ": " (file-name) ":" line line-end))
      :modes (enh-ruby-mode ruby-mode)
      :predicate
      (lambda ()
        (let ((parent-dir (file-name-directory
                           (directory-file-name
                            (expand-file-name default-directory)))))
          (or
           ;; Chef CookBook
           ;; http://docs.opscode.com/chef/knife.html#id38
           (locate-dominating-file parent-dir "recipes")
           ;; Knife Solo
           ;; http://matschaffer.github.io/knife-solo/#label-Init+command
           (locate-dominating-file parent-dir "cookbooks")))))

    (flycheck-define-checker coffee
      "A CoffeeScript syntax checker using coffee.

    See URL `http://coffeescript.org/'."
      ;; --print suppresses generation of compiled .js files
      :command ("coffee" "--compile" "--print" "--stdio")
      :standard-input t
      :error-patterns
      ((error line-start "[stdin]:" line ":" column
              ": error: " (message) line-end))
      :modes coffee-mode
      :next-checkers ((warning . coffee-coffeelint)))

    (flycheck-def-config-file-var flycheck-coffeelintrc coffee-coffeelint
                                  ".coffeelint.json"
      :safe #'stringp)

    (flycheck-define-checker coffee-coffeelint
      "A CoffeeScript style checker using coffeelint.

    See URL `http://www.coffeelint.org/'."
      :command
      ("coffeelint"
       (config-file "--file" flycheck-coffeelintrc)
       "--stdin" "--reporter" "checkstyle")
      :standard-input t
      :error-parser flycheck-parse-checkstyle
      :error-filter (lambda (errors)
                      (flycheck-remove-error-file-names
                       "stdin" (flycheck-remove-error-ids
                                (flycheck-sanitize-errors errors))))
      :modes coffee-mode)

    (flycheck-define-checker coq
      "A Coq syntax checker using the Coq compiler.

    See URL `https://coq.inria.fr/'."
      ;; We use coqtop in batch mode, because coqc is picky about file names.
      :command ("coqtop" "-batch" "-load-vernac-source" source)
      :error-patterns
      ((error line-start "File \"" (file-name) "\", line " line
              ;; TODO: Parse the end column, once Flycheck supports that
              ", characters " column "-" (one-or-more digit) ":\n"
              (or "Syntax error:" "Error:")
              ;; Most Coq error messages span multiple lines, and end with a dot.
              ;; There are simple one-line messages, too, though.
              (message (or (and (one-or-more (or not-newline "\n")) ".")
                           (one-or-more not-newline)))
              line-end))
      :error-filter
      (lambda (errors)
        (dolist (err (flycheck-sanitize-errors errors))
          (setf (flycheck-error-message err)
                (replace-regexp-in-string (rx (1+ (syntax whitespace)) line-end)
                                          "" (flycheck-error-message err)
                                          'fixedcase 'literal)))
        (flycheck-increment-error-columns errors))
      :modes coq-mode)

    (flycheck-define-checker css-csslint
      "A CSS syntax and style checker using csslint.

    See URL `https://github.com/CSSLint/csslint'."
      :command ("csslint" "--format=checkstyle-xml" source)
      :error-parser flycheck-parse-checkstyle
      :error-filter flycheck-dequalify-error-ids
      :modes css-mode)

    (defconst flycheck-stylelint-args '("--formatter" "json")
      "Common arguments to stylelint invocations.")

    (flycheck-def-config-file-var flycheck-stylelintrc
        (css-stylelint scss-stylelint less-stylelint) nil
      :safe #'stringp)

    (flycheck-def-option-var flycheck-stylelint-quiet
        nil (css-stylelint scss-stylelint less-stylelint)
      "Whether to run stylelint in quiet mode.

    When non-nil, enable quiet mode, via `--quiet'."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . 26))

    (defconst flycheck-stylelint-error-re
      (flycheck-rx-to-string
       '(: line-start (id (one-or-more word)) ": " (message) line-end)))

    (defun flycheck-parse-stylelint (output checker buffer)
      "Parse stylelint errors from OUTPUT.

    CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
    the BUFFER that was checked respectively.

    The CHECKER usually returns the errors as JSON.

    If the CHECKER throws an Error it returns an Error message with a stacktrace."
      (condition-case nil
          (flycheck-parse-stylelint-json output checker buffer)

        ;; The output could not be parsed as JSON
        (json-error

         ;; Extract a flycheck error from the output (with a regular expression)
         ;; For match-string 4/5 see flycheck-rx-message/flycheck-rx-id
         (when (string-match flycheck-stylelint-error-re output)
           (list (flycheck-error-new-at
                  1 nil 'error
                  (match-string 4 output)
                  :id (match-string 5 output)
                  :checker checker
                  :buffer buffer
                  :filename (buffer-file-name buffer)))))))

    (defun flycheck-parse-stylelint-json (output checker buffer)
      "Parse stylelint JSON errors from OUTPUT.

    CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
    the BUFFER that was checked respectively.

    See URL `http://stylelint.io/developer-guide/formatters/' for information
    about the JSON format of stylelint."
      (let ((json-object-type 'plist))

        ;; stylelint returns a vector of result objects
        ;; Since we only passed one file, the first element is enough
        (let* ((stylelint-output (elt (json-read-from-string output) 0))
               (filename (buffer-file-name buffer))

               ;; Turn all deprecations into warnings
               (deprecations
                (mapcar (lambda (d)
                          (flycheck-error-new-at
                           1 nil 'warning
                           (plist-get d :text)
                           :id "Deprecation Warning"
                           :checker checker
                           :buffer buffer
                           :filename filename))
                        (plist-get stylelint-output :deprecations)))

               ;; Turn all invalid options into errors
               (invalid-options
                (mapcar (lambda (io)
                          (flycheck-error-new-at
                           1 nil 'error
                           (plist-get io :text)
                           :id "Invalid Option"
                           :checker checker
                           :buffer buffer
                           :filename filename))
                        (plist-get stylelint-output :invalidOptionWarnings)))

               ;; Read all linting warnings
               (warnings
                (mapcar (lambda (w)
                          (flycheck-error-new-at
                           (plist-get w :line) (plist-get w :column)
                           (pcase (plist-get w :severity)
                             (`"error"   'error)
                             (`"warning" 'warning)
                             ;; Default to info for unknown .severity
                             (_          'info))
                           (plist-get w :text)
                           :id (plist-get w :rule)
                           :checker checker
                           :buffer buffer
                           :filename filename))
                        (plist-get stylelint-output :warnings))))

          ;; Return the combined errors (deprecations, invalid options, warnings)
          (append deprecations invalid-options warnings))))

    (flycheck-define-checker css-stylelint
      "A CSS syntax and style checker using stylelint.

    See URL `http://stylelint.io/'."
      :command ("stylelint"
                (eval flycheck-stylelint-args)
                (option-flag "--quiet" flycheck-stylelint-quiet)
                (config-file "--config" flycheck-stylelintrc))
      :standard-input t
      :error-parser flycheck-parse-stylelint
      :modes (css-mode))

    (defconst flycheck-d-module-re
      (rx "module" (one-or-more (syntax whitespace))
          (group (one-or-more (not (syntax whitespace))))
          (zero-or-more (syntax whitespace))
          ";")
      "Regular expression to match a D module declaration.")

    (defun flycheck-d-base-directory ()
      "Get the relative base directory path for this module."
      (let* ((file-name (buffer-file-name))
             (module-file (if (string= (file-name-nondirectory file-name)
                                       "package.d")
                              (directory-file-name (file-name-directory file-name))
                            file-name)))
        (flycheck-module-root-directory
         (flycheck-find-in-buffer flycheck-d-module-re)
         module-file)))

    (flycheck-def-option-var flycheck-dmd-include-path nil d-dmd
      "A list of include directories for dmd.

    The value of this variable is a list of strings, where each
    string is a directory to add to the include path of dmd.
    Relative paths are relative to the file being checked."
      :type '(repeat (directory :tag "Include directory"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.18"))

    (flycheck-def-args-var flycheck-dmd-args d-dmd
      :package-version '(flycheck . "0.24"))

    (flycheck-define-checker d-dmd
      "A D syntax checker using the DMD compiler.

    Requires DMD 2.066 or newer.  See URL `http://dlang.org/'."
      :command ("dmd"
                "-debug"                    ; Compile in debug mode
                "-o-"                       ; Don't generate an object file
                "-vcolumns"                 ; Add columns in output
                "-wi" ; Compilation will continue even if there are warnings
                (eval (concat "-I" (flycheck-d-base-directory)))
                (option-list "-I" flycheck-dmd-include-path concat)
                (eval flycheck-dmd-args)
                source)
      :error-patterns
      ((error line-start
              (file-name) "(" line "," column "): Error: " (message)
              line-end)
       (warning line-start (file-name) "(" line "," column "): "
                (or "Warning" "Deprecation") ": " (message) line-end)
       (info line-start (file-name) "(" line "," column "): "
             (one-or-more " ") (message) line-end))
      :modes d-mode)

    (flycheck-define-checker dockerfile-hadolint
      "A Dockerfile syntax checker using the hadolint.

    See URL `http://hadolint.lukasmartinelli.ch/'."
      :command ("hadolint" "-")
      :standard-input t
      :error-patterns
      ((error line-start
              (file-name) ":" line ":" column " " (message)
              line-end)
       (warning line-start
                (file-name) ":" line " " (id (one-or-more alnum)) " " (message)
                line-end))
      :error-filter
      (lambda (errors)
        (flycheck-sanitize-errors
         (flycheck-remove-error-file-names "/dev/stdin" errors)))
      :modes dockerfile-mode)

    (defun flycheck-elixir--find-default-directory (_checker)
      "Come up with a suitable default directory to run CHECKER in.

    This will either be the directory that contains `mix.exs' or,
    if no such file is found in the directory hierarchy, the directory
    of the current file."
      (or
       (and
        buffer-file-name
        (locate-dominating-file buffer-file-name "mix.exs"))
       default-directory))

    (defun flycheck-elixir--parse-dogma-json (output checker buffer)
      "Parse Dogma errors from JSON OUTPUT.

    CHECKER and BUFFER denote the CHECKER that returned OUTPUT and
    the BUFFER that was checked respectively.

    See URL `https://github.com/lpil/dogma' for more information
    about dogma."
      (let* ((json-object-type 'alist)
             (json-array-type  'list)
             (dogma-json-output
              (car (cdr (assq 'files (json-read-from-string output)))))
             (dogma-errors-list (cdr (assq 'errors dogma-json-output)))
             (dogma-filename (cdr (assq 'path dogma-json-output)))
             errors)
        (dolist (emessage dogma-errors-list)
          (let-alist emessage
            (push (flycheck-error-new-at
                   .line
                   1
                   'error .message
                   :id .rule
                   :checker checker
                   :buffer buffer
                   :filename dogma-filename)
                  errors)))
        (nreverse errors)))

    (defun flycheck-elixir--check-for-dogma ()
      "Check if `dogma' is installed.

    Check by looking for deps/dogma in this directory or a parent to
    handle umbrella apps.
    Used as a predicate for enabling the checker."
      (and buffer-file-name
           (locate-dominating-file buffer-file-name "deps/dogma")))

    (flycheck-define-checker elixir-dogma
      "An Elixir syntax checker using the Dogma analysis tool.

    See URL `https://github.com/lpil/dogma/'."
      :command ("mix" "dogma" "--format=json" source)
      :error-parser flycheck-elixir--parse-dogma-json
      :working-directory flycheck-elixir--find-default-directory
      :predicate flycheck-elixir--check-for-dogma
      :modes elixir-mode)

    (defconst flycheck-this-emacs-executable
      (concat invocation-directory invocation-name)
      "The path to the currently running Emacs executable.")

    (defconst flycheck-emacs-args '("-Q" "--batch")
      "Common arguments to Emacs invocations.")

    (defmacro flycheck-prepare-emacs-lisp-form (&rest body)
      "Prepare BODY for use as check form in a subprocess."
      (declare (indent 0))
      `(flycheck-sexp-to-string
        '(progn
           (defvar jka-compr-inhibit)
           (unwind-protect
               ;; Flycheck inhibits compression of temporary files, thus we
               ;; must not attempt to decompress.
               (let ((jka-compr-inhibit t))
                 ;; Strip option-argument separator from arguments, if present
                 (when (equal (car command-line-args-left) "--")
                   (setq command-line-args-left (cdr command-line-args-left)))
                 ,@body)
             ;; Prevent Emacs from processing the arguments on its own, see
             ;; https://github.com/flycheck/flycheck/issues/319
             (setq command-line-args-left nil)))))

    (defconst flycheck-emacs-lisp-check-form
      (flycheck-prepare-emacs-lisp-form
        ;; Keep track of the generated bytecode files, to delete them after byte
        ;; compilation.
        (defvar flycheck-byte-compiled-files nil)
        (let ((byte-compile-dest-file-function
               (lambda (source)
                 (let ((temp-file (make-temp-file (file-name-nondirectory source))))
                   (push temp-file flycheck-byte-compiled-files)
                   temp-file))))
          (unwind-protect
              (byte-compile-file (car command-line-args-left))
            (mapc (lambda (f) (ignore-errors (delete-file f)))
                  flycheck-byte-compiled-files))
          (when (bound-and-true-p flycheck-emacs-lisp-check-declare)
            (check-declare-file (car command-line-args-left))))))

    (flycheck-def-option-var flycheck-emacs-lisp-load-path nil emacs-lisp
      "Load path to use in the Emacs Lisp syntax checker.

    When set to `inherit', use the `load-path' of the current Emacs
    session during syntax checking.

    When set to a list of strings, add each directory in this list to
    the `load-path' before invoking the byte compiler.  Relative
    paths in this list are expanded against the `default-directory'
    of the buffer to check.

    When nil, do not explicitly set the `load-path' during syntax
    checking.  The syntax check only uses the built-in `load-path' of
    Emacs in this case.

    Note that changing this variable can lead to wrong results of the
    syntax check, e.g. if an unexpected version of a required library
    is used."
      :type '(choice (const :tag "Inherit current `load-path'" inherit)
                     (repeat :tag "Load path" directory))
      :risky t
      :package-version '(flycheck . "0.14"))

    (flycheck-def-option-var flycheck-emacs-lisp-initialize-packages
        'auto emacs-lisp
      "Whether to initialize packages in the Emacs Lisp syntax checker.

    When nil, never initialize packages.  When `auto', initialize
    packages only when checking `user-init-file' or files from
    `user-emacs-directory'.  For any other non-nil value, always
    initialize packages.

    When initializing packages is enabled the `emacs-lisp' syntax
    checker calls `package-initialize' before byte-compiling the file
    to be checked.  It also sets `package-user-dir' according to
    `flycheck-emacs-lisp-package-user-dir'."
      :type '(choice (const :tag "Do not initialize packages" nil)
                     (const :tag "Initialize packages for configuration only" auto)
                     (const :tag "Always initialize packages" t))
      :risky t
      :package-version '(flycheck . "0.14"))

    (defconst flycheck-emacs-lisp-package-initialize-form
      (flycheck-sexp-to-string
       '(with-demoted-errors "Error during package initialization: %S"
          (package-initialize)))
      "Form used to initialize packages.")

    (defun flycheck-option-emacs-lisp-package-initialize (value)
      "Option VALUE filter for `flycheck-emacs-lisp-initialize-packages'."
      (let ((shall-initialize
             (if (eq value 'auto)
                 (or (flycheck-in-user-emacs-directory-p (buffer-file-name))
                     ;; `user-init-file' is nil in non-interactive sessions.  Now,
                     ;; no user would possibly use Flycheck in a non-interactive
                     ;; session, but our unit tests run non-interactively, so we
                     ;; have to handle this case anyway
                     (and user-init-file
                          (flycheck-same-files-p (buffer-file-name)
                                                 user-init-file)))
               value)))
        (when shall-initialize
          ;; If packages shall be initialized, return the corresponding form,
          ;; otherwise make Flycheck ignore the option by returning nil.
          flycheck-emacs-lisp-package-initialize-form)))

    (flycheck-def-option-var flycheck-emacs-lisp-package-user-dir nil emacs-lisp
      "Package directory for the Emacs Lisp syntax checker.

    If set to a string set `package-user-dir' to the value of this
    variable before initializing packages. If set to nil just inherit
    the value of `package-user-dir' from the running Emacs session.

    This variable has no effect, if
    `flycheck-emacs-lisp-initialize-packages' is nil."
      :type '(choice (const :tag "Default package directory" nil)
                     (directory :tag "Custom package directory"))
      :risky t
      :package-version '(flycheck . "0.14"))

    (defun flycheck-option-emacs-lisp-package-user-dir (value)
      "Option VALUE filter for `flycheck-emacs-lisp-package-user-dir'."
      ;; Inherit the package directory from our Emacs session
      (let ((value (or value (bound-and-true-p package-user-dir))))
        (when value
          (flycheck-sexp-to-string `(setq package-user-dir ,value)))))

    (flycheck-def-option-var flycheck-emacs-lisp-check-declare nil emacs-lisp
      "If non-nil, check ‘declare-function’ forms using ‘check-declare-file’."
      :type '(choice (const :tag "Do not check declare forms" nil)
                     (const :tag "Check declare forms" t))
      :risky t
      :package-version '(flycheck . "31"))

    (defun flycheck-option-emacs-lisp-check-declare (value)
      "Option VALUE filter for `flycheck-emacs-lisp-check-declare'."
      (when value
        (flycheck-sexp-to-string
         `(progn
            (defvar flycheck-emacs-lisp-check-declare)
            (setq flycheck-emacs-lisp-check-declare ,value)))))

    (flycheck-define-checker emacs-lisp
      "An Emacs Lisp syntax checker using the Emacs Lisp Byte compiler.

    See Info Node `(elisp)Byte Compilation'."
      :command ("emacs" (eval flycheck-emacs-args)
                (eval
                 (let ((path (pcase flycheck-emacs-lisp-load-path
                               (`inherit load-path)
                               (p (seq-map #'expand-file-name p)))))
                   (flycheck-prepend-with-option "--directory" path)))
                (option "--eval" flycheck-emacs-lisp-package-user-dir nil
                        flycheck-option-emacs-lisp-package-user-dir)
                (option "--eval" flycheck-emacs-lisp-initialize-packages nil
                        flycheck-option-emacs-lisp-package-initialize)
                (option "--eval" flycheck-emacs-lisp-check-declare nil
                        flycheck-option-emacs-lisp-check-declare)
                "--eval" (eval flycheck-emacs-lisp-check-form)
                "--"
                source-inplace)
      :error-patterns
      ((error line-start (file-name) ":" line ":" column ":Error:"
              (message (zero-or-more not-newline)
                       (zero-or-more "\n    " (zero-or-more not-newline)))
              line-end)
       (warning line-start (file-name) ":" line ":" column ":Warning:"
                (message (zero-or-more not-newline)
                         (zero-or-more "\n    " (zero-or-more not-newline)))
                line-end)
       (warning line-start (file-name) ":" line ":" column
                ":Warning (check-declare): said\n"
                (message (zero-or-more "    " (zero-or-more not-newline))
                         (zero-or-more "\n    " (zero-or-more not-newline)))
                line-end)
       ;; The following is for Emacs 24 ‘check-declare-file’, which uses a
       ;; less informative format.
       (warning line-start "Warning (check-declare): " (file-name) " said "
                (message (zero-or-more not-newline))
                line-end))
      :error-filter
      (lambda (errors)
        (flycheck-fill-empty-line-numbers
         (flycheck-collapse-error-message-whitespace
          (flycheck-sanitize-errors errors))))
      :modes (emacs-lisp-mode lisp-interaction-mode)
      :predicate
      (lambda ()
        (and
         ;; Ensure that we only check buffers with a backing file.  For buffers
         ;; without a backing file we cannot guarantee that file names in error
         ;; messages are properly resolved, because `byte-compile-file' emits file
         ;; names *relative to the directory of the checked file* instead of the
         ;; working directory.  Hence our backwards-substitution will fail, because
         ;; the checker process has a different base directory to resolve relative
         ;; file names than the Flycheck code working on the buffer to check.
         (buffer-file-name)
         ;; Do not check buffers which should not be byte-compiled.  The checker
         ;; process will refuse to compile these, which would confuse Flycheck
         (not (bound-and-true-p no-byte-compile))
         ;; Do not check buffers used for autoloads generation during package
         ;; installation.  These buffers are too short-lived for being checked, and
         ;; doing so causes spurious errors.  See
         ;; https://github.com/flycheck/flycheck/issues/45 and
         ;; https://github.com/bbatsov/prelude/issues/248.  We must also not check
         ;; compilation buffers, but as these are ephemeral, Flycheck won't check
         ;; them anyway.
         (not (flycheck-autoloads-file-p))))
      :next-checkers (emacs-lisp-checkdoc))

    (defconst flycheck-emacs-lisp-checkdoc-form
      (flycheck-prepare-emacs-lisp-form
        (unless (require 'elisp-mode nil 'no-error)
          ;; TODO: Fallback for Emacs 24, remove when dropping support for 24
          (require 'lisp-mode))
        (require 'checkdoc)

        (let ((source (car command-line-args-left))
              ;; Remember the default directory of the process
              (process-default-directory default-directory))
          ;; Note that we deliberately use our custom approach even despite of
          ;; `checkdoc-file' which was added to Emacs 25.1.  While it's conceptually
          ;; the better thing, its implementation has too many flaws to be of use
          ;; for us.
          (with-temp-buffer
            (insert-file-contents source 'visit)
            (setq buffer-file-name source)
            ;; And change back to the process default directory to make file-name
            ;; back-substutition work
            (setq default-directory process-default-directory)
            (with-demoted-errors "Error in checkdoc: %S"
              ;; Checkdoc needs the Emacs Lisp syntax table and comment syntax to
              ;; parse sexps and identify docstrings correctly; see
              ;; https://github.com/flycheck/flycheck/issues/833
              (delay-mode-hooks (emacs-lisp-mode))
              (setq delayed-mode-hooks nil)
              (checkdoc-current-buffer t)
              (with-current-buffer checkdoc-diagnostic-buffer
                (princ (buffer-substring-no-properties (point-min) (point-max)))
                (kill-buffer)))))))

    (defconst flycheck-emacs-lisp-checkdoc-variables
      '(checkdoc-symbol-words
        checkdoc-arguments-in-order-flag
        checkdoc-force-history-flag
        checkdoc-permit-comma-termination-flag
        checkdoc-force-docstrings-flag
        checkdoc-package-keywords-flag
        checkdoc-spellcheck-documentation-flag
        checkdoc-verb-check-experimental-flag
        checkdoc-max-keyref-before-warn
        sentence-end-double-space)
      "Variables inherited by the checkdoc subprocess.")

    (defun flycheck-emacs-lisp-checkdoc-variables-form ()
      "Make a sexp to pass relevant variables to a checkdoc subprocess.

    Variables are taken from `flycheck-emacs-lisp-checkdoc-variables'."
      `(progn
         ,@(seq-map (lambda (opt) `(setq-default ,opt ,(symbol-value opt)))
                    (seq-filter #'boundp flycheck-emacs-lisp-checkdoc-variables))))

    (flycheck-define-checker emacs-lisp-checkdoc
      "An Emacs Lisp style checker using CheckDoc.

    The checker runs `checkdoc-current-buffer'."
      :command ("emacs" (eval flycheck-emacs-args)
                "--eval" (eval (flycheck-sexp-to-string
                                (flycheck-emacs-lisp-checkdoc-variables-form)))
                "--eval" (eval flycheck-emacs-lisp-checkdoc-form)
                "--" source)
      :error-patterns
      ((warning line-start (file-name) ":" line ": " (message) line-end))
      :modes (emacs-lisp-mode)
      :predicate
      (lambda ()
        ;; Do not check Autoloads, Cask/Carton and dir-locals files.  These files
        ;; really don't need to follow Checkdoc conventions.
        (not (or (flycheck-autoloads-file-p)
                 (and (buffer-file-name)
                      (member (file-name-nondirectory (buffer-file-name))
                              '("Cask" "Carton" ".dir-locals.el")))))))

    (dolist (checker '(emacs-lisp emacs-lisp-checkdoc))
      (setf (car (flycheck-checker-get checker 'command))
            flycheck-this-emacs-executable))

    (flycheck-def-option-var flycheck-erlang-include-path nil erlang
      "A list of include directories for Erlang.

    The value of this variable is a list of strings, where each
    string is a directory to add to the include path of erlc.
    Relative paths are relative to the file being checked."
      :type '(repeat (directory :tag "Include directory"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.24"))

    (flycheck-def-option-var flycheck-erlang-library-path nil erlang
      "A list of library directories for Erlang.

    The value of this variable is a list of strings, where each
    string is a directory to add to the library path of erlc.
    Relative paths are relative to the file being checked."
      :type '(repeat (directory :tag "Library directory"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.24"))

    (flycheck-define-checker erlang
      "An Erlang syntax checker using the Erlang interpreter.

    See URL `http://www.erlang.org/'."
      :command ("erlc"
                "-o" temporary-directory
                (option-list "-I" flycheck-erlang-include-path)
                (option-list "-pa" flycheck-erlang-library-path)
                "-Wall"
                source)
      :error-patterns
      ((warning line-start (file-name) ":" line ": Warning:" (message) line-end)
       (error line-start (file-name) ":" line ": " (message) line-end))
      :modes erlang-mode
      :enabled (lambda () (string-suffix-p ".erl" (buffer-file-name))))

    (defun contains-rebar-config (dir-name)
      "Return DIR-NAME if DIR-NAME/rebar.config exists, nil otherwise."
      (when (file-exists-p (expand-file-name "rebar.config" dir-name))
        dir-name))

    (defun locate-rebar3-project-root (file-name &optional prev-file-name acc)
      "Find the top-most rebar project root for source FILE-NAME.

    A project root directory is any directory containing a
    rebar.config file.  Find the top-most directory to move out of any
    nested dependencies.

    FILE-NAME is a source file for which to find the project.

    PREV-FILE-NAME helps us prevent infinite looping

    ACC is an accumulator that keeps the list of results, the first
    non-nil of which will be our project root.

    Return the absolute path to the directory"
      (if (string= file-name prev-file-name)
          (car (remove nil acc))
        (let ((current-dir (file-name-directory file-name)))
          (locate-rebar3-project-root
           (directory-file-name current-dir)
           file-name
           (cons (contains-rebar-config current-dir) acc)))))

    (defun flycheck-rebar3-project-root (&optional _checker)
      "Return directory where rebar.config is located."
      (locate-rebar3-project-root buffer-file-name))

    (flycheck-define-checker erlang-rebar3
      "An Erlang syntax checker using the rebar3 build tool."
      :command ("rebar3" "compile")
      :error-parser
      (lambda (output checker buffer)
        ;; rebar3 outputs ANSI terminal colors, which don't match up with
        ;; :error-patterns, so we strip those color codes from the output
        ;; here before passing it along to the default behavior. The
        ;; relevant disucssion can be found at
        ;; https://github.com/flycheck/flycheck/pull/1144
        (require 'ansi-color)
        (flycheck-parse-with-patterns
         (and (fboundp 'ansi-color-filter-apply) (ansi-color-filter-apply output))
         checker buffer))
      :error-patterns
      ((warning line-start
                (file-name) ":" line ": Warning:" (message) line-end)
       (error line-start
              (file-name) ":" line ": " (message) line-end))
      :modes erlang-mode
      :enabled flycheck-rebar3-project-root
      :predicate flycheck-buffer-saved-p
      :working-directory flycheck-rebar3-project-root)

    (flycheck-define-checker eruby-erubis
      "A eRuby syntax checker using the `erubis' command.

    See URL `http://www.kuwata-lab.com/erubis/'."
      :command ("erubis" "-z" source)
      :error-patterns
      ((error line-start (file-name) ":" line ": " (message) line-end))
      :modes (html-erb-mode rhtml-mode))

    (flycheck-def-args-var flycheck-gfortran-args fortran-gfortran
      :package-version '(flycheck . "0.22"))

    (flycheck-def-option-var flycheck-gfortran-include-path nil fortran-gfortran
      "A list of include directories for GCC Fortran.

    The value of this variable is a list of strings, where each
    string is a directory to add to the include path of gcc.
    Relative paths are relative to the file being checked."
      :type '(repeat (directory :tag "Include directory"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.20"))

    (flycheck-def-option-var flycheck-gfortran-language-standard "f95" fortran-gfortran
      "The language standard to use in GFortran.

    The value of this variable is either a string denoting a language
    standard, or nil, to use the default standard.  When non-nil,
    pass the language standard via the `-std' option."
      :type '(choice (const :tag "Default standard" nil)
                     (string :tag "Language standard"))
      :safe #'stringp
      :package-version '(flycheck . "0.20"))

    (flycheck-def-option-var flycheck-gfortran-layout nil fortran-gfortran
      "The source code layout to use in GFortran.

    The value of this variable is one of the following symbols:

    nil
         Let gfortran determine the layout from the extension

    `free'
         Use free form layout


    `fixed'
         Use fixed form layout

    In any other case, an error is signaled."
      :type '(choice (const :tag "Guess layout from extension" nil)
                     (const :tag "Free form layout" free)
                     (const :tag "Fixed form layout" fixed))
      :safe (lambda (value) (or (not value) (memq value '(free fixed))))
      :package-version '(flycheck . "0.20"))

    (defun flycheck-option-gfortran-layout (value)
      "Option VALUE filter for `flycheck-gfortran-layout'."
      (pcase value
        (`nil nil)
        (`free "free-form")
        (`fixed "fixed-form")
        (_ (error "Invalid value for flycheck-gfortran-layout: %S" value))))

    (flycheck-def-option-var flycheck-gfortran-warnings '("all" "extra")
                             fortran-gfortran
      "A list of warnings for GCC Fortran.

    The value of this variable is a list of strings, where each string
    is the name of a warning category to enable.  By default, all
    recommended warnings and some extra warnings are enabled (as by
    `-Wall' and `-Wextra' respectively).

    Refer to the gfortran manual at URL
    `https://gcc.gnu.org/onlinedocs/gfortran/' for more information
    about warnings"
      :type '(choice (const :tag "No additional warnings" nil)
                     (repeat :tag "Additional warnings"
                             (string :tag "Warning name")))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.20"))

    (flycheck-define-checker fortran-gfortran
      "An Fortran syntax checker using GCC.

    Uses GCC's Fortran compiler gfortran.  See URL
    `https://gcc.gnu.org/onlinedocs/gfortran/'."
      :command ("gfortran"
                "-fsyntax-only"
                "-fshow-column"
                "-fno-diagnostics-show-caret" ; Do not visually indicate the source location
                "-fno-diagnostics-show-option" ; Do not show the corresponding
                                            ; warning group
                ;; Fortran has similar include processing as C/C++
                "-iquote" (eval (flycheck-c/c++-quoted-include-directory))
                (option "-std=" flycheck-gfortran-language-standard concat)
                (option "-f" flycheck-gfortran-layout concat
                        flycheck-option-gfortran-layout)
                (option-list "-W" flycheck-gfortran-warnings concat)
                (option-list "-I" flycheck-gfortran-include-path concat)
                (eval flycheck-gfortran-args)
                source)
      :error-patterns
      ((error line-start (file-name) ":" line (or ":" ".") column (or ": " ":\n")
              (or (= 3 (zero-or-more not-newline) "\n") "")
              (or "Error" "Fatal Error") ": "
              (message) line-end)
       (warning line-start (file-name) ":" line (or ":" ".") column (or ": " ":\n")
                (or (= 3 (zero-or-more not-newline) "\n") "")
                "Warning: " (message) line-end))
      :modes (fortran-mode f90-mode))

    (flycheck-define-checker go-gofmt
      "A Go syntax and style checker using the gofmt utility.

    See URL `https://golang.org/cmd/gofmt/'."
      :command ("gofmt")
      :standard-input t
      :error-patterns
      ((error line-start "<standard input>:" line ":" column ": " (message) line-end))
      :modes go-mode
      :next-checkers ((warning . go-golint)
                      ;; Fall back, if go-golint doesn't exist
                      (warning . go-vet)
                      ;; Fall back, if go-vet doesn't exist
                      (warning . go-build) (warning . go-test)
                      (warning . go-errcheck)
                      (warning . go-unconvert)
                      (warning . go-megacheck)))

    (flycheck-define-checker go-golint
      "A Go style checker using Golint.

    See URL `https://github.com/golang/lint'."
      :command ("golint" source)
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column ": " (message) line-end))
      :modes go-mode
      :next-checkers (go-vet
                      ;; Fall back, if go-vet doesn't exist
                      go-build go-test go-errcheck go-unconvert go-megacheck))

    (flycheck-def-option-var flycheck-go-vet-print-functions nil go-vet
      "A list of print-like functions for `go tool vet'.

    Go vet will check these functions for format string problems and
    issues, such as a mismatch between the number of formats used,
    and the number of arguments given.

    Each entry is in the form Name:N where N is the zero-based
    argument position of the first argument involved in the print:
    either the format or the first print argument for non-formatted
    prints.  For example, if you have Warn and Warnf functions that
    take an io.Writer as their first argument, like Fprintf,
    -printfuncs=Warn:1,Warnf:1 "
      :type '(repeat :tag "print-like functions"
                     (string :tag "function"))
      :safe #'flycheck-string-list-p)

    (flycheck-def-option-var flycheck-go-vet-shadow nil go-vet
      "Whether to check for shadowed variables with `go tool vet'.

    When non-nil check for shadowed variables.  When `strict' check
    more strictly, which can very noisy.  When nil do not check for
    shadowed variables.

    This option requires Go 1.6 or newer."
      :type '(choice (const :tag "Do not check for shadowed variables" nil)
                     (const :tag "Check for shadowed variables" t)
                     (const :tag "Strictly check for shadowed variables" strict)))

    (flycheck-def-option-var flycheck-go-megacheck-disabled-checkers nil go-megacheck
      "A list of checkers to disable when running `megacheck'.

    The value of this variable is a list of strings, where each
    string is a checker to be disabled. Valid checkers are `simple',
    `staticcheck' and `unused'. When nil, all checkers will be
    enabled. "
      :type '(set (const :tag "Disable simple" "simple")
                  (const :tag "Disable staticcheck" "staticcheck")
                  (const :tag "Disable unused" "unused"))
      :safe #'flycheck-string-list-p)

    (flycheck-define-checker go-vet
      "A Go syntax checker using the `go tool vet' command.

    See URL `https://golang.org/cmd/go/' and URL
    `https://golang.org/cmd/vet/'."
      :command ("go" "tool" "vet" "-all"
                (option "-printfuncs=" flycheck-go-vet-print-functions concat
                        flycheck-option-comma-separated-list)
                (option-flag "-shadow" flycheck-go-vet-shadow)
                (option-list "-tags=" flycheck-go-build-tags concat)
                (eval (when (eq flycheck-go-vet-shadow 'strict) "-shadowstrict"))
                source)
      :error-patterns
      ((warning line-start (file-name) ":" line ": " (message) line-end))
      :modes go-mode
      ;; We must explicitly check whether the "vet" tool is available
      :predicate (lambda ()
                   (let ((go (flycheck-checker-executable 'go-vet)))
                     (member "vet" (ignore-errors (process-lines go "tool")))))
      :next-checkers (go-build
                      go-test
                      ;; Fall back if `go build' or `go test' can be used
                      go-errcheck
                      go-unconvert
                      go-megacheck)
      :verify (lambda (_)
                (let* ((go (flycheck-checker-executable 'go-vet))
                       (have-vet (member "vet" (ignore-errors
                                                 (process-lines go "tool")))))
                  (list
                   (flycheck-verification-result-new
                    :label "go tool vet"
                    :message (if have-vet "present" "missing")
                    :face (if have-vet 'success '(bold error)))))))

    (flycheck-def-option-var flycheck-go-build-install-deps nil (go-build go-test)
      "Whether to install dependencies in `go build' and `go test'.

    If non-nil automatically install dependencies with `go build'
    while syntax checking."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.25"))

    (flycheck-def-option-var flycheck-go-build-tags nil go-build
      "A list of tags for `go build'.

    Each item is a string with a tag to be given to `go build'."
      :type '(repeat (string :tag "Tag"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.25"))

    (flycheck-define-checker go-build
      "A Go syntax and type checker using the `go build' command.

    Requires Go 1.6 or newer.  See URL `https://golang.org/cmd/go'."
      :command ("go" "build"
                (option-flag "-i" flycheck-go-build-install-deps)
                ;; multiple tags are listed as "dev debug ..."
                (option-list "-tags=" flycheck-go-build-tags concat)
                "-o" null-device)
      :error-patterns
      ((error line-start (file-name) ":" line ":"
              (optional column ":") " "
              (message (one-or-more not-newline)
                       (zero-or-more "\n\t" (one-or-more not-newline)))
              line-end)
       ;; Catch error message about multiple packages in a directory, which doesn't
       ;; follow the standard error message format.
       (info line-start
             (message "can't load package: package "
                      (one-or-more (not (any ?: ?\n)))
                      ": found packages "
                      (one-or-more not-newline))
             line-end))
      :error-filter
      (lambda (errors)
        (dolist (error errors)
          (unless (flycheck-error-line error)
            ;; Flycheck ignores errors without line numbers, but the error
            ;; message about multiple packages in a directory doesn't come with a
            ;; line number, so inject a fake one.
            (setf (flycheck-error-line error) 1)))
        errors)
      :modes go-mode
      :predicate (lambda ()
                   (and (flycheck-buffer-saved-p)
                        (not (string-suffix-p "_test.go" (buffer-file-name)))))
      :next-checkers ((warning . go-errcheck)
                      (warning . go-unconvert)
                      (warning . go-megacheck)))

    (flycheck-define-checker go-test
      "A Go syntax and type checker using the `go test' command.

    Requires Go 1.6 or newer.  See URL `https://golang.org/cmd/go'."
      :command ("go" "test"
                (option-flag "-i" flycheck-go-build-install-deps)
                (option-list "-tags=" flycheck-go-build-tags concat)
                "-c" "-o" null-device)
      :error-patterns
      ((error line-start (file-name) ":" line ":"
              (optional column ":") " "
              (message (one-or-more not-newline)
                       (zero-or-more "\n\t" (one-or-more not-newline)))
              line-end))
      :modes go-mode
      :predicate
      (lambda () (and (flycheck-buffer-saved-p)
                      (string-suffix-p "_test.go" (buffer-file-name))))
      :next-checkers ((warning . go-errcheck)
                      (warning . go-unconvert)
                      (warning . go-megacheck)))

    (flycheck-define-checker go-errcheck
      "A Go checker for unchecked errors.

    Requires errcheck newer than commit 8515d34 (Aug 28th, 2015).

    See URL `https://github.com/kisielk/errcheck'."
      :command ("errcheck"
                "-abspath"
                (option-list "-tags=" flycheck-go-build-tags concat)
                ".")
      :error-patterns
      ((warning line-start
                (file-name) ":" line ":" column (or (one-or-more "\t") ": " ":\t")
                (message)
                line-end))
      :error-filter
      (lambda (errors)
        (let ((errors (flycheck-sanitize-errors errors)))
          (dolist (err errors)
            (-when-let (message (flycheck-error-message err))
              ;; Improve the messages reported by errcheck to make them more clear.
              (setf (flycheck-error-message err)
                    (format "Ignored `error` returned from `%s`" message)))))
        errors)
      :modes go-mode
      :predicate (lambda () (flycheck-buffer-saved-p))
      :next-checkers ((warning . go-unconvert)
                      (warning . go-megacheck)))

    (flycheck-define-checker go-unconvert
      "A Go checker looking for unnecessary type conversions.

    See URL `https://github.com/mdempsky/unconvert'."
      :command ("unconvert" ".")
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column ": " (message) line-end))
      :modes go-mode
      :predicate (lambda () (flycheck-buffer-saved-p))
      :next-checkers ((warning . go-megacheck)))

    (flycheck-define-checker go-megacheck
      "A Go checker that performs static analysis and linting using the `megacheck'
    command.

    Requires Go 1.6 or newer. See URL
    `https://github.com/dominikh/go-tools'."
      :command ("megacheck"
                (option-list "-tags=" flycheck-go-build-tags concat)
                (eval (mapcar (lambda (checker) (concat "-" checker
                                                        ".enabled=false"))
                              flycheck-go-megacheck-disabled-checkers))
                ;; Run in current directory to make megacheck aware of symbols
                ;; declared in other files.
                ".")
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column ": " (message) line-end))
      :modes go-mode)

    (flycheck-define-checker groovy
      "A groovy syntax checker using groovy compiler API.

    See URL `http://www.groovy-lang.org'."
      :command ("groovy" "-e"
                "import org.codehaus.groovy.control.*

    file = new File(args[0])
    unit = new CompilationUnit()
    unit.addSource(file)

    try {
        unit.compile(Phases.CONVERSION)
    } catch (MultipleCompilationErrorsException e) {
        e.errorCollector.write(new PrintWriter(System.out, true), null)
    }
    "
                source)
      :error-patterns
      ((error line-start (file-name) ": " line ":" (message)
              " @ line " line ", column " column "." line-end))
      :modes groovy-mode)

    (flycheck-define-checker haml
      "A Haml syntax checker using the Haml compiler.

    See URL `http://haml.info'."
      :command ("haml" "-c" "--stdin")
      :standard-input t
      :error-patterns
      ((error line-start "Syntax error on line " line ": " (message) line-end))
      :modes haml-mode)

    (flycheck-define-checker handlebars
      "A Handlebars syntax checker using the Handlebars compiler.

    See URL `http://handlebarsjs.com/'."
      :command ("handlebars" "-i-")
      :standard-input t
      :error-patterns
      ((error line-start
              "Error: Parse error on line " line ":" (optional "\r") "\n"
              (zero-or-more not-newline) "\n" (zero-or-more not-newline) "\n"
              (message) line-end))
      :modes (handlebars-mode handlebars-sgml-mode web-mode)
      :predicate
      (lambda ()
        (if (eq major-mode 'web-mode)
            ;; Check if this is a handlebars file since web-mode does not store the
            ;; non-canonical engine name
            (let* ((regexp-alist (bound-and-true-p web-mode-engine-file-regexps))
                   (pattern (cdr (assoc "handlebars" regexp-alist))))
              (and pattern (buffer-file-name)
                   (string-match-p pattern (buffer-file-name))))
          t)))

    (defconst flycheck-haskell-module-re
      (rx line-start (zero-or-more (or "\n" (any space)))
          "module" (one-or-more (or "\n" (any space)))
          (group (one-or-more (not (any space "(" "\n")))))
      "Regular expression for a Haskell module name.")

    (flycheck-def-args-var flycheck-ghc-args (haskell-stack-ghc haskell-ghc)
      :package-version '(flycheck . "0.22"))

    (flycheck-def-option-var flycheck-ghc-stack-use-nix nil haskell-stack-ghc
      "Whether to enable nix support in stack.

    When non-nil, stack will append '--nix' flag to any call."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "26"))

    (flycheck-def-option-var flycheck-ghc-no-user-package-database nil haskell-ghc
      "Whether to disable the user package database in GHC.

    When non-nil, disable the user package database in GHC, via
    `-no-user-package-db'."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.16"))

    (flycheck-def-option-var flycheck-ghc-package-databases nil haskell-ghc
      "Additional module databases for GHC.

    The value of this variable is a list of strings, where each
    string is a directory of a package database.  Each package
    database is given to GHC via `-package-db'."
      :type '(repeat (directory :tag "Package database"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.16"))

    (flycheck-def-option-var flycheck-ghc-search-path nil
                             (haskell-stack-ghc haskell-ghc)
      "Module search path for (Stack) GHC.

    The value of this variable is a list of strings, where each
    string is a directory containing Haskell modules.  Each directory
    is added to the GHC search path via `-i'."
      :type '(repeat (directory :tag "Module directory"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.16"))

    (flycheck-def-option-var flycheck-ghc-language-extensions nil
                             (haskell-stack-ghc haskell-ghc)
      "Language extensions for (Stack) GHC.

    The value of this variable is a list of strings, where each
    string is a Haskell language extension, as in the LANGUAGE
    pragma.  Each extension is enabled via `-X'."
      :type '(repeat (string :tag "Language extension"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.19"))

    (defvar flycheck-haskell-ghc-cache-directory nil
      "The cache directory for `ghc' output.")

    (defun flycheck-haskell-ghc-cache-directory ()
      "Get the cache location for `ghc' output.

    If no cache directory exists yet, create one and return it.
    Otherwise return the previously used cache directory."
      (setq flycheck-haskell-ghc-cache-directory
            (or flycheck-haskell-ghc-cache-directory
                (make-temp-file "flycheck-haskell-ghc-cache" 'directory))))

    (defun flycheck--locate-dominating-file-matching (directory regexp)
      "Search for a file in directory hierarchy starting at DIRECTORY.

    Look up the directory hierarchy from DIRECTORY for a directory
    containing a file that matches REGEXP."
      (locate-dominating-file
       directory
       (lambda (dir)
         (directory-files dir t regexp t))))

    (defun flycheck-haskell--find-default-directory (checker)
      "Come up with a suitable default directory for Haskell to run CHECKER in.

    In case of `haskell-stack-ghc' checker it is directory with
    stack.yaml file.  If there's no stack.yaml file in any parent
    directory, it will be the directory that \"stack path --project-root\"
    command returns.

    For all other checkers, it is the closest parent directory that
    contains a cabal file."
      (pcase checker
        (`haskell-stack-ghc
         (or
          (when (buffer-file-name)
            (flycheck--locate-dominating-file-matching
             (file-name-directory (buffer-file-name))
             "stack.*\\.yaml\\'"))
          (-when-let* ((stack (funcall flycheck-executable-find "stack"))
                       (output (ignore-errors
                                 (process-lines stack "path" "--project-root")))
                       (stack-dir (car output)))
            (and (file-directory-p stack-dir) stack-dir))))
        (_
         (when (buffer-file-name)
           (flycheck--locate-dominating-file-matching
            (file-name-directory (buffer-file-name))
            ".+\\.cabal\\'")))))

    (flycheck-define-checker haskell-stack-ghc
      "A Haskell syntax and type checker using `stack ghc'.

    See URL `https://github.com/commercialhaskell/stack'."
      :command ("stack"
                (option-flag "--nix" flycheck-ghc-stack-use-nix)
                "ghc" "--" "-Wall" "-no-link"
                "-outputdir" (eval (flycheck-haskell-ghc-cache-directory))
                (option-list "-X" flycheck-ghc-language-extensions concat)
                (option-list "-i" flycheck-ghc-search-path concat)
                (eval (concat
                       "-i"
                       (flycheck-module-root-directory
                        (flycheck-find-in-buffer flycheck-haskell-module-re))))
                (eval flycheck-ghc-args)
                "-x" (eval
                      (pcase major-mode
                        (`haskell-mode "hs")
                        (`literate-haskell-mode "lhs")))
                source)
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column ":"
                (or " " "\n    ") (in "Ww") "arning:"
                (optional " " "[" (id (one-or-more not-newline)) "]")
                (optional "\n")
                (message
                 (one-or-more " ") (one-or-more not-newline)
                 (zero-or-more "\n"
                               (one-or-more " ")
                               (one-or-more not-newline)))
                line-end)
       (error line-start (file-name) ":" line ":" column ":" (optional " error:")
              (or (message (one-or-more not-newline))
                  (and "\n"
                       (message
                        (one-or-more " ") (one-or-more not-newline)
                        (zero-or-more "\n"
                                      (one-or-more " ")
                                      (one-or-more not-newline)))))
              line-end))
      :error-filter
      (lambda (errors)
        (flycheck-sanitize-errors (flycheck-dedent-error-messages errors)))
      :modes (haskell-mode literate-haskell-mode)
      :next-checkers ((warning . haskell-hlint))
      :working-directory flycheck-haskell--find-default-directory)

    (flycheck-define-checker haskell-ghc
      "A Haskell syntax and type checker using ghc.

    See URL `https://www.haskell.org/ghc/'."
      :command ("ghc" "-Wall" "-no-link"
                "-outputdir" (eval (flycheck-haskell-ghc-cache-directory))
                (option-flag "-no-user-package-db"
                             flycheck-ghc-no-user-package-database)
                (option-list "-package-db" flycheck-ghc-package-databases)
                (option-list "-i" flycheck-ghc-search-path concat)
                ;; Include the parent directory of the current module tree, to
                ;; properly resolve local imports
                (eval (concat
                       "-i"
                       (flycheck-module-root-directory
                        (flycheck-find-in-buffer flycheck-haskell-module-re))))
                (option-list "-X" flycheck-ghc-language-extensions concat)
                (eval flycheck-ghc-args)
                "-x" (eval
                      (pcase major-mode
                        (`haskell-mode "hs")
                        (`literate-haskell-mode "lhs")))
                source)
      :error-patterns
      ((warning line-start (file-name) ":" line ":" column ":"
                (or " " "\n    ") (in "Ww") "arning:"
                (optional " " "[" (id (one-or-more not-newline)) "]")
                (optional "\n")
                (message
                 (one-or-more " ") (one-or-more not-newline)
                 (zero-or-more "\n"
                               (one-or-more " ")
                               (one-or-more not-newline)))
                line-end)
       (error line-start (file-name) ":" line ":" column ":" (optional " error:")
              (or (message (one-or-more not-newline))
                  (and "\n"
                       (message
                        (one-or-more " ") (one-or-more not-newline)
                        (zero-or-more "\n"
                                      (one-or-more " ")
                                      (one-or-more not-newline)))))
              line-end))
      :error-filter
      (lambda (errors)
        (flycheck-sanitize-errors (flycheck-dedent-error-messages errors)))
      :modes (haskell-mode literate-haskell-mode)
      :next-checkers ((warning . haskell-hlint))
      :working-directory flycheck-haskell--find-default-directory)

    (flycheck-def-config-file-var flycheck-hlintrc haskell-hlint "HLint.hs"
      :safe #'stringp)

    (flycheck-def-args-var flycheck-hlint-args haskell-hlint
      :package-version '(flycheck . "0.25"))

    (flycheck-def-option-var flycheck-hlint-language-extensions
        nil haskell-hlint
      "Extensions list to enable for hlint.

    The value of this variable is a list of strings, where each
    string is a name of extension to enable in
    hlint (e.g. \"QuasiQuotes\")."
      :type '(repeat :tag "Extensions" (string :tag "Extension"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.24"))

    (flycheck-def-option-var flycheck-hlint-ignore-rules
        nil haskell-hlint
      "Ignore rules list for hlint checks.

    The value of this variable is a list of strings, where each
    string is an ignore rule (e.g. \"Use fmap\")."
      :type '(repeat :tag "Ignore rules" (string :tag "Ignore rule"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.24"))

    (flycheck-def-option-var flycheck-hlint-hint-packages
        nil haskell-hlint
      "Hint packages to include for hlint checks.

    The value of this variable is a list of strings, where each
    string is a default hint package (e.g. (\"Generalise\"
    \"Default\" \"Dollar\"))."
      :type '(repeat :tag "Hint packages" (string :tag "Hint package"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.24"))

    (flycheck-define-checker haskell-hlint
      "A Haskell style checker using hlint.

    See URL `https://github.com/ndmitchell/hlint'."
      :command ("hlint"
                (option-list "-X" flycheck-hlint-language-extensions concat)
                (option-list "-i=" flycheck-hlint-ignore-rules concat)
                (option-list "-h" flycheck-hlint-hint-packages concat)
                (config-file "-h" flycheck-hlintrc)
                (eval flycheck-hlint-args)
                source-inplace)
      :error-patterns
      ((info line-start
             (file-name) ":" line ":" column
             ": Suggestion: "
             (message (one-or-more (and (one-or-more (not (any ?\n))) ?\n)))
             line-end)
       (warning line-start
                (file-name) ":" line ":" column
                ": Warning: "
                (message (one-or-more (and (one-or-more (not (any ?\n))) ?\n)))
                line-end)
       (error line-start
              (file-name) ":" line ":" column
              ": Error: "
              (message (one-or-more (and (one-or-more (not (any ?\n))) ?\n)))
              line-end))
      :modes (haskell-mode literate-haskell-mode))

    (flycheck-def-config-file-var flycheck-tidyrc html-tidy ".tidyrc"
      :safe #'stringp)

    (flycheck-define-checker html-tidy
      "A HTML syntax and style checker using Tidy.

    See URL `https://github.com/htacg/tidy-html5'."
      :command ("tidy" (config-file "-config" flycheck-tidyrc) "-e" "-q")
      :standard-input t
      :error-patterns
      ((error line-start
              "line " line
              " column " column
              " - Error: " (message) line-end)
       (warning line-start
                "line " line
                " column " column
                " - Warning: " (message) line-end))
      :modes (html-mode nxhtml-mode))

    (flycheck-def-config-file-var flycheck-jshintrc javascript-jshint ".jshintrc"
      :safe #'stringp)

    (flycheck-def-option-var flycheck-jshint-extract-javascript nil
                             javascript-jshint
      "Whether jshint should extract Javascript from HTML.

    If nil no extract rule is given to jshint.  If `auto' only
    extract Javascript if a HTML file is detected.  If `always' or
    `never' extract Javascript always or never respectively.

    Refer to the jshint manual at the URL
    `http://jshint.com/docs/cli/#flags' for more information."
      :type
      '(choice (const :tag "No extraction rule" nil)
               (const :tag "Try to extract Javascript when detecting HTML files"
                      auto)
               (const :tag "Always try to extract Javascript" always)
               (const :tag "Never try to extract Javascript" never))
      :safe #'symbolp
      :package-version '(flycheck . "26"))

    (flycheck-define-checker javascript-jshint
      "A Javascript syntax and style checker using jshint.

    See URL `http://www.jshint.com'."
      :command ("jshint" "--reporter=checkstyle"
                "--filename" source-original
                (config-file "--config" flycheck-jshintrc)
                (option "--extract=" flycheck-jshint-extract-javascript
                        concat flycheck-option-symbol)
                "-")
      :standard-input t
      :error-parser flycheck-parse-checkstyle
      :error-filter
      (lambda (errors)
        (flycheck-remove-error-file-names
         "stdin" (flycheck-dequalify-error-ids errors)))
      :modes (js-mode js2-mode js3-mode rjsx-mode)
      :next-checkers ((warning . javascript-jscs)))

    (flycheck-def-option-var flycheck-eslint-rules-directories nil javascript-eslint
      "A list of directories with custom rules for ESLint.

    The value of this variable is a list of strings, where each
    string is a directory with custom rules for ESLint.

    Refer to the ESLint manual at URL
    `http://eslint.org/docs/user-guide/command-line-interface#--rulesdir'
    for more information about the custom directories."
      :type '(repeat (directory :tag "Custom rules directory"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "29"))

    (defun flycheck-eslint-config-exists-p ()
      "Whether there is a valid eslint config for the current buffer."
      (let* ((executable (flycheck-find-checker-executable 'javascript-eslint))
             (exitcode (and executable (call-process executable nil nil nil
                                                     "--print-config" "."))))
        (eq exitcode 0)))

    (flycheck-define-checker javascript-eslint
      "A Javascript syntax and style checker using eslint.

    See URL `http://eslint.org/'."
      :command ("eslint" "--format=checkstyle"
                (option-list "--rulesdir" flycheck-eslint-rules-directories)
                "--stdin" "--stdin-filename" source-original)
      :standard-input t
      :error-parser flycheck-parse-checkstyle
      :error-filter
      (lambda (errors)
        (seq-do (lambda (err)
                  ;; Parse error ID from the error message
                  (setf (flycheck-error-message err)
                        (replace-regexp-in-string
                         (rx " ("
                             (group (one-or-more (not (any ")"))))
                             ")" string-end)
                         (lambda (s)
                           (setf (flycheck-error-id err)
                                 (match-string 1 s))
                           "")
                         (flycheck-error-message err))))
                (flycheck-sanitize-errors errors))
        errors)
      :enabled (lambda () (flycheck-eslint-config-exists-p))
      :modes (js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode)
      :next-checkers ((warning . javascript-jscs))
      :verify
      (lambda (_)
        (let* ((default-directory
                 (flycheck-compute-working-directory 'javascript-eslint))
               (have-config (flycheck-eslint-config-exists-p)))
          (list
           (flycheck-verification-result-new
            :label "config file"
            :message (if have-config "found" "missing or incorrect")
            :face (if have-config 'success '(bold error)))))))

    (defun flycheck-parse-jscs (output checker buffer)
      "Parse JSCS OUTPUT from CHECKER and BUFFER.

    Like `flycheck-parse-checkstyle', but catches errors about no
    configuration found and prevents to be reported as a suspicious
    error."
      (if (string-match-p (rx string-start "No configuration found") output)
          (let ((message "No JSCS configuration found.  Set `flycheck-jscsrc' for JSCS"))
            (list (flycheck-error-new-at 1 nil 'warning message
                                         :checker checker
                                         :buffer buffer
                                         :filename (buffer-file-name buffer))))
        (flycheck-parse-checkstyle output checker buffer)))

    (flycheck-def-config-file-var flycheck-jscsrc javascript-jscs ".jscsrc"
      :safe #'stringp
      :package-version '(flycheck . "0.24"))

    (flycheck-define-checker javascript-jscs
      "A Javascript style checker using JSCS.

    See URL `http://www.jscs.info'."
      :command ("jscs" "--reporter=checkstyle"
                (config-file "--config" flycheck-jscsrc)
                "-")
      :standard-input t
      :error-parser flycheck-parse-jscs
      :error-filter (lambda (errors)
                      (flycheck-remove-error-ids
                       (flycheck-sanitize-errors
                        (flycheck-remove-error-file-names "input" errors))))
      :modes (js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode))

    (flycheck-define-checker javascript-standard
      "A Javascript code and style checker for the (Semi-)Standard Style.

    This checker works with `standard' and `semistandard', defaulting
    to the former.  To use it with the latter, set
    `flycheck-javascript-standard-executable' to `semistandard'.

    See URL `https://github.com/feross/standard' and URL
    `https://github.com/Flet/semistandard'."
      :command ("standard" "--stdin")
      :standard-input t
      :error-patterns
      ((error line-start "  <text>:" line ":" column ":" (message) line-end))
      :modes (js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode rjsx-mode))

    (flycheck-define-checker json-jsonlint
      "A JSON syntax and style checker using jsonlint.

    See URL `https://github.com/zaach/jsonlint'."
      ;; We can't use standard input for jsonlint, because it doesn't output errors
      ;; anymore when using -c -q with standard input :/
      :command ("jsonlint" "-c" "-q" source)
      :error-patterns
      ((error line-start
              (file-name)
              ": line " line
              ", col " column ", "
              (message) line-end))
      :error-filter
      (lambda (errors)
        (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
      :modes json-mode)

    (flycheck-define-checker json-python-json
      "A JSON syntax checker using Python json.tool module.

    See URL `https://docs.python.org/3.5/library/json.html#command-line-interface'."
      :command ("python" "-m" "json.tool" source
                ;; Send the pretty-printed output to the null device
                null-device)
      :error-patterns
      ((error line-start
              (message) ": line " line " column " column
              ;; Ignore the rest of the line which shows the char position.
              (one-or-more not-newline)
              line-end))
      :modes json-mode
      ;; The JSON parser chokes if the buffer is empty and has no JSON inside
      :predicate (lambda () (not (flycheck-buffer-empty-p))))

    (flycheck-define-checker less
      "A LESS syntax checker using lessc.

    Requires lessc 1.4 or newer.

    See URL `http://lesscss.org'."
      :command ("lessc" "--lint" "--no-color"
                "-")
      :standard-input t
      :error-patterns
      ((error line-start (one-or-more word) ":"
              (message)
              " in - on line " line
              ", column " column ":"
              line-end))
      :modes less-css-mode)

    (flycheck-define-checker less-stylelint
      "A LESS syntax and style checker using stylelint.

    See URL `http://stylelint.io/'."
      :command ("stylelint"
                (eval flycheck-stylelint-args)
                "--syntax" "less"
                (option-flag "--quiet" flycheck-stylelint-quiet)
                (config-file "--config" flycheck-stylelintrc))
      :standard-input t
      :error-parser flycheck-parse-stylelint
      :modes (less-css-mode))

    (flycheck-define-checker llvm-llc
      "Flycheck LLVM IR checker using llc.

    See URL `http://llvm.org/docs/CommandGuide/llc.html'."
      :command ("llc" "-o" null-device source)
      :error-patterns
      ((error line-start
              ;; llc prints the executable path
              (zero-or-one (minimal-match (one-or-more not-newline)) ": ")
              (file-name) ":" line ":" column ": error: " (message)
              line-end))
      :error-filter
      (lambda (errors)
        ;; sanitize errors occurring in inline assembly
        (flycheck-sanitize-errors
         (flycheck-remove-error-file-names "<inline asm>" errors)))
      :modes llvm-mode)

    (flycheck-def-config-file-var flycheck-luacheckrc lua-luacheck ".luacheckrc"
      :safe #'stringp)

    (flycheck-def-option-var flycheck-luacheck-standards nil lua-luacheck
      "The standards to use in luacheck.

    The value of this variable is either a list of strings denoting
    the standards to use, or nil to pass nothing to luacheck.  When
    non-nil, pass the standards via one or more `--std' options."
      :type '(choice (const :tag "Default" nil)
                     (repeat :tag "Custom standards"
                             (string :tag "Standard name")))
      :safe #'flycheck-string-list-p)
    (make-variable-buffer-local 'flycheck-luacheck-standards)

    (flycheck-define-checker lua-luacheck
      "A Lua syntax checker using luacheck.

    See URL `https://github.com/mpeterv/luacheck'."
      :command ("luacheck"
                "--formatter" "plain"
                "--codes"                   ; Show warning codes
                "--no-color"
                (option-list "--std" flycheck-luacheck-standards)
                (config-file "--config" flycheck-luacheckrc)
                "--filename" source-original
                ;; Read from standard input
                "-")
      :standard-input t
      :error-patterns
      ((warning line-start
                (optional (file-name))
                ":" line ":" column
                ": (" (id "W" (one-or-more digit)) ") "
                (message) line-end)
       (error line-start
              (optional (file-name))
              ":" line ":" column ":"
              ;; `luacheck' before 0.11.0 did not output codes for errors, hence
              ;; the ID is optional here
              (optional " (" (id "E" (one-or-more digit)) ") ")
              (message) line-end))
      :modes lua-mode)

    (flycheck-define-checker lua
      "A Lua syntax checker using the Lua compiler.

    See URL `http://www.lua.org/'."
      :command ("luac" "-p" "-")
      :standard-input t
      :error-patterns
      ((error line-start
              ;; Skip the name of the luac executable.
              (minimal-match (zero-or-more not-newline))
              ": stdin:" line ": " (message) line-end))
      :modes lua-mode)

    (flycheck-def-option-var flycheck-perl-include-path nil perl
      "A list of include directories for Perl.

    The value of this variable is a list of strings, where each
    string is a directory to add to the include path of Perl.
    Relative paths are relative to the file being checked."
      :type '(repeat (directory :tag "Include directory"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.24"))

    (flycheck-define-checker perl
      "A Perl syntax checker using the Perl interpreter.

    See URL `https://www.perl.org'."
      :command ("perl" "-w" "-c"
                (option-list "-I" flycheck-perl-include-path))
      :standard-input t
      :error-patterns
      ((error line-start (minimal-match (message))
              " at - line " line
              (or "." (and ", " (zero-or-more not-newline))) line-end))
      :modes (perl-mode cperl-mode)
      :next-checkers (perl-perlcritic))

    (flycheck-def-option-var flycheck-perlcritic-severity nil perl-perlcritic
      "The message severity for Perl Critic.

    The value of this variable is a severity level as integer, for
    the `--severity' option to Perl Critic."
      :type '(integer :tag "Severity level")
      :safe #'integerp
      :package-version '(flycheck . "0.18"))

    (flycheck-def-config-file-var flycheck-perlcriticrc perl-perlcritic
                                  ".perlcriticrc"
      :safe #'stringp
      :package-version '(flycheck . "26"))

    (flycheck-define-checker perl-perlcritic
      "A Perl syntax checker using Perl::Critic.

    See URL `https://metacpan.org/pod/Perl::Critic'."
      :command ("perlcritic" "--no-color" "--verbose" "%f/%l/%c/%s/%p/%m (%e)\n"
                (config-file "--profile" flycheck-perlcriticrc)
                (option "--severity" flycheck-perlcritic-severity nil
                        flycheck-option-int))
      :standard-input t
      :error-patterns
      ((info line-start
             "STDIN/" line "/" column "/" (any "1") "/"
             (id (one-or-more (not (any "/")))) "/" (message)
             line-end)
       (warning line-start
                "STDIN/" line "/" column "/" (any "234") "/"
                (id (one-or-more (not (any "/")))) "/" (message)
                line-end)
       (error line-start
              "STDIN/" line "/" column "/" (any "5") "/"
              (id (one-or-more (not (any "/")))) "/" (message)
              line-end))
      :modes (cperl-mode perl-mode))

    (flycheck-define-checker php
      "A PHP syntax checker using the PHP command line interpreter.

    See URL `http://php.net/manual/en/features.commandline.php'."
      :command ("php" "-l" "-d" "error_reporting=E_ALL" "-d" "display_errors=1"
                "-d" "log_errors=0" source)
      :error-patterns
      ((error line-start (or "Parse" "Fatal" "syntax") " error" (any ":" ",") " "
              (message) " in " (file-name) " on line " line line-end))
      :modes (php-mode php+-mode)
      :next-checkers ((warning . php-phpmd)
                      (warning . php-phpcs)))

    (flycheck-def-option-var flycheck-phpmd-rulesets
        '("cleancode" "codesize" "controversial" "design" "naming" "unusedcode")
        php-phpmd
      "The rule sets for PHP Mess Detector.

    Set default rule sets and custom rule set files.

    See section \"Using multiple rule sets\" in the PHP Mess Detector
    manual at URL `https://phpmd.org/documentation/index.html'."
      :type '(repeat :tag "rule sets"
                     (string :tag "A filename or rule set"))
      :safe #'flycheck-string-list-p)

    (flycheck-define-checker php-phpmd
      "A PHP style checker using PHP Mess Detector.

    See URL `https://phpmd.org/'."
      :command ("phpmd" source "xml"
                (eval (flycheck-option-comma-separated-list
                       flycheck-phpmd-rulesets)))
      :error-parser flycheck-parse-phpmd
      :modes (php-mode php+-mode)
      :next-checkers (php-phpcs))

    (flycheck-def-option-var flycheck-phpcs-standard nil php-phpcs
      "The coding standard for PHP CodeSniffer.

    When nil, use the default standard from the global PHP
    CodeSniffer configuration.  When set to a string, pass the string
    to PHP CodeSniffer which will interpret it as name as a standard,
    or as path to a standard specification."
      :type '(choice (const :tag "Default standard" nil)
                     (string :tag "Standard name or file"))
      :safe #'stringp)

    (flycheck-define-checker php-phpcs
      "A PHP style checker using PHP Code Sniffer.

    Needs PHP Code Sniffer 2.6 or newer.

    See URL `http://pear.php.net/package/PHP_CodeSniffer/'."
      :command ("phpcs" "--report=checkstyle"
                ;; Use -q flag to force quiet mode
                ;; Quiet mode prevents errors from extra output when phpcs has
                ;; been configured with show_progress enabled
                "-q"
                (option "--standard=" flycheck-phpcs-standard concat)
                ;; Pass original file name to phpcs.  We need to concat explicitly
                ;; here, because phpcs really insists to get option and argument as
                ;; a single command line argument :|
                (eval (when (buffer-file-name)
                        (concat "--stdin-path=" (buffer-file-name)))))
      :standard-input t
      :error-parser flycheck-parse-checkstyle
      :error-filter
      (lambda (errors)
        (flycheck-sanitize-errors
         (flycheck-remove-error-file-names "STDIN" errors)))
      :modes (php-mode php+-mode)
      ;; phpcs seems to choke on empty standard input, hence skip phpcs if the
      ;; buffer is empty, see https://github.com/flycheck/flycheck/issues/907
      :predicate (lambda () (not (flycheck-buffer-empty-p))))

    (flycheck-define-checker processing
      "Processing command line tool.

    See https://github.com/processing/processing/wiki/Command-Line"
      :command ("processing-java" "--force"
                ;; Don't change the order of these arguments, processing is pretty
                ;; picky
                (eval (concat "--sketch=" (file-name-directory (buffer-file-name))))
                (eval (concat "--output=" (flycheck-temp-dir-system)))
                "--build")
      :error-patterns
      ((error line-start (file-name) ":" line ":" column
              (zero-or-more (or digit ":")) (message) line-end))
      :modes processing-mode
      ;; This syntax checker needs a file name
      :predicate (lambda () (buffer-file-name)))

    (defun flycheck-proselint-parse-errors (output checker buffer)
      "Parse proselint json output errors from OUTPUT.

    CHECKER and BUFFER denoted the CHECKER that returned OUTPUT and
    the BUFFER that was checked respectively.

    See URL `http://proselint.com/' for more information about proselint."
      (mapcar (lambda (err)
                (let-alist err
                  (flycheck-error-new-at
                   .line
                   .column
                   (pcase .severity
                     (`"suggestion" 'info)
                     (`"warning"    'warning)
                     (`"error"      'error)
                     ;; Default to error
                     (_             'error))
                   .message
                   :id .check
                   :buffer buffer
                   :checker checker)))
              (let-alist (car (flycheck-parse-json output))
                .data.errors)))

    (flycheck-define-checker proselint
      "Flycheck checker using Proselint.

    See URL `http://proselint.com/'."
      :command ("proselint" "--json" "-")
      :standard-input t
      :error-parser flycheck-proselint-parse-errors
      :modes (text-mode markdown-mode gfm-mode))

    (flycheck-define-checker protobuf-protoc
      "A protobuf syntax checker using the protoc compiler.

    See URL `https://developers.google.com/protocol-buffers/'."
      :command ("protoc" "--error_format" "gcc"
                (eval (concat "--java_out=" (flycheck-temp-dir-system)))
                ;; Add the file directory of protobuf path to resolve import directives
                (eval (concat "--proto_path=" (file-name-directory (buffer-file-name))))
                source-inplace)
      :error-patterns
      ((info line-start (file-name) ":" line ":" column
             ": note: " (message) line-end)
       (error line-start (file-name) ":" line ":" column
              ": " (message) line-end)
       (error line-start
              (message "In file included from") " " (file-name) ":" line ":"
              column ":" line-end))
      :modes protobuf-mode
      :predicate (lambda () (buffer-file-name)))

    (flycheck-define-checker pug
      "A Pug syntax checker using the pug compiler.

    See URL `https://pugjs.org/'."
      :command ("pug" "-p" (eval (expand-file-name (buffer-file-name))))
      :standard-input t
      :error-patterns
      ;; errors with includes/extends (e.g. missing files)
      ((error "Error: " (message) (zero-or-more not-newline) "\n"
              (zero-or-more not-newline) "at "
              (zero-or-more not-newline) " line " line)
       ;; syntax/runtime errors (e.g. type errors, bad indentation, etc.)
       (error line-start
              (optional "Type") "Error: "  (file-name) ":" line (optional ":" column)
              (zero-or-more not-newline) "\n"
              (one-or-more (or (zero-or-more not-newline) "|"
                               (zero-or-more not-newline) "\n")
                           (zero-or-more "-")  (zero-or-more not-newline) "|"
                           (zero-or-more not-newline) "\n")
              (zero-or-more not-newline) "\n"
              (one-or-more
               (zero-or-more not-newline) "|"
               (zero-or-more not-newline) "\n") (zero-or-more not-newline) "\n"
               (message) line-end))
      :modes pug-mode)

    (flycheck-define-checker puppet-parser
      "A Puppet DSL syntax checker using puppet's own parser.

    See URL `https://puppet.com/'."
      :command ("puppet" "parser" "validate" "--color=false")
      :standard-input t
      :error-patterns
      (
       ;; Patterns for Puppet 4
       (error line-start "Error: Could not parse for environment "
              (one-or-more (in "a-z" "0-9" "_")) ":"
              (message) " at line " line ":" column line-end)
       ;; Errors from Puppet < 4
       (error line-start "Error: Could not parse for environment "
              (one-or-more (in "a-z" "0-9" "_")) ":"
              (message (minimal-match (one-or-more anything)))
              " at line " line line-end)
       (error line-start
              ;; Skip over the path of the Puppet executable
              (minimal-match (zero-or-more not-newline))
              ": Could not parse for environment " (one-or-more word)
              ": " (message (minimal-match (zero-or-more anything)))
              " at " (file-name "/" (zero-or-more not-newline)) ":" line line-end))
      :modes puppet-mode
      :next-checkers ((warning . puppet-lint)))

    (flycheck-def-config-file-var flycheck-puppet-lint-rc puppet-lint
                                  ".puppet-lint.rc"
      :safe #'stringp
      :package-version '(flycheck . "26"))

    (flycheck-def-option-var flycheck-puppet-lint-disabled-checks nil puppet-lint
      "Disabled checkers for `puppet-lint'.

    The value of this variable is a list of strings, where each
    string is the name of a check to disable (e.g. \"80chars\" or
    \"double_quoted_strings\").

    See URL `http://puppet-lint.com/checks/' for a list of all checks
    and their names."
      :type '(repeat (string :tag "Check Name"))
      :package-version '(flycheck . "26"))

    (defun flycheck-puppet-lint-disabled-arg-name (check)
      "Create an argument to disable a puppetlint CHECK."
      (concat "--no-" check "-check"))

    (flycheck-define-checker puppet-lint
      "A Puppet DSL style checker using puppet-lint.

    See URL `http://puppet-lint.com/'."
      ;; We must check the original file, because Puppetlint is quite picky on the
      ;; names of files and there place in the directory structure, to comply with
      ;; Puppet's autoload directory layout.  For instance, a class foo::bar is
      ;; required to be in a file foo/bar.pp.  Any other place, such as a Flycheck
      ;; temporary file will cause an error.
      :command ("puppet-lint"
                (config-file "--config" flycheck-puppet-lint-rc)
                "--log-format"
                "%{path}:%{line}:%{kind}: %{message} (%{check})"
                (option-list "" flycheck-puppet-lint-disabled-checks concat
                             flycheck-puppet-lint-disabled-arg-name)
                source-original)
      :error-patterns
      ((warning line-start (file-name) ":" line ":warning: " (message) line-end)
       (error line-start (file-name) ":" line ":error: " (message) line-end))
      :modes puppet-mode
      ;; Since we check the original file, we can only use this syntax checker if
      ;; the buffer is actually linked to a file, and if it is not modified.
      :predicate flycheck-buffer-saved-p)

    (flycheck-def-config-file-var flycheck-flake8rc python-flake8 ".flake8rc"
      :safe #'stringp)

    (flycheck-def-option-var flycheck-flake8-error-level-alist
        '(("^E9.*$"  . error)               ; Syntax errors from pep8
          ("^F82.*$" . error)               ; undefined variables from pyflakes
          ("^F83.*$" . error)               ; Duplicate arguments from flake8
          ("^D.*$"   . info)                ; Docstring issues from flake8-pep257
          ("^N.*$"   . info)                ; Naming issues from pep8-naming
          )
        python-flake8
      "An alist mapping flake8 error IDs to Flycheck error levels.

    Each item in this list is a cons cell `(PATTERN . LEVEL)' where
    PATTERN is a regular expression matched against the error ID, and
    LEVEL is a Flycheck error level symbol.

    Each PATTERN is matched in the order of appearance in this list
    against the error ID.  If it matches the ID, the level of the
    corresponding error is set to LEVEL.  An error that is not
    matched by any PATTERN defaults to warning level.

    The default value of this option matches errors from flake8
    itself and from the following flake8 plugins:

    - pep8-naming
    - flake8-pep257

    You may add your own mappings to this option in order to support
    further flake8 plugins."
      :type '(repeat (cons (regexp :tag "Error ID pattern")
                           (symbol :tag "Error level")))
      :package-version '(flycheck . "0.22"))

    (flycheck-def-option-var flycheck-flake8-maximum-complexity nil python-flake8
      "The maximum McCabe complexity of methods.

    If nil, do not check the complexity of methods.  If set to an
    integer, report any complexity greater than the value of this
    variable as warning.

    If set to an integer, this variable overrules any similar setting
    in the configuration file denoted by `flycheck-flake8rc'."
      :type '(choice (const :tag "Do not check McCabe complexity" nil)
                     (integer :tag "Maximum complexity"))
      :safe #'integerp)

    (flycheck-def-option-var flycheck-flake8-maximum-line-length nil python-flake8
      "The maximum length of lines.

    If set to an integer, the value of this variable denotes the
    maximum length of lines, overruling any similar setting in the
    configuration file denoted by `flycheck-flake8rc'.  An error will
    be reported for any line longer than the value of this variable.

    If set to nil, use the maximum line length from the configuration
    file denoted by `flycheck-flake8rc', or the PEP 8 recommendation
    of 79 characters if there is no configuration with this setting."
      :type '(choice (const :tag "Default value")
                     (integer :tag "Maximum line length in characters"))
      :safe #'integerp)

    (defun flycheck-flake8-fix-error-level (err)
      "Fix the error level of ERR.

    Update the error level of ERR according to
    `flycheck-flake8-error-level-alist'."
      (pcase-dolist (`(,pattern . ,level) flycheck-flake8-error-level-alist)
        (when (string-match-p pattern (flycheck-error-id err))
          (setf (flycheck-error-level err) level)))
      err)

    (flycheck-define-checker python-flake8
      "A Python syntax and style checker using Flake8.

    Requires Flake8 3.0 or newer. See URL
    `https://flake8.readthedocs.io/'."
      :command ("flake8"
                "--format=default"
                (config-file "--config" flycheck-flake8rc)
                (option "--max-complexity" flycheck-flake8-maximum-complexity nil
                        flycheck-option-int)
                (option "--max-line-length" flycheck-flake8-maximum-line-length nil
                        flycheck-option-int)
                "-")
      :standard-input t
      :error-filter (lambda (errors)
                      (let ((errors (flycheck-sanitize-errors errors)))
                        (seq-map #'flycheck-flake8-fix-error-level errors)))
      :error-patterns
      ((warning line-start
                "stdin:" line ":" (optional column ":") " "
                (id (one-or-more (any alpha)) (one-or-more digit)) " "
                (message (one-or-more not-newline))
                line-end))
      :modes python-mode)

    (flycheck-def-config-file-var flycheck-pylintrc python-pylint
                                  ".pylintrc"
      :safe #'stringp)

    (flycheck-def-option-var flycheck-pylint-use-symbolic-id t python-pylint
      "Whether to use pylint message symbols or message codes.

    A pylint message has both an opaque identifying code (such as `F0401') and a
    more meaningful symbolic code (such as `import-error').  This option governs
    which should be used and reported to the user."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.25"))

    (flycheck-define-checker python-pylint
      "A Python syntax and style checker using Pylint.

    This syntax checker requires Pylint 1.0 or newer.

    See URL `https://www.pylint.org/'."
      ;; -r n disables the scoring report
      :command ("pylint" "-r" "n"
                "--output-format" "text"
                "--msg-template"
                (eval (if flycheck-pylint-use-symbolic-id
                          "{path}:{line}:{column}:{C}:{symbol}:{msg}"
                        "{path}:{line}:{column}:{C}:{msg_id}:{msg}"))
                (config-file "--rcfile" flycheck-pylintrc)
                ;; Need `source-inplace' for relative imports (e.g. `from .foo
                ;; import bar'), see https://github.com/flycheck/flycheck/issues/280
                source-inplace)
      :error-filter
      (lambda (errors)
        (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
      :error-patterns
      ((error line-start (file-name) ":" line ":" column ":"
              (or "E" "F") ":"
              (id (one-or-more (not (any ":")))) ":"
              (message) line-end)
       (warning line-start (file-name) ":" line ":" column ":"
                (or "W" "R") ":"
                (id (one-or-more (not (any ":")))) ":"
                (message) line-end)
       (info line-start (file-name) ":" line ":" column ":"
             "C:" (id (one-or-more (not (any ":")))) ":"
             (message) line-end))
      :modes python-mode)

    (flycheck-define-checker python-pycompile
      "A Python syntax checker using Python's builtin compiler.

    See URL `https://docs.python.org/3.4/library/py_compile.html'."
      :command ("python" "-m" "py_compile" source)
      :error-patterns
      ;; Python 2.7
      ((error line-start "  File \"" (file-name) "\", line " line "\n"
              (>= 2 (zero-or-more not-newline) "\n")
              "SyntaxError: " (message) line-end)
       (error line-start "Sorry: IndentationError: "
              (message) "(" (file-name) ", line " line ")"
              line-end)
       ;; 2.6
       (error line-start "SyntaxError: ('" (message (one-or-more (not (any "'"))))
              "', ('" (file-name (one-or-more (not (any "'")))) "', "
              line ", " column ", " (one-or-more not-newline) line-end))
      :modes python-mode)

    (flycheck-def-option-var flycheck-lintr-caching t r-lintr
      "Whether to enable caching in lintr.

    By default, lintr caches all expressions in a file and re-checks
    only those that have changed.  Setting this option to nil
    disables caching in case there are problems."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.23"))

    (flycheck-def-option-var flycheck-lintr-linters "default_linters" r-lintr
      "Linters to use with lintr.

    The value of this variable is a string containing an R
    expression, which selects linters for lintr."
      :type 'string
      :risky t
      :package-version '(flycheck . "0.23"))

    (defun flycheck-r-has-lintr (R)
      "Whether R has installed the `lintr' library."
      (with-temp-buffer
        (let ((process-environment (append '("LC_ALL=C") process-environment)))
          (call-process R nil t nil
                        "--slave" "--restore" "--no-save" "-e"
                        "library('lintr')")
          (goto-char (point-min))
          (not (re-search-forward "there is no package called 'lintr'" nil 'no-error)))))

    (flycheck-define-checker r-lintr
      "An R style and syntax checker using the lintr package.

    See URL `https://github.com/jimhester/lintr'."
      :command ("R" "--slave" "--restore" "--no-save" "-e"
                (eval (concat
                       "library(lintr);"
                       "try(lint(commandArgs(TRUE)"
                       ", cache=" (if flycheck-lintr-caching "TRUE" "FALSE")
                       ", " flycheck-lintr-linters
                       "))"))
                "--args" source)
      :error-patterns
      ((info line-start (file-name) ":" line ":" column ": style: " (message)
             line-end)
       (warning line-start (file-name) ":" line ":" column ": warning: " (message)
                line-end)
       (error line-start (file-name) ":" line ":" column ": error: " (message)
              line-end))
      :modes ess-mode
      :predicate
      ;; Don't check ESS files which do not contain R, and make sure that lintr is
      ;; actually available
      (lambda ()
        (and (equal ess-language "S")
             (flycheck-r-has-lintr (flycheck-checker-executable 'r-lintr))))
      :verify (lambda (checker)
                (let ((has-lintr (flycheck-r-has-lintr
                                  (flycheck-checker-executable checker))))
                  (list
                   (flycheck-verification-result-new
                    :label "lintr library"
                    :message (if has-lintr "present" "missing")
                    :face (if has-lintr 'success '(bold error)))))))

    (defun flycheck-racket-has-expand-p (checker)
      "Whether the executable of CHECKER provides the `expand' command."
      (let ((raco (flycheck-find-checker-executable checker)))
        (when raco
          (with-temp-buffer
            (call-process raco nil t nil "expand")
            (goto-char (point-min))
            (not (looking-at-p (rx bol (1+ not-newline)
                                   "Unrecognized command: expand"
                                   eol)))))))

    (flycheck-define-checker racket
      "A Racket syntax checker with `raco expand'.

    The `compiler-lib' racket package is required for this syntax
    checker.

    See URL `https://racket-lang.org/'."
      :command ("raco" "expand" source-inplace)
      :predicate
      (lambda ()
        (and (or (not (eq major-mode 'scheme-mode))
                 ;; In `scheme-mode' we must check the current Scheme implementation
                 ;; being used
                 (and (boundp 'geiser-impl--implementation)
                      (eq geiser-impl--implementation 'racket)))
             (flycheck-racket-has-expand-p 'racket)))
      :verify
      (lambda (checker)
        (let ((has-expand (flycheck-racket-has-expand-p checker))
              (in-scheme-mode (eq major-mode 'scheme-mode))
              (geiser-impl (bound-and-true-p geiser-impl--implementation)))
          (list
           (flycheck-verification-result-new
            :label "compiler-lib package"
            :message (if has-expand "present" "missing")
            :face (if has-expand 'success '(bold error)))
           (flycheck-verification-result-new
            :label "Geiser Implementation"
            :message (cond
                      ((not in-scheme-mode) "Using Racket Mode")
                      ((eq geiser-impl 'racket) "Racket")
                      (geiser-impl (format "Other: %s" geiser-impl))
                      (t "Geiser not active"))
            :face (cond
                   ((or (not in-scheme-mode) (eq geiser-impl 'racket)) 'success)
                   (t '(bold error)))))))
      :error-filter
      (lambda (errors)
        (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
      :error-patterns
      ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
      :modes (racket-mode scheme-mode))

    (flycheck-define-checker rpm-rpmlint
      "A RPM SPEC file syntax checker using rpmlint.

    See URL `https://sourceforge.net/projects/rpmlint/'."
      :command ("rpmlint" source)
      :error-patterns
      ((error line-start
              (file-name) ":" (optional line ":") " E: " (message)
              line-end)
       (warning line-start
                (file-name) ":" (optional line ":") " W: " (message)
                line-end))
      :error-filter
      ;; Add fake line numbers if they are missing in the lint output
      (lambda (errors)
        (dolist (err errors)
          (unless (flycheck-error-line err)
            (setf (flycheck-error-line err) 1)))
        errors)
      :error-explainer
      (lambda (error)
        (-when-let* ((error-message (flycheck-error-message error))
                     (message-id (save-match-data (string-match "\\([^ ]+\\)" error-message)
                                                  (match-string 1 error-message))))
          (with-output-to-string
            (call-process "rpmlint" nil standard-output nil "-I" message-id))))
      :modes (sh-mode rpm-spec-mode)
      :predicate (lambda () (or (not (eq major-mode 'sh-mode))
                                ;; In `sh-mode', we need the proper shell
                                (eq sh-shell 'rpm))))

    (flycheck-def-option-var flycheck-markdown-mdl-rules nil markdown-mdl
      "Rules to enable for mdl.

    The value of this variable is a list of strings each of which is
    the name of a rule to enable.

    By default all rules are enabled.

    See URL
    `https://github.com/mivok/markdownlint/blob/master/docs/configuration.md'."
      :type '(repeat :tag "Enabled rules"
                     (string :tag "rule name"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "27"))

    (flycheck-def-option-var flycheck-markdown-mdl-tags nil markdown-mdl
      "Rule tags to enable for mdl.

    The value of this variable is a list of strings each of which is
    the name of a rule tag.  Only rules with these tags are enabled.

    By default all rules are enabled.

    See URL
    `https://github.com/mivok/markdownlint/blob/master/docs/configuration.md'."
      :type '(repeat :tag "Enabled tags"
                     (string :tag "tag name"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "27"))

    (flycheck-def-config-file-var flycheck-markdown-mdl-style markdown-mdl nil
      :safe #'stringp
      :package-version '(flycheck . "27"))

    (flycheck-define-checker markdown-mdl
      "Markdown checker using mdl.

    See URL `https://github.com/mivok/markdownlint'."
      :command ("mdl"
                (config-file "--style" flycheck-markdown-mdl-style)
                (option "--tags=" flycheck-markdown-mdl-rules concat
                        flycheck-option-comma-separated-list)
                (option "--rules=" flycheck-markdown-mdl-rules concat
                        flycheck-option-comma-separated-list))
      :standard-input t
      :error-patterns
      ((error line-start
              (file-name) ":" line ": " (id (one-or-more alnum)) " " (message)
              line-end))
      :error-filter
      (lambda (errors)
        (flycheck-sanitize-errors
         (flycheck-remove-error-file-names "(stdin)" errors)))
      :modes (markdown-mode gfm-mode))

    (flycheck-define-checker nix
      "Nix checker using nix-instantiate.

    See URL `https://nixos.org/nix/manual/#sec-nix-instantiate'."
      :command ("nix-instantiate" "--parse" "-")
      :standard-input t
      :error-patterns
      ((error line-start
              "error: " (message) " at " (file-name) ":" line ":" column
              line-end))
      :error-filter
      (lambda (errors)
        (flycheck-sanitize-errors
         (flycheck-remove-error-file-names "(string)" errors)))
      :modes nix-mode)

    (defun flycheck-locate-sphinx-source-directory ()
      "Locate the Sphinx source directory for the current buffer.

    Return the source directory, or nil, if the current buffer is not
    part of a Sphinx project."
      (-when-let* ((filename (buffer-file-name))
                   (dir (locate-dominating-file filename "conf.py")))
        (expand-file-name dir)))

    (flycheck-define-checker rst
      "A ReStructuredText (RST) syntax checker using Docutils.

    See URL `http://docutils.sourceforge.net/'."
      ;; We need to use source-inplace to properly resolve relative paths in
      ;; include:: directives
      :command ("rst2pseudoxml.py" "--report=2" "--halt=5"
                ;; Read from standard input and throw output away
                "-" null-device)
      :standard-input t
      :error-patterns
      ((warning line-start "<stdin>:" line ": (WARNING/2) " (message) line-end)
       (error line-start "<stdin>:" line
              ": (" (or "ERROR/3" "SEVERE/4") ") "
              (message) line-end))
      :modes rst-mode)

    (flycheck-def-option-var flycheck-sphinx-warn-on-missing-references t rst-sphinx
      "Whether to warn about missing references in Sphinx.

    When non-nil (the default), warn about all missing references in
    Sphinx via `-n'."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.17"))

    (flycheck-define-checker rst-sphinx
      "A ReStructuredText (RST) syntax checker using Sphinx.

    Requires Sphinx 1.2 or newer.  See URL `http://sphinx-doc.org'."
      :command ("sphinx-build" "-b" "pseudoxml"
                "-q" "-N"                   ; Reduced output and no colors
                (option-flag "-n" flycheck-sphinx-warn-on-missing-references)
                (eval (flycheck-locate-sphinx-source-directory))
                temporary-directory         ; Redirect the output to a temporary
                                            ; directory
                source-original)            ; Sphinx needs the original document
      :error-patterns
      ((warning line-start (file-name) ":" line ": WARNING: " (message) line-end)
       (error line-start
              (file-name) ":" line
              ": " (or "ERROR" "SEVERE") ": "
              (message) line-end))
      :modes rst-mode
      :predicate (lambda () (and (flycheck-buffer-saved-p)
                                 (flycheck-locate-sphinx-source-directory))))

    (flycheck-def-config-file-var flycheck-rubocoprc ruby-rubocop ".rubocop.yml"
      :safe #'stringp)

    (flycheck-def-option-var flycheck-rubocop-lint-only nil ruby-rubocop
      "Whether to only report code issues in Rubocop.

    When non-nil, only report code issues in Rubocop, via `--lint'.
    Otherwise report style issues as well."
      :safe #'booleanp
      :type 'boolean
      :package-version '(flycheck . "0.16"))

    (flycheck-define-checker ruby-rubocop
      "A Ruby syntax and style checker using the RuboCop tool.

    You need at least RuboCop 0.34 for this syntax checker.

    See URL `http://batsov.com/rubocop/'."
      :command ("rubocop" "--display-cop-names" "--format" "emacs"
                ;; Explicitly disable caching to prevent Rubocop 0.35.1 and earlier
                ;; from caching standard input.  Later versions of Rubocop
                ;; automatically disable caching with --stdin, see
                ;; https://github.com/flycheck/flycheck/issues/844 and
                ;; https://github.com/bbatsov/rubocop/issues/2576
                "--cache" "false"
                (config-file "--config" flycheck-rubocoprc)
                (option-flag "--lint" flycheck-rubocop-lint-only)
                ;; Rubocop takes the original file name as argument when reading
                ;; from standard input
                "--stdin" source-original)
      :standard-input t
      :error-patterns
      ((info line-start (file-name) ":" line ":" column ": C: "
             (optional (id (one-or-more (not (any ":")))) ": ") (message) line-end)
       (warning line-start (file-name) ":" line ":" column ": W: "
                (optional (id (one-or-more (not (any ":")))) ": ") (message)
                line-end)
       (error line-start (file-name) ":" line ":" column ": " (or "E" "F") ": "
              (optional (id (one-or-more (not (any ":")))) ": ") (message)
              line-end))
      :modes (enh-ruby-mode ruby-mode)
      :next-checkers ((warning . ruby-reek)
                      (warning . ruby-rubylint)))

    ;; Default to `nil' to let Reek find its configuration file by itself
    (flycheck-def-config-file-var flycheck-reekrc ruby-reek nil
      :safe #'string-or-null-p
      :package-version '(flycheck . "30"))

    (flycheck-define-checker ruby-reek
      "A Ruby smell checker using reek.

    See URL `https://github.com/troessner/reek'."
      :command ("reek" "--format" "json"
                (config-file "--config" flycheck-reekrc)
                source)
      :error-parser flycheck-parse-reek
      :modes (enh-ruby-mode ruby-mode)
      :next-checkers ((warning . ruby-rubylint)))

    ;; Default to `nil' to let Rubylint find its configuration file by itself, and
    ;; to maintain backwards compatibility with older Rubylint and Flycheck releases
    (flycheck-def-config-file-var flycheck-rubylintrc ruby-rubylint nil
      :safe #'stringp)

    (flycheck-define-checker ruby-rubylint
      "A Ruby syntax and code analysis checker using ruby-lint.

    Requires ruby-lint 2.0.2 or newer.  See URL
    `https://github.com/YorickPeterse/ruby-lint'."
      :command ("ruby-lint" "--presenter=syntastic"
                (config-file "--config" flycheck-rubylintrc)
                source)
      ;; Ruby Lint can't read from standard input
      :error-patterns
      ((info line-start
             (file-name) ":I:" line ":" column ": " (message) line-end)
       (warning line-start
                (file-name) ":W:" line ":" column ": " (message) line-end)
       (error line-start
              (file-name) ":E:" line ":" column ": " (message) line-end))
      :modes (enh-ruby-mode ruby-mode))

    (flycheck-define-checker ruby
      "A Ruby syntax checker using the standard Ruby interpreter.

    Please note that the output of different Ruby versions and
    implementations varies wildly.  This syntax checker supports
    current versions of MRI and JRuby, but may break when used with
    other implementations or future versions of these
    implementations.

    Please consider using `ruby-rubocop' or `ruby-reek' instead.

    See URL `https://www.ruby-lang.org/'."
      :command ("ruby" "-w" "-c")
      :standard-input t
      :error-patterns
      ;; These patterns support output from JRuby, too, to deal with RVM or Rbenv
      ((error line-start "SyntaxError in -:" line ": " (message) line-end)
       (warning line-start "-:" line ":" (optional column ":")
                " warning: " (message) line-end)
       (error line-start "-:" line ": " (message) line-end))
      :modes (enh-ruby-mode ruby-mode)
      :next-checkers ((warning . ruby-rubylint)))

    (flycheck-define-checker ruby-jruby
      "A Ruby syntax checker using the JRuby interpreter.

    This syntax checker is very primitive, and may break on future
    versions of JRuby.

    Please consider using `ruby-rubocop' or `ruby-rubylint' instead.

    See URL `http://jruby.org/'."
      :command ("jruby" "-w" "-c")
      :standard-input t
      :error-patterns
      ((error line-start "SyntaxError in -:" line ": " (message) line-end)
       (warning line-start "-:" line ":" " warning: " (message) line-end)
       (error line-start  "-:" line ": " (message) line-end))
      :modes (enh-ruby-mode ruby-mode)
      :next-checkers ((warning . ruby-rubylint)))

    (flycheck-def-args-var flycheck-cargo-rustc-args (rust-cargo)
      :package-version '(flycheck . "30"))

    (flycheck-def-args-var flycheck-rust-args (rust-cargo rust)
      :package-version '(flycheck . "0.24"))

    (flycheck-def-option-var flycheck-rust-check-tests t (rust-cargo rust)
      "Whether to check test code in Rust.

    When non-nil, `rustc' is passed the `--test' flag, which will
    check any code marked with the `#[cfg(test)]' attribute and any
    functions marked with `#[test]'. Otherwise, `rustc' is not passed
    `--test' and test code will not be checked.  Skipping `--test' is
    necessary when using `#![no_std]', because compiling the test
    runner requires `std'."
      :type 'boolean
      :safe #'booleanp
      :package-version '("flycheck" . "0.19"))

    (flycheck-def-option-var flycheck-rust-crate-root nil rust
      "A path to the crate root for the current buffer.

    The value of this variable is either a string with the path to
    the crate root for the current buffer, or nil if the current buffer
    is a crate.  A relative path is relative to the current buffer.

    If this variable is non nil the current buffer will only be checked
    if it is not modified, i.e. after it has been saved."
      :type 'string
      :package-version '(flycheck . "0.20")
      :safe #'stringp)
    (make-variable-buffer-local 'flycheck-rust-crate-root)

    (flycheck-def-option-var flycheck-rust-crate-type "lib" (rust-cargo rust)
      "The type of the Rust Crate to check.

    For `rust-cargo', the value should be a string denoting the
    target type passed to Cargo.  See
    `flycheck-rust-valid-crate-type-p' for the list of allowed
    values.

    For `rust', the value should be a string denoting the crate type
    for the `--crate-type' flag of rustc."
      :type '(choice (const :tag "nil (rust/rust-cargo)" nil)
                     (const :tag "lib (rust/rust-cargo)" "lib")
                     (const :tag "bin (rust/rust-cargo)" "bin")
                     (const :tag "example (rust-cargo)" "example")
                     (const :tag "test (rust-cargo)" "test")
                     (const :tag "bench (rust-cargo)" "bench")
                     (const :tag "rlib (rust)" "rlib")
                     (const :tag "dylib (rust)" "dylib")
                     (const :tag "cdylib (rust)" "cdylib")
                     (const :tag "staticlib (rust)" "staticlib")
                     (const :tag "metadata (rust)" "metadata"))
      :safe #'stringp
      :package-version '(flycheck . "0.20"))
    (make-variable-buffer-local 'flycheck-rust-crate-type)

    (flycheck-def-option-var flycheck-rust-binary-name nil rust-cargo
      "The name of the binary to pass to `cargo rustc --CRATE-TYPE'.

    The value of this variable is a string denoting the name of the
    target to check: usually the name of the crate, or the name of
    one of the files under `src/bin', `tests', `examples' or
    `benches'.

    This always requires a non-nil value, unless
    `flycheck-rust-crate-type' is `lib' or nil, in which case it is
    ignored."
      :type 'string
      :safe #'stringp
      :package-version '(flycheck . "28"))
    (make-variable-buffer-local 'flycheck-rust-binary-name)

    (flycheck-def-option-var flycheck-rust-library-path nil rust
      "A list of library directories for Rust.

    The value of this variable is a list of strings, where each
    string is a directory to add to the library path of Rust.
    Relative paths are relative to the file being checked."
      :type '(repeat (directory :tag "Library directory"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.18"))

    (defun flycheck-rust-error-explainer (error)
      "Return an explanation text for the given `flycheck-error' ERROR."
      (-when-let (error-code (flycheck-error-id error))
        (with-output-to-string
          (call-process "rustc" nil standard-output nil "--explain" error-code))))

    (defun flycheck-rust-error-filter (errors)
      "Filter ERRORS from rustc output that have no explanatory value."
      (seq-remove (lambda (err)
                    (string-match-p
                     (rx "aborting due to " (optional (one-or-more num) " ")
                         "previous error")
                     (flycheck-error-message err)))
                  errors))

    (defun flycheck-rust-valid-crate-type-p (crate-type)
      "Whether CRATE-TYPE is a valid target type for Cargo.

    A valid Cargo target type is one of `lib', `bin', `example',
    `test' or `bench'."
      (member crate-type '(nil "lib" "bin" "example" "test" "bench")))

    (flycheck-define-checker rust-cargo
      "A Rust syntax checker using Cargo.

    This syntax checker requires Rust 1.15 or newer.  See URL
    `https://www.rust-lang.org'."
      :command ("cargo" "rustc"
                (eval (when flycheck-rust-crate-type
                        (concat "--" flycheck-rust-crate-type)))
                ;; All crate targets except "lib" need a binary name
                (eval (when (and flycheck-rust-crate-type
                                 (not (string= flycheck-rust-crate-type "lib")))
                        flycheck-rust-binary-name))
                "--message-format=json"
                (eval flycheck-cargo-rustc-args)
                "--"
                ;; Passing the "--test" flag when the target is a test binary or
                ;; bench is unnecessary and triggers an error.
                (eval (when flycheck-rust-check-tests
                        (unless (member flycheck-rust-crate-type '("test" "bench"))
                          "--test")))
                (eval flycheck-rust-args))
      :error-parser flycheck-parse-cargo-rustc
      :error-filter flycheck-rust-error-filter
      :error-explainer flycheck-rust-error-explainer
      :modes rust-mode
      :predicate flycheck-buffer-saved-p
      :enabled (lambda ()
                 (-when-let (file (buffer-file-name))
                   (locate-dominating-file file "Cargo.toml")))
      :verify (lambda (_)
                (-when-let (file (buffer-file-name))
                  (let* ((has-toml (locate-dominating-file file "Cargo.toml"))
                         (valid-crate-type (flycheck-rust-valid-crate-type-p
                                            flycheck-rust-crate-type))
                         (need-binary-name
                          (and flycheck-rust-crate-type
                               (not (string= flycheck-rust-crate-type "lib")))))
                    (list
                     (flycheck-verification-result-new
                      :label "Cargo.toml"
                      :message (if has-toml "Found" "Missing")
                      :face (if has-toml 'success '(bold warning)))
                     (flycheck-verification-result-new
                      :label "Crate type"
                      :message (if valid-crate-type
                                   (format "%s" flycheck-rust-crate-type)
                                 (format "%s (invalid, should be one of 'lib', 'bin', 'test', 'example' or 'bench')"
                                         flycheck-rust-crate-type))
                      :face (if valid-crate-type 'success '(bold error)))
                     (flycheck-verification-result-new
                      :label "Binary name"
                      :message (cond
                                ((not need-binary-name) "Not required")
                                ((not flycheck-rust-binary-name) "Required")
                                (t (format "%s" flycheck-rust-binary-name)))
                      :face (cond
                             ((not need-binary-name) 'success)
                             ((not flycheck-rust-binary-name) '(bold error))
                             (t 'success))))))))

    (flycheck-define-checker rust
      "A Rust syntax checker using Rust compiler.

    This syntax checker needs Rust 1.7 or newer.  See URL
    `https://www.rust-lang.org'."
      :command ("rustc"
                (option "--crate-type" flycheck-rust-crate-type)
                "--error-format=json"
                (option-flag "--test" flycheck-rust-check-tests)
                (option-list "-L" flycheck-rust-library-path concat)
                (eval flycheck-rust-args)
                (eval (or flycheck-rust-crate-root
                          (flycheck-substitute-argument 'source-original 'rust))))
      :error-parser flycheck-parse-rustc
      :error-filter flycheck-rust-error-filter
      :error-explainer flycheck-rust-error-explainer
      :modes rust-mode
      :predicate flycheck-buffer-saved-p)

    (defvar flycheck-sass-scss-cache-directory nil
      "The cache directory for `sass' and `scss'.")

    (defun flycheck-sass-scss-cache-location ()
      "Get the cache location for `sass' and `scss'.

    If no cache directory exists yet, create one and return it.
    Otherwise return the previously used cache directory."
      (setq flycheck-sass-scss-cache-directory
            (or flycheck-sass-scss-cache-directory
                (make-temp-file "flycheck-sass-scss-cache" 'directory))))

    (flycheck-def-option-var flycheck-sass-compass nil sass
      "Whether to enable the Compass CSS framework.

    When non-nil, enable the Compass CSS framework, via `--compass'."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.16"))

    (flycheck-define-checker sass
      "A Sass syntax checker using the Sass compiler.

    See URL `http://sass-lang.com'."
      :command ("sass"
                "--cache-location" (eval (flycheck-sass-scss-cache-location))
                (option-flag "--compass" flycheck-sass-compass)
                "--check" "--stdin")
      :standard-input t
      :error-patterns
      ((error line-start
              (or "Syntax error: " "Error: ")
              (message (one-or-more not-newline)
                       (zero-or-more "\n"
                                     (one-or-more " ")
                                     (one-or-more not-newline)))
              (optional "\r") "\n" (one-or-more " ") "on line " line
              " of standard input"
              line-end)
       (warning line-start
                "WARNING: "
                (message (one-or-more not-newline)
                         (zero-or-more "\n"
                                       (one-or-more " ")
                                       (one-or-more not-newline)))
                (optional "\r") "\n" (one-or-more " ") "on line " line
                " of " (one-or-more not-newline)
                line-end))
      :modes sass-mode)

    (flycheck-def-config-file-var flycheck-sass-lintrc sass/scss-sass-lint ".sass-lint.yml"
      :safe #'stringp
      :package-version '(flycheck . "30"))

    (flycheck-define-checker sass/scss-sass-lint
      "A SASS/SCSS syntax checker using sass-Lint.

    See URL `https://github.com/sasstools/sass-lint'."
      :command ("sass-lint"
                "--verbose"
                "--no-exit"
                "--format" "Checkstyle"
                (config-file "--config" flycheck-sass-lintrc)
                source)
      :error-parser flycheck-parse-checkstyle
      :modes (sass-mode scss-mode))

    (flycheck-define-checker scala
      "A Scala syntax checker using the Scala compiler.

    See URL `http://www.scala-lang.org/'."
      :command ("scalac" "-Ystop-after:parser" source)
      :error-patterns
      ((error line-start (file-name) ":" line ": error: " (message) line-end))
      :modes scala-mode
      :next-checkers ((warning . scala-scalastyle)))

    (flycheck-def-config-file-var flycheck-scalastylerc scala-scalastyle nil
      :safe #'stringp
      :package-version '(flycheck . "0.20"))

    (flycheck-define-checker scala-scalastyle
      "A Scala style checker using scalastyle.

    Note that this syntax checker is not used if
    `flycheck-scalastylerc' is nil or refers to a non-existing file.

    See URL `http://www.scalastyle.org'."
      :command ("scalastyle"
                (config-file "-c" flycheck-scalastylerc)
                source)
      :error-patterns
      ((error line-start "error file=" (file-name) " message="
              (message) " line=" line (optional " column=" column) line-end)
       (warning line-start "warning file=" (file-name) " message="
                (message) " line=" line (optional " column=" column) line-end))
      :error-filter (lambda (errors)
                      (flycheck-sanitize-errors
                       (flycheck-increment-error-columns errors)))
      :modes scala-mode
      :predicate
      ;; Inhibit this syntax checker if the JAR or the configuration are unset or
      ;; missing
      (lambda () (and flycheck-scalastylerc
                      (flycheck-locate-config-file flycheck-scalastylerc
                                                   'scala-scalastyle)))
      :verify (lambda (checker)
                (let ((config-file (and flycheck-scalastylerc
                                        (flycheck-locate-config-file
                                         flycheck-scalastylerc checker))))
                  (list
                   (flycheck-verification-result-new
                    :label "Configuration file"
                    :message (cond
                              ((not flycheck-scalastylerc)
                               "`flycheck-scalastyletrc' not set")
                              ((not config-file)
                               (format "file %s not found" flycheck-scalastylerc))
                              (t (format "found at %s" config-file)))
                    :face (cond
                           ((not flycheck-scalastylerc) '(bold warning))
                           ((not config-file) '(bold error))
                           (t 'success)))))))

    (flycheck-define-checker scheme-chicken
      "A CHICKEN Scheme syntax checker using the CHICKEN compiler `csc'.

    See URL `http://call-cc.org/'."
      :command ("csc" "-analyze-only" "-local" source)
      :error-patterns
      ((info line-start
             "Note: " (zero-or-more not-newline) ":\n"
             (one-or-more (any space)) "(" (file-name) ":" line ") " (message)
             line-end)
       (warning line-start
                "Warning: " (zero-or-more not-newline) ":\n"
                (one-or-more (any space)) "(" (file-name) ":" line ") " (message)
                line-end)
       (error line-start "Error: (line " line ") " (message) line-end)
       (error line-start "Syntax error: (" (file-name) ":" line ")"
              (zero-or-more not-newline) " - "
              (message (one-or-more not-newline)
                       (zero-or-more "\n"
                                     (zero-or-more space)
                                     (zero-or-more not-newline))
                       (one-or-more space) "<--")
              line-end)
       (error line-start
              "Error: " (zero-or-more not-newline) ":\n"
              (one-or-more (any space)) "(" (file-name) ":" line ") " (message)
              line-end))
      :predicate
      (lambda ()
        ;; In `scheme-mode' we must check the current Scheme implementation
        ;; being used
        (and (boundp 'geiser-impl--implementation)
             (eq geiser-impl--implementation 'chicken)))
      :verify
      (lambda (_checker)
        (let ((geiser-impl (bound-and-true-p geiser-impl--implementation)))
          (list
           (flycheck-verification-result-new
            :label "Geiser Implementation"
            :message (cond
                      ((eq geiser-impl 'chicken) "Chicken Scheme")
                      (geiser-impl (format "Other: %s" geiser-impl))
                      (t "Geiser not active"))
            :face (cond
                   ((eq geiser-impl 'chicken) 'success)
                   (t '(bold error)))))))
      :modes scheme-mode)

    (defconst flycheck-scss-lint-checkstyle-re
      (rx "cannot load such file" (1+ not-newline) "scss_lint_reporter_checkstyle")
      "Regular expression to parse missing checkstyle error.")

    (defun flycheck-parse-scss-lint (output checker buffer)
      "Parse SCSS-Lint OUTPUT from CHECKER and BUFFER.

    Like `flycheck-parse-checkstyle', but catches errors about
    missing checkstyle reporter from SCSS-Lint."
      (if (string-match-p flycheck-scss-lint-checkstyle-re output)
          (list (flycheck-error-new-at
                 1 nil 'error "Checkstyle reporter for SCSS-Lint missing.
    Please run gem install scss_lint_reporter_checkstyle"
                 :checker checker
                 :buffer buffer
                 :filename (buffer-file-name buffer)))
        (flycheck-parse-checkstyle output checker buffer)))

    (flycheck-def-config-file-var flycheck-scss-lintrc scss-lint ".scss-lint.yml"
      :safe #'stringp
      :package-version '(flycheck . "0.23"))

    (flycheck-define-checker scss-lint
      "A SCSS syntax checker using SCSS-Lint.

    Needs SCSS-Lint 0.43.2 or newer.

    See URL `https://github.com/brigade/scss-lint'."
      :command ("scss-lint"
                "--require=scss_lint_reporter_checkstyle"
                "--format=Checkstyle"
                (config-file "--config" flycheck-scss-lintrc)
                "--stdin-file-path" source-original "-")
      :standard-input t
      ;; We cannot directly parse Checkstyle XML, since for some mysterious reason
      ;; SCSS-Lint doesn't have a built-in Checkstyle reporter, and instead ships it
      ;; as an addon which might not be installed.  We use a custom error parser to
      ;; check whether the addon is missing and turn that into a special kind of
      ;; Flycheck error.
      :error-parser flycheck-parse-scss-lint
      :modes scss-mode
      :verify (lambda (checker)
                (let* ((executable (flycheck-find-checker-executable checker))
                       (reporter-missing
                        (and executable
                             (with-temp-buffer
                               (call-process executable nil t nil
                                             "--require=scss_lint_reporter_checkstyle")
                               (goto-char (point-min))
                               (re-search-forward
                                flycheck-scss-lint-checkstyle-re
                                nil 'no-error)))))
                  (when executable
                    (list
                     (flycheck-verification-result-new
                      :label "checkstyle reporter"
                      :message (if reporter-missing
                                   "scss_lint_reporter_checkstyle missing"
                                 "present")
                      :face (if reporter-missing
                                '(bold error)
                              'success)))))))

    (flycheck-define-checker scss-stylelint
      "A SCSS syntax and style checker using stylelint.

    See URL `http://stylelint.io/'."
      :command ("stylelint"
                (eval flycheck-stylelint-args)
                "--syntax" "scss"
                (option-flag "--quiet" flycheck-stylelint-quiet)
                (config-file "--config" flycheck-stylelintrc))
      :standard-input t
      :error-parser flycheck-parse-stylelint
      :modes (scss-mode))

    (flycheck-def-option-var flycheck-scss-compass nil scss
      "Whether to enable the Compass CSS framework.

    When non-nil, enable the Compass CSS framework, via `--compass'."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "0.16"))

    (flycheck-define-checker scss
      "A SCSS syntax checker using the SCSS compiler.

    See URL `http://sass-lang.com'."
      :command ("scss"
                "--cache-location" (eval (flycheck-sass-scss-cache-location))
                (option-flag "--compass" flycheck-scss-compass)
                "--check" "--stdin")
      :standard-input t
      :error-patterns
      ((error line-start
              (or "Syntax error: " "Error: ")
              (message (one-or-more not-newline)
                       (zero-or-more "\n"
                                     (one-or-more " ")
                                     (one-or-more not-newline)))
              (optional "\r") "\n" (one-or-more " ") "on line " line
              " of standard input"
              line-end)
       (warning line-start
                "WARNING: "
                (message (one-or-more not-newline)
                         (zero-or-more "\n"
                                       (one-or-more " ")
                                       (one-or-more not-newline)))
                (optional "\r") "\n" (one-or-more " ") "on line " line
                " of an unknown file"
                line-end))
      :modes scss-mode)

    (flycheck-define-checker sh-bash
      "A Bash syntax checker using the Bash shell.

    See URL `http://www.gnu.org/software/bash/'."
      :command ("bash" "--norc" "-n" "--")
      :standard-input t
      :error-patterns
      ((error line-start
              ;; The name/path of the bash executable
              (one-or-more (not (any ":"))) ":"
              ;; A label "line", possibly localized
              (one-or-more (not (any digit)))
              line (zero-or-more " ") ":" (zero-or-more " ")
              (message) line-end))
      :modes sh-mode
      :predicate (lambda () (eq sh-shell 'bash))
      :next-checkers ((warning . sh-shellcheck)))

    (flycheck-define-checker sh-posix-dash
      "A POSIX Shell syntax checker using the Dash shell.

    See URL `http://gondor.apana.org.au/~herbert/dash/'."
      :command ("dash" "-n")
      :standard-input t
      :error-patterns
      ((error line-start (one-or-more (not (any ":"))) ": " line ": " (message)))
      :modes sh-mode
      :predicate (lambda () (eq sh-shell 'sh))
      :next-checkers ((warning . sh-shellcheck)))

    (flycheck-define-checker sh-posix-bash
      "A POSIX Shell syntax checker using the Bash shell.

    See URL `http://www.gnu.org/software/bash/'."
      :command ("bash" "--posix" "--norc" "-n" "--")
      :standard-input t
      :error-patterns
      ((error line-start
              ;; The name/path of the bash executable
              (one-or-more (not (any ":"))) ":"
              ;; A label "line", possibly localized
              (one-or-more (not (any digit)))
              line (zero-or-more " ") ":" (zero-or-more " ")
              (message) line-end))
      :modes sh-mode
      :predicate (lambda () (eq sh-shell 'sh))
      :next-checkers ((warning . sh-shellcheck)))

    (flycheck-define-checker sh-zsh
      "A Zsh syntax checker using the Zsh shell.

    See URL `http://www.zsh.org/'."
      :command ("zsh" "--no-exec" "--no-globalrcs" "--no-rcs" source)
      :error-patterns
      ((error line-start (file-name) ":" line ": " (message) line-end))
      :modes sh-mode
      :predicate (lambda () (eq sh-shell 'zsh))
      :next-checkers ((warning . sh-shellcheck)))

    (defconst flycheck-shellcheck-supported-shells '(bash ksh88 sh)
      "Shells supported by ShellCheck.")

    (flycheck-def-option-var flycheck-shellcheck-excluded-warnings nil sh-shellcheck
      "A list of excluded warnings for ShellCheck.

    The value of this variable is a list of strings, where each
    string is a warning code to be excluded from ShellCheck reports.
    By default, no warnings are excluded."
      :type '(repeat :tag "Excluded warnings"
                     (string :tag "Warning code"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.21"))

    (flycheck-def-option-var flycheck-shellcheck-follow-sources t sh-shellcheck
      "Whether to follow external sourced files in scripts.

    Shellcheck will follow and parse sourced files so long as a
    pre-runtime resolvable path to the file is present.  This can
    either be part of the source command itself:
       source /full/path/to/file.txt
    or added as a shellcheck directive before the source command:
       # shellcheck source=/full/path/to/file.txt."
      :type 'boolean
      :safe #'booleanp
      :package-version '(flycheck . "31"))

    (flycheck-define-checker sh-shellcheck
      "A shell script syntax and style checker using Shellcheck.

    See URL `https://github.com/koalaman/shellcheck/'."
      :command ("shellcheck"
                "--format" "checkstyle"
                "--shell" (eval (symbol-name sh-shell))
                (option-flag "--external-sources" flycheck-shellcheck-follow-sources)
                (option "--exclude" flycheck-shellcheck-excluded-warnings list
                        flycheck-option-comma-separated-list)
                "-")
      :standard-input t
      :error-parser flycheck-parse-checkstyle
      :error-filter
      (lambda (errors)
        (flycheck-remove-error-file-names
         "-" (flycheck-dequalify-error-ids errors)))
      :modes sh-mode
      :predicate (lambda () (memq sh-shell flycheck-shellcheck-supported-shells))
      :verify (lambda (_)
                (let ((supports-shell (memq sh-shell
                                            flycheck-shellcheck-supported-shells)))
                  (list
                   (flycheck-verification-result-new
                    :label (format "Shell %s supported" sh-shell)
                    :message (if supports-shell "yes" "no")
                    :face (if supports-shell 'success '(bold warning)))))))

    (flycheck-define-checker slim
      "A Slim syntax checker using the Slim compiler.

    See URL `http://slim-lang.com'."
      :command ("slimrb" "--compile")
      :standard-input t
      :error-patterns
      ((error line-start
              "Slim::Parser::SyntaxError:" (message) (optional "\r") "\n  "
              "STDIN, Line " line (optional ", Column " column)
              line-end))
      :modes slim-mode
      :next-checkers ((warning . slim-lint)))

    (flycheck-define-checker slim-lint
      "A Slim linter.

    See URL `https://github.com/sds/slim-lint'."
      :command ("slim-lint" "--reporter=checkstyle" source)
      :error-parser flycheck-parse-checkstyle
      :modes slim-mode)

    (flycheck-define-checker sql-sqlint
      "A SQL syntax checker using the sqlint tool.

    See URL `https://github.com/purcell/sqlint'."
      :command ("sqlint")
      :standard-input t
      :error-patterns
      ((warning line-start "stdin:" line ":" column ":WARNING "
                (message (one-or-more not-newline)
                         (zero-or-more "\n"
                                       (one-or-more "  ")
                                       (one-or-more not-newline)))
                line-end)
       (error line-start "stdin:" line ":" column ":ERROR "
              (message (one-or-more not-newline)
                       (zero-or-more "\n"
                                     (one-or-more "  ")
                                     (one-or-more not-newline)))
              line-end))
      :modes (sql-mode))

    (flycheck-define-checker systemd-analyze
      "A systemd unit checker using systemd-analyze(1).

    See URL `https://www.freedesktop.org/software/systemd/man/systemd-analyze.html'."
      :command ("systemd-analyze" "verify" source)
      :error-patterns
      ((error line-start "[" (file-name) ":" line "] " (message) line-end))
      :modes (systemd-mode))

    (flycheck-def-config-file-var flycheck-chktexrc tex-chktex ".chktexrc"
      :safe #'stringp)

    (flycheck-define-checker tex-chktex
      "A TeX and LaTeX syntax and style checker using chktex.

    See URL `http://www.nongnu.org/chktex/'."
      :command ("chktex"
                (config-file "--localrc" flycheck-chktexrc)
                ;; Compact error messages, and no version information, and execute
                ;; \input statements
                "--verbosity=0" "--quiet" "--inputfiles")
      :standard-input t
      :error-patterns
      ((warning line-start "stdin:" line ":" column ":"
                (id (one-or-more digit)) ":" (message) line-end))
      :error-filter
      (lambda (errors)
        (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
      :modes (latex-mode plain-tex-mode))

    (flycheck-define-checker tex-lacheck
      "A LaTeX syntax and style checker using lacheck.

    See URL `http://www.ctan.org/pkg/lacheck'."
      :command ("lacheck" source-inplace)
      :error-patterns
      ((warning line-start
                "\"" (file-name) "\", line " line ": " (message)
                line-end))
      :modes latex-mode)

    (flycheck-define-checker texinfo
      "A Texinfo syntax checker using makeinfo.

    See URL `http://www.gnu.org/software/texinfo/'."
      :command ("makeinfo" "-o" null-device "-")
      :standard-input t
      :error-patterns
      ((warning line-start
                "-:" line (optional ":" column) ": " "warning: " (message)
                line-end)
       (error line-start
              "-:" line (optional ":" column) ": " (message)
              line-end))
      :modes texinfo-mode)

    (flycheck-def-config-file-var flycheck-typescript-tslint-config
        typescript-tslint "tslint.json"
      :safe #'stringp
      :package-version '(flycheck . "27"))

    (flycheck-def-option-var flycheck-typescript-tslint-rulesdir
        nil typescript-tslint
      "The directory of custom rules for TSLint.

    The value of this variable is either a string containing the path
    to a directory with custom rules, or nil, to not give any custom
    rules to TSLint.

    Refer to the TSLint manual at URL
    `http://palantir.github.io/tslint/usage/cli/'
    for more information about the custom directory."
      :type '(choice (const :tag "No custom rules directory" nil)
                     (directory :tag "Custom rules directory"))
      :safe #'stringp
      :package-version '(flycheck . "27"))

    (flycheck-def-args-var flycheck-tslint-args (typescript-tslint)
      :package-version '(flycheck . "31"))

    (flycheck-define-checker typescript-tslint
      "TypeScript style checker using TSLint.

    Note that this syntax checker is not used if
    `flycheck-typescript-tslint-config' is nil or refers to a
    non-existing file.

    See URL `https://github.com/palantir/tslint'."
      :command ("tslint" "--format" "json"
                (config-file "--config" flycheck-typescript-tslint-config)
                (option "--rules-dir" flycheck-typescript-tslint-rulesdir)
                (eval flycheck-tslint-args)
                source)
      :error-parser flycheck-parse-tslint
      :modes (typescript-mode))

    (flycheck-def-option-var flycheck-verilator-include-path nil verilog-verilator
      "A list of include directories for Verilator.

    The value of this variable is a list of strings, where each
    string is a directory to add to the include path of Verilator.
    Relative paths are relative to the file being checked."
      :type '(repeat (directory :tag "Include directory"))
      :safe #'flycheck-string-list-p
      :package-version '(flycheck . "0.24"))

    (flycheck-define-checker verilog-verilator
      "A Verilog syntax checker using the Verilator Verilog HDL simulator.

    See URL `https://www.veripool.org/wiki/verilator'."
      :command ("verilator" "--lint-only" "-Wall"
                (option-list "-I" flycheck-verilator-include-path concat)
                source)
      :error-patterns
      ((warning line-start "%Warning-" (zero-or-more not-newline) ": "
                (file-name) ":" line ": " (message) line-end)
       (error line-start "%Error: " (file-name) ":"
              line ": " (message) line-end))
      :modes verilog-mode)

    (flycheck-def-option-var flycheck-xml-xmlstarlet-xsd-path nil xml-xmlstarlet
      "An XSD schema to validate against."
      :type '(file :tag "XSD schema")
      :safe #'stringp
      :package-version '(flycheck . "31"))

    (flycheck-define-checker xml-xmlstarlet
      "A XML syntax checker and validator using the xmlstarlet utility.

    See URL `http://xmlstar.sourceforge.net/'."
      ;; Validate standard input with verbose error messages, and do not dump
      ;; contents to standard output
      :command ("xmlstarlet" "val" "--err" "--quiet"
                (option "--xsd" flycheck-xml-xmlstarlet-xsd-path)
                "-")
      :standard-input t
      :error-patterns
      ((error line-start "-:" line "." column ": " (message) line-end))
      :modes (xml-mode nxml-mode))

    (flycheck-def-option-var flycheck-xml-xmllint-xsd-path nil xml-xmllint
      "An XSD schema to validate against."
      :type '(file :tag "XSD schema")
      :safe #'stringp
      :package-version '(flycheck . "31"))

    (flycheck-define-checker xml-xmllint
      "A XML syntax checker and validator using the xmllint utility.

    The xmllint is part of libxml2, see URL
    `http://www.xmlsoft.org/'."
      :command ("xmllint" "--noout"
                (option "--schema" flycheck-xml-xmllint-xsd-path)
                "-")
      :standard-input t
      :error-patterns
      ((error line-start "-:" line ": " (message) line-end))
      :modes (xml-mode nxml-mode))

    (flycheck-define-checker yaml-jsyaml
      "A YAML syntax checker using JS-YAML.

    See URL `https://github.com/nodeca/js-yaml'."
      :command ("js-yaml")
      :standard-input t
      :error-patterns
      ((error line-start
              (or "JS-YAML" "YAMLException") ": "
              (message) " at line " line ", column " column ":"
              line-end))
      :modes yaml-mode)

    (flycheck-define-checker yaml-ruby
      "A YAML syntax checker using Ruby's YAML parser.

    This syntax checker uses the YAML parser from Ruby's standard
    library.

    See URL `http://www.ruby-doc.org/stdlib-2.0.0/libdoc/yaml/rdoc/YAML.html'."
      :command ("ruby" "-ryaml" "-e" "begin;
       YAML.load(STDIN); \
     rescue Exception => e; \
       STDERR.puts \"stdin:#{e}\"; \
     end")
      :standard-input t
      :error-patterns
      ((error line-start "stdin:" (zero-or-more not-newline) ":" (message)
              "at line " line " column " column line-end))
      :modes yaml-mode)

    (provide 'flycheck)

    ;; Local Variables:
    ;; coding: utf-8
    ;; indent-tabs-mode: nil
    ;; End:

    ;;; flycheck.el ends here

; -------------------------------- FLYCHECK RUST SETTINGS ---------------------------------- ;

    ;;; flycheck-rust.el --- Flycheck: Rust additions and Cargo support  -*- lexical-binding: t; -*-

    ;; Copyright (C) 2016, 2017  fmdkdd
    ;; Copyright (C) 2014, 2015  Sebastian Wiesner <swiesner@lunaryorn.com>

    ;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
    ;; URL: https://github.com/flycheck/flycheck-rust
    ;; Keywords: tools, convenience
    ;; Version: 0.1-cvs
    ;; Package-Requires: ((emacs "24.1") (flycheck "0.20") (dash "2.13.0") (seq "2.15") (let-alist "1.0.4"))

    ;; This file is not part of GNU Emacs.

    ;; This program is free software; you can redistribute it and/or modify
    ;; it under the terms of the GNU General Public License as published by
    ;; the Free Software Foundation, either version 3 of the License, or
    ;; (at your option) any later version.

    ;; This program is distributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ;; GNU General Public License for more details.

    ;; You should have received a copy of the GNU General Public License
    ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

    ;;; Commentary:

    ;; This Flycheck extension configures Flycheck automatically for the current
    ;; Cargo project.
    ;;
    ;; # Setup
    ;;
    ;;     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    ;;
    ;; # Usage
    ;;
    ;; Just use Flycheck as usual in your Rust/Cargo projects.

    ;;; Code:

    (eval-when-compile
      (require 'pcase)
      (require 'let-alist))

    (require 'dash)
    (require 'flycheck)
    (require 'seq)
    (require 'json)

    (defun flycheck-rust-find-manifest (file-name)
      "Get the Cargo.toml manifest for FILE-NAME.
    FILE-NAME is the path of a file in a cargo project given as a
    string.
    See http://doc.crates.io/guide.html for an introduction to the
    Cargo.toml manifest.
    Return the path to the Cargo.toml manifest file, or nil if the
    manifest could not be located."
      (-when-let (root-dir (locate-dominating-file file-name "Cargo.toml"))
        (expand-file-name "Cargo.toml" root-dir)))

    (defun flycheck-rust-dirs-list (start end)
      "Return a list of directories from START (inclusive) to END (exclusive).
    E.g., if START is '/a/b/c/d' and END is '/a', return the list
    '(/a/b/c/d /a/b/c /a/b) in this order.
    START and END are strings representing file paths.  END should be
    above START in the file hierarchy; if not, the list stops at the
    root of the file hierarchy."
      (let ((dirlist)
            (dir (expand-file-name start))
            (end (expand-file-name end)))
        (while (not (or (equal dir (car dirlist)) ; avoid infinite loop
                        (file-equal-p dir end)))
          (push dir dirlist)
          (setq dir (directory-file-name (file-name-directory dir))))
        (nreverse dirlist)))

    (defun flycheck-rust-get-cargo-targets (manifest)
      "Return the list of available Cargo targets for the given project.
    MANIFEST is the path to the Cargo.toml file of the project.
    Calls `cargo metadata --no-deps --manifest-path MANIFEST
    --format-version 1', parses and collects the targets for the
    current workspace, and returns them in a list, or nil if no
    targets could be found."
      (let ((cargo (funcall flycheck-executable-find "cargo")))
        (unless cargo
          (user-error "flycheck-rust cannot find `cargo'.  Please \
    make sure that cargo is installed and on your PATH.  See \
    http://www.flycheck.org/en/latest/user/troubleshooting.html for \
    more information on setting your PATH with Emacs."))
        ;; metadata contains a list of packages, and each package has a list of
        ;; targets.  We concatenate all targets, regardless of the package.
        (-when-let (packages (let-alist
                                 (with-temp-buffer
                                   (call-process cargo nil '(t nil) nil
                                                 "metadata" "--no-deps"
                                                 "--manifest-path" manifest
                                                 "--format-version" "1")
                                   (goto-char (point-min))
                                   (let ((json-array-type 'list))
                                     (json-read)))
                               .packages))
          (seq-mapcat (lambda (pkg)
                        (let-alist pkg .targets))
                      packages))))

    (defun flycheck-rust-find-cargo-target (file-name)
      "Return the Cargo build target associated with FILE-NAME.
    FILE-NAME is the path of the file that is matched against the
    `src_path' value in the list of `targets' returned by `cargo
    read-manifest'.
    Return a cons cell (KIND . NAME) where KIND is the target
    kind (lib, bin, test, example or bench), and NAME the target
    name (usually, the crate name).  If FILE-NAME exactly matches a
    target `src-path', this target is returned.  Otherwise, return
    the closest matching target, or nil if no targets could be found.
    See http://doc.crates.io/manifest.html#the-project-layout for a
    description of the conventional Cargo project layout."
      (-when-let* ((manifest (flycheck-rust-find-manifest file-name))
                   (targets (flycheck-rust-get-cargo-targets manifest)))
        (let ((target
               (or
                ;; If there is a target that matches the file-name exactly, pick
                ;; that one
                (seq-find (lambda (target)
                            (let-alist target (string= file-name .src_path)))
                          targets)
                ;; Otherwise find the closest matching target by walking up the tree
                ;; from FILE-NAME and looking for targets in each directory.  E.g.,
                ;; the file 'tests/common/a.rs' will look for a target in
                ;; 'tests/common', then in 'tests/', etc.
                (car (seq-find
                      (lambda (pair)
                        (-let [((&alist 'src_path target-path) . dir) pair]
                          (file-equal-p dir (file-name-directory target-path))))
                      ;; build a list of (target . dir) candidates
                      (-table-flat
                       'cons targets
                       (flycheck-rust-dirs-list file-name
                                                (file-name-directory manifest)))))
                ;; If all else fails, just pick the first target
                (car targets))))
          (let-alist target (cons (flycheck-rust-normalize-target-kind .kind) .name)))))

    (defun flycheck-rust-normalize-target-kind (kinds)
      "Return the normalized target name from KIND.
    KIND is a list of target name as returned by `cargo metadata',
    which do not necessarily correspond to to target names that can
    be passed as argument to `cargo rustc'.
    The normalization returns a valid cargo target based on KINDS."
      ;; Assumption: we only care about the first kind name.  Multiple kinds only
      ;; seem to happen for library crate types, and those all maps to the same
      ;; `lib' target.
      (pcase (car kinds)
        (`"dylib" "lib")
        (`"rlib" "lib")
        (`"staticlib" "lib")
        (`"cdylib" "lib")
        (`"proc-macro" "lib")
        (_ (car kinds))))

    ;;;###autoload
    (defun flycheck-rust-setup ()
      "Setup Rust in Flycheck.
    If the current file is part of a Cargo project, configure
    Flycheck according to the Cargo project layout."
      (interactive)
      ;; We should avoid raising any error in this function, as in combination
      ;; with `global-flycheck-mode' it will render Emacs unusable (see
      ;; https://github.com/flycheck/flycheck-rust/issues/40#issuecomment-253760883).
      (with-demoted-errors "Error in flycheck-rust-setup: %S"
        (-when-let* ((file-name (buffer-file-name))
                     ((kind . name) (flycheck-rust-find-cargo-target file-name)))
          (setq-local flycheck-rust-crate-type kind)
          (setq-local flycheck-rust-binary-name name))))

    (provide 'flycheck-rust)

; -------------------------------- CARGO PROCESS SETTINGS ---------------------------------- ;

    ;;; cargo-process.el --- Cargo Process Major Mode -*-lexical-binding: t-*-

    ;; Copyright (C) 2015  Kevin W. van Rooijen

    ;; Author: Kevin W. van Rooijen <kevin.van.rooijen@attichacker.com>
    ;; Keywords: processes, tools

    ;; This program is free software; you can redistribute it and/or modify
    ;; it under the terms of the GNU General Public License as published by
    ;; the Free Software Foundation, either version 3 of the License, or
    ;; (at your option) any later version.

    ;; This program is distributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ;; GNU General Public License for more details.

    ;; You should have received a copy of the GNU General Public License
    ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

    ;;; Commentary:

    ;; Cargo Process Major mode.
    ;; Used to run Cargo background processes.
    ;; Current supported Cargo functions:
    ;;  * cargo-process-bench              - Run the benchmarks.
    ;;  * cargo-process-build              - Compile the current project.
    ;;  * cargo-process-clean              - Remove the target directory.
    ;;  * cargo-process-doc                - Build this project's and its dependencies' documentation.
    ;;  * cargo-process-doc-open           - Open this project's documentation.
    ;;  * cargo-process-new                - Create a new cargo project.
    ;;  * cargo-process-init               - Create a new cargo project inside an existing directory.
    ;;  * cargo-process-run                - Build and execute src/main.rs.
    ;;  * cargo-process-run-example        - Build and execute with --example <name>.
    ;;  * cargo-process-run-bin            - Build and execute a specific binary.
    ;;  * cargo-process-search             - Search registry for crates.
    ;;  * cargo-process-test               - Run all unit tests.
    ;;  * cargo-process-update             - Update dependencies listed in Cargo.lock.
    ;;  * cargo-process-repeat             - Run the last cargo-process command.
    ;;  * cargo-process-current-test       - Run the current unit test.
    ;;  * cargo-process-current-file-tests - Run the current file unit tests.
    ;;  * cargo-process-fmt                - Run the optional cargo command fmt.
    ;;  * cargo-process-check              - Run the optional cargo command check.
    ;;  * cargo-process-clippy             - Run the optional cargo command clippy.

    ;;
    ;;; Code:

    (require 'compile)
    (require 'button)
    (require 'rust-mode)

    (defgroup cargo-process nil
      "Cargo Process group."
      :prefix "cargo-process-"
      :group 'cargo)

    (defcustom cargo-process--custom-path-to-bin nil
      "Custom path to the directory containing the cargo executable"
      :type 'directory
      :group 'cargo-process)

    (defcustom cargo-process--enable-rust-backtrace nil
      "Set RUST_BACKTRACE environment variable to 1 for tasks test and run"
      :group 'cargo-process)

    (defvar cargo-process-mode-map
      (nconc (make-sparse-keymap) compilation-mode-map)
      "Keymap for Cargo major mode.")

    (defvar cargo-process-last-command nil "Command used last for repeating.")

    (defvar cargo-process--command-bench "cargo bench")

    (defvar cargo-process--command-build "cargo build")

    (defvar cargo-process--command-clean "cargo clean")

    (defvar cargo-process--command-doc "cargo doc")

    (defvar cargo-process--command-doc-open "cargo doc --open")

    (defvar cargo-process--command-new "cargo new")

    (defvar cargo-process--command-init "cargo init")

    (defvar cargo-process--command-run "cargo run")

    (defvar cargo-process--command-run-bin "cargo run --bin")

    (defvar cargo-process--command-run-example "cargo run --example")

    (defvar cargo-process--command-search "cargo search")

    (defvar cargo-process--command-test "cargo test")

    (defvar cargo-process--command-current-test "cargo test")

    (defvar cargo-process--command-current-file-tests "cargo test")

    (defvar cargo-process--command-update "cargo update")

    (defvar cargo-process--command-fmt "cargo fmt")

    (defvar cargo-process--command-check "cargo check")

    (defvar cargo-process--command-clippy "cargo clippy")

    (defface cargo-process--ok-face
      '((t (:foreground "#00ff00")))
      "Ok face"
      :group 'cargo-process)

    (defface cargo-process--error-face
      '((t (:foreground "#FF0000")))
      "Error face"
      :group 'cargo-process)

    (defface cargo-process--warning-face
      '((t (:foreground "#eeee00")))
      "Warning face"
      :group 'cargo-process)

    (defface cargo-process--pointer-face
      '((t (:foreground "#ff00ff")))
      "Pointer face"
      :group 'cargo-process)

    (defface cargo-process--standard-face
      '((t (:foreground "#ffa500")))
      "Standard face"
      :group 'cargo-process)

    (defface cargo-process--errno-face
      '((t :foreground "#7777ff"
           :underline t))
      "Error number face"
      :group 'cargo-process)

    (defconst cargo-process--rust-backtrace "RUST_BACKTRACE")

    (defconst cargo-process-test-regexp "^[[:space:]]*fn[[:space:]]*"
      "Regex to find Rust unit test function.")

    (defconst cargo-process-test-mod-regexp "^[[:space:]]*mod[[:space:]]*\\w+[[:space:]]*{")

    (defconst cargo-process-font-lock-keywords
      '(("^error\\:?" . 'cargo-process--error-face)
        ("^warning\\:?" . 'cargo-process--warning-face)
        ("^\s*\\^\\~*\s*$" . 'cargo-process--pointer-face)
        ("^\s*Compiling.*" . 'cargo-process--standard-face)
        ("^\s*Running.*" . 'cargo-process--standard-face)
        ("^\s*Updating.*" . 'cargo-process--standard-face)
        ("test result: FAILED." . 'cargo-process--error-face)
        ("test result: ok." . 'cargo-process--ok-face)
        ("test\s.*\sFAILED" . 'cargo-process--error-face)
        ("test\s.*\sok" . 'cargo-process--ok-face))
      "Minimal highlighting expressions for cargo-process mode.")

    ;; Bind `case-fold-search' to nil before using the regex.
    (defconst cargo-process--errno-regex "\\bE[0-9]\\{4\\}\\b"
      "A regular expression to match Rust error number.")

    (define-button-type 'rustc-errno
      'follow-link t
      'face 'cargo-process--errno-face
      'action #'cargo-process--explain-action)

    (defun cargo-process--defun-at-point-p ()
      (string-match cargo-process-test-regexp
                    (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position))))

    (defun cargo-process--project-root ()
      "Find the root of the current Cargo project."
      (let ((root (locate-dominating-file (or buffer-file-name default-directory) "Cargo.toml")))
        (and root (file-truename root))))

    (define-derived-mode cargo-process-mode compilation-mode "Cargo-Process."
      "Major mode for the Cargo process buffer."
      (use-local-map cargo-process-mode-map)
      (setq major-mode 'cargo-process-mode)
      (setq mode-name "Cargo-Process")
      (setq-local truncate-lines t)
      (run-hooks 'cargo-process-mode-hook)
      (add-hook 'compilation-filter-hook #'cargo-process--add-errno-buttons)
      (font-lock-add-keywords nil cargo-process-font-lock-keywords))

    (defun cargo-process--finished-sentinel (process event)
      "Execute after PROCESS return and EVENT is 'finished'."
      (compilation-sentinel process event)
      (when (equal event "finished\n")
        (message "Cargo Process finished.")))

    (defun cargo-process--activate-mode (buffer)
      "Execute commands BUFFER at process start."
      (with-current-buffer buffer
        (funcall 'cargo-process-mode)
        (setq-local window-point-insertion-type t)))

    (defun set-rust-backtrace (command)
      "Set RUST_BACKTRACE variable depending on the COMMAND used.
    Always set to nil if cargo-process--enable-rust-backtrace is nil"
      (when cargo-process--enable-rust-backtrace
        (if (string-match "cargo \\(test\\|run\\)" "cargo run")
            (setenv cargo-process--rust-backtrace "1")
          (setenv cargo-process--rust-backtrace nil))))

    (defun cargo-process--start (name command)
      "Start the Cargo process NAME with the cargo command COMMAND."
      (set-rust-backtrace command)
      (let* ((buffer (concat "*Cargo " name "*"))
             (path cargo-process--custom-path-to-bin)
             (path (and path (file-name-as-directory path)))
             (command (cargo-process--maybe-read-command (concat path command)))
             (project-root (cargo-process--project-root))
             (default-directory (or project-root default-directory)))
        (save-some-buffers (not compilation-ask-about-save)
                           (lambda ()
                             (and project-root
                                  buffer-file-name
                                  (string-prefix-p project-root (file-truename buffer-file-name)))))
        (setq cargo-process-last-command (list name command))
        (compilation-start command 'cargo-process-mode (lambda(_) buffer))
        (set-process-sentinel (get-buffer-process buffer) 'cargo-process--finished-sentinel)))

    (defun cargo-process--explain-action (button)
      "Action called when the user activates Rust errno BUTTON."
      (cargo-process--explain-help (button-label button)))

    (defun cargo-process--explain-help (errno)
      "Display a detailed explaination of ERRNO in the Help buffer."
      (help-setup-xref (list #'cargo-process--explain-help errno)
                       (called-interactively-p 'interactive))
      (save-excursion
        (with-help-window (help-buffer)
          (princ (shell-command-to-string
                  (concat "rustc --explain=" errno)))
          (with-current-buffer standard-output
            (buffer-string)))))

    (defun cargo-process--add-errno-buttons ()
      "Turn error numbers into clickable links in Cargo process output.
    Meant to be run as a `compilation-filter-hook'."
      (save-excursion
        (let ((start compilation-filter-start)
              (end (point))
              (case-fold-search nil))
         (goto-char start)
         (while (re-search-forward cargo-process--errno-regex end t)
           (make-button (match-beginning 0)
                        (match-end 0)
                        :type 'rustc-errno)))))

    (defun cargo-process--get-current-test ()
      "Return the current test."
      (save-excursion
        (unless (cargo-process--defun-at-point-p)
          (rust-beginning-of-defun))
        (beginning-of-line)
        (search-forward "fn ")
        (let* ((line (buffer-substring-no-properties (point)
                                                     (line-end-position)))
               (lines (split-string line "("))
               (function-name (car lines)))
          function-name)))

    (defun cargo-process--get-current-mod ()
      "Return the current mod."
      (save-excursion
        (when (search-backward-regexp cargo-process-test-mod-regexp nil t)
          (let* ((line (buffer-substring-no-properties (line-beginning-position)
                                                       (line-end-position)))
                 (line (string-trim-left line))
                 (lines (split-string line " "))
                 (mod (cadr lines)))
            mod))))

    (defun cargo-process--get-current-test-fullname ()
      (if (cargo-process--get-current-mod)
          (concat (cargo-process--get-current-mod)
                  "::"
                  (cargo-process--get-current-test))
        (cargo-process--get-current-test)))

    (defun cargo-process--maybe-read-command (default)
      "Prompt to modify the DEFAULT command when the prefix argument is present.
    Without the prefix argument, return DEFAULT immediately."
      (if current-prefix-arg
          (read-shell-command "Cargo command: " default)
        default))

    ;;;###autoload
    (defun cargo-process-bench ()
      "Run the Cargo bench command.
    With the prefix argument, modify the command's invocation.
    Cargo: Run the benchmarks."
      (interactive)
      (cargo-process--start "Bench" cargo-process--command-bench))

    ;;;###autoload
    (defun cargo-process-build ()
      "Run the Cargo build command.
    With the prefix argument, modify the command's invocation.
    Cargo: Compile the current project."
      (interactive)
      (cargo-process--start "Build" cargo-process--command-build))

    ;;;###autoload
    (defun cargo-process-clean ()
      "Run the Cargo clean command.
    With the prefix argument, modify the command's invocation.
    Cargo: Remove the target directory."
      (interactive)
      (cargo-process--start "Clean" cargo-process--command-clean))

    ;;;###autoload
    (defun cargo-process-doc ()
      "Run the Cargo doc command.
    With the prefix argument, modify the command's invocation.
    Cargo: Build this project's and its dependencies' documentation."
      (interactive)
      (cargo-process--start "Doc" cargo-process--command-doc))

    ;;;###autoload
    (defun cargo-process-doc-open ()
      "Run the Cargo doc command with the --open switch.
    With the prefix argument, modify the command's invocation.
    Cargo: Open this project's documentation."
      (interactive)
      (cargo-process--start "Doc" cargo-process--command-doc-open))

    ;;;###autoload
    (defun cargo-process-new (name &optional bin)
      "Run the Cargo new command.
    With the prefix argument, modify the command's invocation.
    NAME is the name of your application.
    If BIN is t then create a binary application, otherwise a library.
    Cargo: Create a new cargo project."
      (interactive "sProject name: ")
      (let ((bin (when (or bin
                           (y-or-n-p "Create Bin Project? "))
                   " --bin")))
        (cargo-process--start "New" (concat cargo-process--command-new
                                            " "
                                            name
                                            bin))))

    ;;;###autoload
    (defun cargo-process-init (directory &optional bin)
      "Run the Cargo init command.
    With the prefix argument, modify the command's invocation.
    DIRECTORY is the directory you want to create a cargo project in.
    If BIN is t then create a binary application, otherwise a library.
    Cargo: Create a new cargo project in current directory."
      (interactive
       (list (read-directory-name "Directory: " nil default-directory t)))
      (let ((bin (when (or bin (y-or-n-p "Create Bin Project? ")) " --bin")))
        (cargo-process--start "Init" (concat cargo-process--command-init
                                             " "
                                             directory
                                             bin))))

    ;;;###autoload
    (defun cargo-process-run ()
      "Run the Cargo run command.
    With the prefix argument, modify the command's invocation.
    Cargo: Build and execute src/main.rs."
      (interactive)
      (cargo-process--start "Run" cargo-process--command-run))

    ;;;###autoload
    (defun cargo-process-run-bin (command)
      "Run the Cargo run command --bin <name>.
    With the prefix argument, modify the command's invocation.
    Cargo: Build and execute a specific binary"
      (interactive "sBinary name: ")
      (cargo-process--start (concat "Run " command)
                            (concat cargo-process--command-run-bin " " command)))

    ;;;###autoload
    (defun cargo-process-run-example (command)
      "Run the Cargo run command --example <name>.
    With the prefix argument, modify the command's invocation.
    Cargo: Build and execute with --example <name>."
      (interactive "sExample name: ")
      (cargo-process--start (concat "Example " command)
                            (concat cargo-process--command-run-example " " command)))

    ;;;###autoload
    (defun cargo-process-search (search-term)
      "Run the Cargo search command.
    With the prefix argument, modify the command's invocation.
    SEARCH-TERM is used as the search term for the Cargo registry.
    Cargo: Search registry for crates."
      (interactive "sSearch: ")
      (cargo-process--start "Search"
                            (concat cargo-process--command-search " " search-term)))

    ;;;###autoload
    (defun cargo-process-test ()
      "Run the Cargo test command.
    With the prefix argument, modify the command's invocation.
    Cargo: Run the tests."
      (interactive)
      (cargo-process--start "Test" cargo-process--command-test))

    ;;;###autoload
    (defun cargo-process-current-test ()
      "Run the Cargo test command for the current test.
    With the prefix argument, modify the command's invocation.
    Cargo: Run the tests."
      (interactive)
      (cargo-process--start "Test"
                            (concat cargo-process--command-current-test
                                    " "
                                    (cargo-process--get-current-test-fullname))))

    ;;;###autoload
    (defun cargo-process-current-file-tests ()
      "Run the Cargo test command for the current file.
    With the prefix argument, modify the command's invocation.
    Cargo: Run the tests."
      (interactive)
      (cargo-process--start "Test" (concat cargo-process--command-current-file-tests
                                           " "
                                           (cargo-process--get-current-mod))))

    ;;;###autoload
    (defun cargo-process-update ()
      "Run the Cargo update command.
    With the prefix argument, modify the command's invocation.
    Cargo: Update dependencies listed in Cargo.lock."
      (interactive)
      (cargo-process--start "Update" cargo-process--command-update))

    ;;;###autoload
    (defun cargo-process-fmt ()
      "Run the Cargo fmt command.
    With the prefix argument, modify the command's invocation.
    Requires Cargo Fmt to be installed."
      (interactive)
      (cargo-process--start "Fmt" cargo-process--command-fmt))

    ;;;###autoload
    (defun cargo-process-check ()
      "Run the Cargo check command.
    With the prefix argument, modify the command's invocation.
    Cargo: Check compile the current project.
    Requires cargo-check to be installed."
      (interactive)
      (cargo-process--start "Check" cargo-process--command-check))

    ;;;###autoload
    (defun cargo-process-clippy ()
      "Run the Cargo clippy command.
    With the prefix argument, modify the command's invocation.
    Cargo: Clippy compile the current project.
    Requires Cargo clippy to be installed."
      (interactive)
      (cargo-process--start "Clippy" cargo-process--command-clippy))

    ;;;###autoload
    (defun cargo-process-repeat ()
      "Run the last cargo-process command."
      (interactive)
      (if cargo-process-last-command
          (apply 'cargo-process--start cargo-process-last-command)
        (message "No last Cargo command.")))

    ;(define-key cargo-process-mode-map (kbd "n") 'forward-button)
    ;(define-key cargo-process-mode-map (kbd "p") 'backward-button)

    (provide 'cargo-process)
    ;;; cargo-process.el ends here

; ------------------------------------ CARGO SETTINGS -------------------------------------- ;

    ;;; cargo.el --- Emacs Minor Mode for Cargo, Rust's Package Manager.

    ;; Copyright (C) 2015  Kevin W. van Rooijen

    ;; Author: Kevin W. van Rooijen <kevin.van.rooijen@attichacker.com>
    ;; Version  : 0.4.0
    ;; Keywords: tools
    ;; Package-Requires: ((emacs "24.3") (rust-mode "0.2.0"))

    ;; This program is free software; you can redistribute it and/or modify
    ;; it under the terms of the GNU General Public License as published by
    ;; the Free Software Foundation, either version 3 of the License, or
    ;; (at your option) any later version.

    ;; This program is distributed in the hope that it will be useful,
    ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
    ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    ;; GNU General Public License for more details.

    ;; You should have received a copy of the GNU General Public License
    ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

    ;;; Commentary:
    ;;
    ;; Cargo Minor mode.
    ;; Provides a number of key combinations and functions for managing Cargo.
    ;; Current supported Cargo Key Combinations:
    ;;  * C-c C-c C-e - cargo-process-bench
    ;;  * C-c C-c C-b - cargo-process-build
    ;;  * C-c C-c C-l - cargo-process-clean
    ;;  * C-c C-c C-d - cargo-process-doc
    ;;  * C-c C-c C-v - cargo-process-doc-open
    ;;  * C-c C-c C-n - cargo-process-new
    ;;  * C-c C-c C-i - cargo-process-init
    ;;  * C-c C-c C-r - cargo-process-run
    ;;  * C-c C-c C-x - cargo-process-run-example
    ;;  * C-c C-c C-s - cargo-process-search
    ;;  * C-c C-c C-t - cargo-process-test
    ;;  * C-c C-c C-u - cargo-process-update
    ;;  * C-c C-c C-c - cargo-process-repeat
    ;;  * C-c C-c C-f - cargo-process-current-test
    ;;  * C-c C-c C-o - cargo-process-current-file-tests
    ;;  * C-c C-c C-m - cargo-process-fmt
    ;;  * C-c C-c C-k - cargo-process-check
    ;;  * C-c C-c C-K - cargo-process-clippy

    ;;
    ;;; Code:

    (require 'cargo-process)

    (defgroup cargo nil
      "Cargo group."
      :prefix "cargo-"
      :group 'tools)

    (defvar cargo-minor-mode-map (make-keymap) "Cargo-mode keymap.")
    (defvar cargo-minor-mode nil)

    ;;;###autoload
    (define-minor-mode cargo-minor-mode
      "Cargo minor mode. Used to hold keybindings for cargo-mode"
      nil " cargo" cargo-minor-mode-map)

    (provide 'cargo)
    ;;; cargo.el ends here

; --------------------------------- GLOBAL USER SETTINGS ----------------------------------- ;

    ; Load .rust-mode file for Rust-specific settings
    (autoload 'rust-mode "rust-mode" nil t)

    ; Display project directory
    (setq current-project-home "~/Development/srtmtoimage/")
    (setq initial-buffer-choice current-project-home)

    ; Maximize window
    (toggle-frame-maximized)

    ; Auto-save last session
    ;(desktop-save-mode 1)

    ; Stop Emacs from losing undo information by
    ; setting very high limits for undo buffers
    (setq undo-limit 20000000)
    (setq undo-strong-limit 40000000)

    ; Determine the underlying operating system
    (setq felix-aquamacs (featurep 'aquamacs))
    (setq felix-linux (featurep 'x))
    (setq felix-win32 (not (or felix-aquamacs felix-linux)))

    ; Disable some settings
    (setq auto-save-default nil)
    (setq auto-save-interval 0)
    (setq auto-save-list-file-prefix nil)
    (setq auto-save-timeout 0)
    (setq auto-show-mode t)
    (setq delete-auto-save-files nil)
    (setq delete-old-versions (quote other))
    (setq imenu-auto-rescan t)
    (setq imenu-auto-rescan-maxout 500000)
    (setq kept-new-versions 5)
    (setq kept-old-versions 5)
    (setq make-backup-file-name-function (quote ignore))
    (setq make-backup-files nil)
    (setq mouse-wheel-follow-mouse nil)
    (setq mouse-wheel-progressive-speed nil)
    (setq mouse-wheel-scroll-amount (quote (15)))
    (setq version-control nil)

    ; Never split a window
    (setq split-window-preferred-function ())

    ; Startup windowing
    (setq next-line-add-newlines nil)
    (setq-default truncate-lines t)
    (setq truncate-partial-width-windows nil)
    (split-window-horizontally)

    ; Not sure what this does
    (require 'cc-mode)
    (require 'ido)
    (require 'compile)
    (ido-mode t)

    ; Setup file extensions and their appropriate modes
    (setq auto-mode-alist
          (append
           '(("\\.rs$"      . rust-mode)
             ("\\.txt$"     . indented-text-mode)
             ("\\.emacs$"   . emacs-lisp-mode)
             ("\\.gen$"     . gen-mode)
             ("\\.ms$"      . fundamental-mode)
             ("\\.m$"       . objc-mode)
             ("\\.mm$"      . objc-mode)
             ) auto-mode-alist))

; --------------------------------- OS-DEPENDENT SETTINGS ----------------------------------- ;

    ; Settings for Windows
    (when felix-win32
      (set-variable 'grep-command "findstr -s -n -i -l ")
    )

    ; Settings for Mac
    (when felix-aquamacs
      (cua-mode 0)
      (set-variable 'grep-command "grep -irHn ")
      (osx-key-mode 0)
      (tabbar-mode 0)
      (setq mac-command-modifier 'meta)
      (setq x-select-enable-clipboard t)
      (setq aquamacs-save-options-on-quit 0)
      (setq special-display-regexps nil)
      (setq special-display-buffer-names nil)
      (define-key function-key-map [return] [13])
      (setq mac-command-key-is-meta t)
      (scroll-bar-mode nil)
      (setq mac-pass-command-to-system nil)
      ; Turn off the bell on Mac OS X
      (setq ring-bell-function ())
    )

    ; Settings for Linux
    (when felix-linux
      (display-battery-mode 1)
      (set-variable 'grep-command "grep -irHn ")
    )

; --------------------------------- INTERACTION SETTINGS ------------------------------------ ;

    ; Disable shift-selection
    (setq shift-select-mode nil)
    ; Disable middle mouse button
    (global-unset-key [mouse-2])
    ; Disable autoloading other lisp files
    (setq enable-local-variables nil)
    ; Disable indenting on semicolon
    (setq c-hanging-semi&comma-criteria '((lambda () 'stop)))
    ; Smooth scroll: 3 lines
    (setq scroll-step 3)
    ; Display clock
    (display-time)
    ; Smooth scroll
    (setq scroll-step 3)

    ; Autocomplection settings
    (setq dabbrev-case-replace t)
    (setq dabbrev-case-fold-search t)
    (setq dabbrev-upcase-means-case-search t)
    (abbrev-mode 1)

    ; Prevent consecutive marks activating bloody `transient-mark-mode'.
    (defadvice set-mark-command (after no-bloody-t-m-m activate)
      (if transient-mark-mode (setq transient-mark-mode nil)))

    ; Prevent mouse commands activating bloody `transient-mark-mode'.
    (defadvice mouse-set-region-1 (after no-bloody-t-m-m activate)
      (if transient-mark-mode (setq transient-mark-mode nil)))

    ; Moves to the previous line containing nothing but whitespace.
    (defun previous-blank-line ()
      (interactive)
      (search-backward-regexp "^[ \t]*\n")
    )

    ; Moves to the next line containing nothing but whitespace.
    (defun next-blank-line ()
      (interactive)
      (forward-line)
      (search-forward-regexp "^[ \t]*\n")
      (forward-line -1)
    )

    ; Search and replace with y/n question
    (defun felix-replace-string (FromString ToString)
      (interactive "sReplace: \nsReplace: %s  With: ")
      (save-excursion
        (replace-string FromString ToString)
      )
    )

    ; Perform a replace-string in the current region.
    (defun felix-replace-in-region (old-word new-word)
      (interactive "sReplace: \nsReplace: %s  With: ")
      (save-excursion (save-restriction
                (narrow-to-region (mark) (point))
                (beginning-of-buffer)
                (replace-string old-word new-word)
                )
      )
    )

    ; Save the buffer after untabifying it.
    (defun untabify-and-save-buffer ()
      (interactive)
      (save-excursion
        (save-restriction
          (widen)
          (untabify (point-min) (point-max))))
      (save-buffer)
    )

    ; Performs copy-region-as-kill as an append.
    (defun append-as-kill ()
      (interactive)
      (append-next-kill)
      (copy-region-as-kill (mark) (point))
    )

; ------------------------------------ BUILD SETTINGS --------------------------------------- ;

    ; Set build command command
    ; TODO: If it is a library, use cargo build, if its a binary, use cargo run
    (setq compile-command "cargo build")
    ; Unlock build directory (TODO: shouldn't cargo do this)
    (setq compilation-directory-locked nil)

    ; Setup compilation mode
    (defun felix-compilation-hook ()
      (make-local-variable 'truncate-lines)
      (setq truncate-lines nil)
    )

    (add-hook 'compilation-mode-hook 'felix-compilation-hook)

    ; Compilation
    (setq compilation-context-lines 0)
    (setq compilation-error-regexp-alist
        (cons '("^\\([0-9]+>\\)?\\(\\(?:[a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) : \\(?:fatal error\\|warnin\\(g\\)\\) C[0-9]+:" 2 3 nil (4))
         compilation-error-regexp-alist))

    ; Disable "hungry backspace"
    (c-toggle-auto-hungry-state -1)

    ; Recursively search for a Cargo.toml file
    (defun cargo-find-project-directory-recursive ()
      (interactive)
      (if (file-exists-p "Cargo.toml") t
          (cd "../")
          (cargo-find-project-directory-recursive)))

    ; Find the project directory
    (defun find-project-directory ()
      (interactive)
      (setq find-project-from-directory default-directory)
      (switch-to-buffer-other-window "*compilation*")
      (if compilation-directory-locked (cd last-compilation-directory)
      (cd find-project-from-directory)
      (cargo-find-project-directory-recursive)
      (setq last-compilation-directory default-directory))
    )

    ; Cargo run command, output to other window
    (defun cargo-run-without-asking ()
      (interactive)
      (if (find-project-directory) 'compile)
      (other-window 1)
    )

    ; Opens a file, but from the default-directory
    (defun find-file-with-custom-start-directory()
        (interactive)
        (cd default-directory)
        (call-interactively 'find-file)
    )

    ; Opens a file, but from the default-directory in the other window
    (defun find-file-other-window-with-custom-start-directory()
        (interactive)
        (cd default-directory)
        (call-interactively 'find-file-other-window)
    )

    (setq ido-default-buffer-method 'selected-window)

    (setq x-select-enable-clipboard t)
    (setq x-select-enable-primary t)

    ; Copy function
    (defun copy (beg end)
        "Copy region into clipboard"
        (interactive "r")
        (if mark-active
            (progn
                (x-set-selection 'CLIPBOARD (buffer-substring beg end))
                (setq mark-active nil)
                (message "Marked text copied")
                )
            (progn
                (x-set-selection 'CLIPBOARD (buffer-substring line-beginning-position line-end-position))
                (setq mark-active nil)
                (message "Current line is copied")
                )
            )
        )

    ; Cut function
    (defun cut (beg end)
        "Copy region into clipboard and kill"
        (interactive "r")
        (if mark-active
            (progn
                (x-set-selection 'CLIPBOARD (buffer-substring beg end))
                (delete-region beg end)
                (setq mark-active nil)
                (message "Marked text cut"))
            (progn
                (x-set-selection 'CLIPBOARD (buffer-substring line-beginning-position line-end-position))
                (delete-region line-beginning-position line-end-position)
                (setq mark-active nil)
                (message "Current line is cut")
            )
        )
    )

    ; Paste function
    (defun paste (beg end)
        "Paste contents of clipboard into point"
        (interactive "r")
        (if mark-active
            (delete-region beg end))
        (insert (x-selection 'CLIPBOARD))
    )

    (setq interprogram-paste-function 'paste)
    (setq interprogram-copy-function 'copy)
    (setq interprogram-cut-function 'cut)

; ---------------------------------- SHORTCUT SETTINGS -------------------------------------- ;

    ; CTRL + O            Open file in current active buffer
    (global-set-key (kbd "C-o") 'find-file)
    ; CTRL + SHIFT + O    Open file in other buffer
    (global-set-key (kbd "C-S-o") 'find-file-other-window)
    ; CTRL + U            Load buffer in current window
    (global-set-key (kbd "C-u") 'ido-switch-buffer)
    ; CTRL + T            Load buffer in other window
    (global-set-key (kbd "C-t") 'ido-switch-buffer-other-window)
    ; CTRL + D            Switch to other window (override delete in front)
    (global-set-key (kbd "C-d") 'other-window)
    ; CTRL + M            Newline and indent
    (global-set-key (kbd "C-m") 'newline-and-indent)
    ; CTRL + S             Save buffer
    (global-set-key (kbd "C-s") 'untabify-and-save-buffer)
    ; TAB                 Autocomplete
    ;(global-set-key (kbd "TAB") 'dabbrev-expand)
    ; CTRL + J            Search + replace
    (global-set-key (kbd "C-j") 'imenu)
    ; CTRL + SPACE        Set mark
    (global-set-key (read-kbd-macro "\C- ") 'set-mark-command)
    ; CTRL + SHIFT + P    Previous blank line
    (global-set-key (kbd "C-S-p") 'previous-blank-line)
    ; CTRL + SHIFT + N    Next blank line
    (global-set-key (kbd "C-S-n") 'next-blank-line)
    ; CTRL + TAB          Indent region
    (define-key global-map [C-tab] 'indent-region)
    ; CTRL + Q            Copy region
    (global-set-key (kbd "C-q") 'copy)
    ; CTRL + X            Cut region
    (global-set-key (kbd "C-x") 'cut)
    ; CTRL + V            Paste region
    (global-set-key (kbd "C-v") 'paste)
    ; CTRL + E            Go to end of line
    (global-set-key (kbd "C-e") 'end-of-line)
    ; CTRL + SHIFT + E    Go to end of buffer
    (global-set-key (kbd "C-S-e") 'end-of-buffer)
    ; CTRL + A            Go to beginning of line
    (global-set-key (kbd "C-a") 'beginning-of-line)
    ; CTRL + SHIFT + A    Go to beginning of buffer
    (global-set-key (kbd "C-S-a") 'beginning-of-buffer)


    ; ALT + Z             Kill region
    (global-set-key (kbd "M-Z") 'kill-region)
    ; ALT + BACKSPACE     Kill last word
    (define-key global-map "\377" 'backward-kill-word)
    ; ALT + DELETE        Kill next word
    (define-key global-map [M-delete] 'kill-word)
    ; ALT + R             Revert buffer
    (global-set-key (kbd "M-R") 'revert-buffer)
    ; ALT + K             Kill buffer
    (global-set-key (kbd "M-K") 'kill-this-buffer)
    ; ALT + P             Maximize current buffer
    (global-set-key (kbd "M-P") 'maximize-frame)
    ; ALT + W             Maximize other buffer
    (global-set-key (kbd "M-W") 'other-window)
    ; ALT + :             Jump view back to last mark
    (global-set-key (kbd "M-:") 'View-back-to-mark)
    ; ALT + ;             Exchange point and mark
    (global-set-key (kbd "M-;") 'exchange-point-and-mark)
    ; CTRL + G            Go to line
    (global-set-key (kbd "C-G") 'goto-line)
    ; CTRL + Z            Undo
    (global-set-key (kbd "C-Z") 'undo)
    ; ALT + 6             Uppercase words first letter
    (global-set-key (kbd "M-6") 'upcase-word)
    ; ALT + ^             Capitalize word
    (global-set-key (kbd "M-^") 'captilize-word)
    ; ALT + .             Fill paragraph
    (global-set-key (kbd "M-.") 'fill-paragraph)
    ; ALT + .             Replace in region (from last seen mark)
    (global-set-key (kbd "M-L") 'felix-replace-in-region)
    ; CTRL + o            Interactive replace
    ;(define-key global-map "\eo" 'query-replace)
    ; CTRL + SHIFT + o    Interactive replace
    ;(define-key global-map "\eO" 'felix-replace-string)
    ; ALT + [             Start keyboard macro
    (global-set-key (kbd "M-[") 'start-kbd-macro)
    ; ALT + ]             End keyboard macro
    (global-set-key (kbd "M-]") 'end-kbd-macro)
    ; ALT + #             Call last keyboard macro
    (global-set-key (kbd "M-#") 'call-last-kbd-macro)

    ; SHIFT + TAB         Actually tab
    (define-key global-map [S-tab] 'indent-for-tab-command)
    ; HOME                Go to begin of line
    (define-key global-map [home] 'beginning-of-line)
    ; END                 Go to end of line
    (define-key global-map [end] 'end-of-line)
    ; PGUP                Forward page
    (define-key global-map [pgup] 'forward-page)
    ; PGDOWM              Backward page
    (define-key global-map [pgdown] 'backward-page)

    ; F8                  Replace
    (define-key global-map [f8] 'felix-replace-string)
    ; F9                  Go to first error
    (define-key global-map [f9] 'first-error)
    ; F10                  Go to previous error
    (define-key global-map [f10] 'previous-error)
    ; F11                  Go to next error
    (define-key global-map [f11] 'next-error)

    ; --- Rust-specific keyboard shortcuts
    ; CTRL + C, CTRL + SHIFT + F           Format current buffer
    (define-key global-map (kbd "C-c C-S-f") 'rust-format-buffer)
    ; CTRL + C, CTRL + E                   cargo bench
    (define-key global-map (kbd "C-c C-e") 'cargo-process-bench)
    ; CTRL + C, CTRL + B                   cargo build
    (define-key global-map (kbd "C-c C-b") 'cargo-process-build)
    ; CTRL + C, CTRL + L                   cargo clean
    (define-key global-map (kbd "C-c C-l") 'cargo-process-clean)
    ; CTRL + C, CTRL + D                   cargo doc
    (define-key global-map (kbd "C-c C-d") 'cargo-process-doc)
    ; CTRL + C, CTRL + V                   cargo doc --open
    (define-key global-map (kbd "C-c C-v") 'cargo-process-doc-open)
    ; CTRL + C, CTRL + N                   cargo new
    (define-key global-map (kbd "C-c C-n") 'cargo-process-new)
    ; CTRL + C, CTRL + I                   cargo init
    (define-key global-map (kbd "C-c C-i") 'cargo-process-init)
    ; CTRL + C, CTRL + R                   cargo run (also CTRL + ALT + B)
    (define-key global-map (kbd "C-c C-r") 'cargo-process-run)
    ; CTRL + C, CTRL + X                   cargo run --example
    (define-key global-map (kbd "C-c C-x") 'cargo-process-run-example)
    ; CTRL + C, CTRL + S                   cargo search
    (define-key global-map (kbd "C-c C-s") 'cargo-process-search)
    ; CTRL + C, CTRL + T                   cargo test
    (define-key global-map (kbd "C-c C-t") 'cargo-process-test)
    ; CTRL + C, CTRL + U                   cargo update
    (define-key global-map (kbd "C-c C-u") 'cargo-process-update)
    ; CTRL + C, CTRL + C                   cargo repeat
    (define-key global-map (kbd "C-c C-c") 'cargo-process-repeat)
    ; CTRL + C, CTRL + F                   cargo test --current
    (define-key global-map (kbd "C-c C-f") 'cargo-process-current-test)
    ; CTRL + C, CTRL + O                   cargo test --current-file
    (define-key global-map (kbd "C-c C-o") 'cargo-process-current-file-tests)
    ; CTRL + C, CTRL + M                   cargo fmt
    (define-key global-map (kbd "C-c C-m") 'cargo-process-fmt)
    ; CTRL + C, CTRL + M                   cargo check (also CTRL + SHIFT + B)
    (define-key global-map (kbd "C-c C-k") 'cargo-process-check)
    ; CTRL + C, CTRL + SHIFT + K           cargo clippy
    (define-key global-map (kbd "C-c C-S-k") 'cargo-process-clippy)

    ; CTRL + SHIFT + B                     cargo check
    (global-set-key (kbd "C-S-B") 'cargo-process-check)
    ; CTRL + ALT + B                       cargo run
    (global-set-key (kbd "C-M-B") 'cargo-process-run)

    ; Debugging commands
    ;(global-set-key [f5] 'gud-cont)
    ;(global-set-key [f7] 'gud-tbreak)
    ;(global-set-key [S-f11] 'gud-finish)
    ;(global-set-key [f9] 'gud-break)
    ;(global-set-key [f10] 'gud-next)
    ;(global-set-key [f11] 'gud-step)

; ----------------------------------- VISUAL SETTINGS -------------------------------------- ;

    ; Highlight current line
    (global-hl-line-mode 1)
    ; Current line background color dark blue
    (set-face-background 'hl-line "#333333")
    ; Set cursor to line
    (setq-default cursor-type 'bar)
    (set-cursor-color "#ffffff")

    ; global foreground color white
    (set-foreground-color "#ffffff")
    ; global background color dark grey
    (set-background-color "#1a1a1a")

    ; Turn off the menu bar
    (menu-bar-mode -1)
    ; Turn off the toolbar
    (tool-bar-mode 0)
    ; Turn off the scroll bar
    (scroll-bar-mode -1)

    ; TODO and NOTE in red & dark red
    (setq fixme-modes '(rust-mode c++-mode c-mode emacs-lisp-mode))
    (make-face 'font-lock-fixme-face)
    (make-face 'font-lock-note-face)
    (mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
               ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
       fixme-modes)
     (modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
     (modify-face 'font-lock-note-face "Dark Red" nil nil t nil t nil nil)

    ; Syntax highlighting colors
    (add-to-list 'default-frame-alist '(font . "Monospace-13.5"))
    (set-face-attribute 'default t :font "Monospace-13.5")

    (set-face-attribute 'font-lock-builtin-face nil :foreground "#e74c3c")        ; tomato
    (set-face-attribute 'font-lock-comment-face nil :foreground "gray50")         ; grey
    (set-face-attribute 'font-lock-constant-face nil :foreground "#535bd4")       ; violet
    (set-face-attribute 'font-lock-doc-face nil :foreground "gray50")             ; grey
    (set-face-attribute 'font-lock-function-name-face nil :foreground "#3498db")  ; blue
    (set-face-attribute 'font-lock-keyword-face nil :foreground "#0ce067")        ; green
    (set-face-attribute 'font-lock-string-face nil :foreground "#fef51c")         ; yellow
    (set-face-attribute 'font-lock-type-face nil :foreground "#3498db")           ; blue
    (set-face-attribute 'font-lock-variable-name-face nil :foreground "#ffffff")  ; white

; --------------------------------- FORMATTING SETTINGS ------------------------------------ ;

    (setq tab-width 4)
    (setq indent-tabs-mode nil)

    (defun felix-ediff-setup-windows (buffer-A buffer-B buffer-C control-buffer)
      (ediff-setup-windows-plain buffer-A buffer-B buffer-C control-buffer)
    )

    (setq ediff-window-setup-function 'felix-ediff-setup-windows)
    (setq ediff-split-window-function 'split-window-horizontally)

    (add-hook 'c-mode-common-hook 'felix-big-fun-c-hook)
