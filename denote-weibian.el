;;; denote-weibian.el --- Extension of denote that integrates with Weibian (NOte Taking in TYpst) -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Hanwen Guo <guo@hanwen.io>
;; Maintainer: Hanwen Guo <guo@hanwen.io>
;; URL: https://github.com/hanwenguo/weibian
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (denote "4.0.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Optional extensions for Denote that work specifically with Weibian, a note taking system for Typst.

;;; Code:
(require 'denote)

(defgroup denote-weibian nil
  "Extensions that integrate Denote with Weibian."
  :group 'denote)

(defconst denote-weibian-transclusion-prompt-types
  '(show-metadata expanded disable-numbering demote-headings)
  "Supported prompt symbols for `denote-weibian-transclusion-prompts'.")

(defcustom denote-weibian-transclusion-prompts nil
  "Specify the prompts followed by `denote-weibian-transclude'.

The value of this user option is a list of symbols, which includes any
of the following:

- `show-metadata': Prompt for the `show-metadata' argument of `#tr'.

- `expanded': Prompt for the `expanded' argument of `#tr'.

- `disable-numbering': Prompt for the `disable-numbering' argument of
  `#tr'.

- `demote-headings': Prompt for the `demote-headings' argument of `#tr'.

The prompts occur in the given order.

If the value of this user option is nil, no transclusion option prompts
are used.

With a prefix argument, `denote-weibian-transclude' prompts for all
available transclusion options once."
  :group 'denote-weibian
  :type '(radio (const :tag "Use no prompts" nil)
                (set :tag "Available prompts" :greedy t
                     (const :tag "Show metadata" show-metadata)
                     (const :tag "Expanded" expanded)
                     (const :tag "Disable numbering" disable-numbering)
                     (const :tag "Demote headings" demote-headings))))

(defvar denote-weibian-front-matter
  "#import \"/_template/template.typ\": template, tr, ln, ct, inline-tree
#show: template(
  title:      %s,
  date:       %s,
  tags:       %s,
  identifier: %s,
  taxon:  %s,
)")
(defvar denote-weibian-title-key-regexp "^\\s-*title\\s-*:")
(defvar denote-weibian-keywords-key-regexp "^\\s-*tags\\s-*:")
(defvar denote-weibian-signature-key-regexp "^\\s-*taxon\\s-*:")
(defvar denote-weibian-identifier-key-regexp "^\\s-*identifier\\s-*:")
(defvar denote-weibian-date-key-regexp "^\\s-*date\\s-*:")

(defun denote-weibian--trim-trailing-comma (s)
  "Trim trailing comma from string S."
  (if (string-suffix-p "," s)
      (substring s 0 -1)
    s))

(defun denote-weibian--trim-brackets (s)
  "Trim brackets around string S."
  (let ((trims "[][]+"))
    (string-trim s trims trims)))

(defun denote-weibian-trim-whitespace-then-comma-then-quotes (s)
  "Trim whitespace then trailing comma then quotes from string S."
  (denote--trim-quotes (denote-trim-whitespace (denote-weibian--trim-trailing-comma (denote-trim-whitespace s)))))

(defun denote-weibian-trim-whitespace-then-comma-then-brackets (s)
  "Trim whitespace then trailing comma then quotes from string S."
  (denote-weibian--trim-brackets (denote-trim-whitespace (denote-weibian--trim-trailing-comma (denote-trim-whitespace s)))))

(defun denote-weibian-format-string-for-front-matter (s)
  "Surround string S with quotes.

This can be used in `denote-file-types' to format front mattter."
  (let ((completion-ignore-case nil))
    (format "\"%s\"" s)))

(defun denote-weibian-format-string-into-content-for-front-matter (s)
  "Surround string S with quotes.

This can be used in `denote-file-types' to format front mattter."
  (let ((completion-ignore-case nil))
    (format "[%s]" s)))

(defun denote-weibian-format-keywords-for-front-matter (keywords)
  "Format front matter KEYWORDS for Typst file type.
KEYWORDS is a list of strings.  Consult the `denote-file-types'
for how this is used."
  (format "(%s)" (mapconcat (lambda (k) (format "%S," k)) keywords " ")))

(defun denote-weibian-extract-keywords-from-front-matter (keywords-string)
  "Extract keywords list from front matter KEYWORDS-STRING.
Split KEYWORDS-STRING into a list of strings.

Consult the `denote-file-types' for how this is used."
  (split-string keywords-string "[:,\s]+" t "[][)( \"']+"))

(defun denote-weibian-format-date (date)
  "Format DATE as Typst datetime."
  (if date
      (format-time-string
       "datetime(year: %Y, month: %m, day: %d, hour: %H, minute: %M, second: %S)"
       date)
    ""))

(defmacro denote-weibian--define-retrieve-date (field)
  "Define a function to retrieve FIELD of a Typst datetime expression."
  (declare (indent 1))
  `(defun ,(intern (format "denote-weibian--retrieve-date-%s" field)) (date-string)
     (string-match ,(format "%s\\s-*:\\s-*\\([[:digit:]]+\\)\\s-*\\(,\\|)\\)" field)
                   date-string)
     (let ((matched (match-string 1 date-string)))
       matched)))

(denote-weibian--define-retrieve-date year)
(denote-weibian--define-retrieve-date month)
(denote-weibian--define-retrieve-date day)
(denote-weibian--define-retrieve-date hour)
(denote-weibian--define-retrieve-date minute)
(denote-weibian--define-retrieve-date second)

(defun denote-weibian--extract-date-from-front-matter (date-string)
  "Extract date object from front matter DATE-STRING."
  (let ((year (denote-weibian--retrieve-date-year date-string))
        (month (denote-weibian--retrieve-date-month date-string))
        (day (denote-weibian--retrieve-date-day date-string))
        (hour (denote-weibian--retrieve-date-hour date-string))
        (minute (denote-weibian--retrieve-date-minute date-string))
        (second (denote-weibian--retrieve-date-second date-string)))
    (if (and year month day hour minute second)
        (encode-time
         (string-to-number second)
         (string-to-number minute)
         (string-to-number hour)
         (string-to-number day)
         (string-to-number month)
         (string-to-number year)))))

(defun denote-weibian-extract-date-from-front-matter (date-string)
  "Extract date object from front matter DATE-STRING.

Consult the `denote-file-types' for how this is used."
  (let ((date-string (denote-weibian-trim-whitespace-then-comma-then-quotes date-string)))
    (if (string-empty-p date-string)
        nil
      (denote-weibian--extract-date-from-front-matter date-string))))

(defvar denote-weibian-link-format "#ln(\"wb:%s\")[%s]")
(defvar denote-weibian-link-in-context-regexp
  "#ln([[:blank:]]*\"wb:\\(?1:[^\"()]+?\\)\"[[:blank:]]*)\\[\\(?2:.*?\\)\\]")
(defvar denote-weibian-transclusion-format "#tr(\"wb:%s\"%s)")
(defvar denote-weibian-transclusion-in-context-regexp
  "#tr([[:blank:]]*\"wb:\\(?1:[^\"()]+?\\)\"")

(defvar denote-weibian-file-type
  `(weibian
    :extension ".typ"
    :front-matter denote-weibian-front-matter
    :link-retrieval-format "#ln(\"wb:%VALUE%\")"
    :link denote-weibian-link-format
    :link-in-context-regexp denote-weibian-link-in-context-regexp
    :title-key-regexp ,denote-weibian-title-key-regexp
    :title-value-function denote-weibian-format-string-into-content-for-front-matter
    :title-value-reverse-function denote-weibian-trim-whitespace-then-comma-then-brackets
    :keywords-key-regexp ,denote-weibian-keywords-key-regexp
    :keywords-value-function denote-weibian-format-keywords-for-front-matter
    :keywords-value-reverse-function denote-weibian-extract-keywords-from-front-matter
    :signature-key-regexp ,denote-weibian-signature-key-regexp
    :signature-value-function denote-weibian-format-string-for-front-matter
    :signature-value-reverse-function denote-weibian-trim-whitespace-then-comma-then-quotes
    :identifier-key-regexp ,denote-weibian-identifier-key-regexp
    :identifier-value-function denote-weibian-format-string-for-front-matter
    :identifier-value-reverse-function denote-weibian-trim-whitespace-then-comma-then-quotes
    :date-key-regexp ,denote-weibian-date-key-regexp
    :date-value-function denote-weibian-format-date
    :date-value-reverse-function denote-weibian-extract-date-from-front-matter))

(defun denote-weibian--transclusion-option-key (option)
  "Return plist key for transclusion OPTION."
  (intern (format ":%s" option)))

(defun denote-weibian--format-transclusion-boolean (value)
  "Format boolean VALUE for Typst transclusion options."
  (if value "true" "false"))

(defun denote-weibian--format-transclusion-integer (value)
  "Format non-negative integer VALUE for Typst transclusion options."
  (unless (natnump value)
    (user-error "Transclusion option `demote-headings' must be a non-negative integer"))
  (number-to-string value))

(defun denote-weibian--format-transclusion-option (option value)
  "Format Typst transclusion OPTION with VALUE."
  (pcase option
    ('show-metadata
     (format "show-metadata: %s"
             (denote-weibian--format-transclusion-boolean value)))
    ('expanded
     (format "expanded: %s"
             (denote-weibian--format-transclusion-boolean value)))
    ('disable-numbering
     (format "disable-numbering: %s"
             (denote-weibian--format-transclusion-boolean value)))
    ('demote-headings
     (format "demote-headings: %s"
             (denote-weibian--format-transclusion-integer value)))
    (_ (user-error "Unknown transclusion option `%s'" option))))

(defun denote-weibian-format-transclude (file &rest options)
  "Prepare transclusion to FILE with Typst OPTIONS.

OPTIONS is a plist whose recognized keys are `:show-metadata',
`:expanded', `:disable-numbering', and `:demote-headings'.  Omitted
keys are not written to the resulting `#tr' call."
  (let* ((identifier (denote-retrieve-filename-identifier file))
         (arguments '()))
    (unless identifier
      (user-error "The transcluded file does not have a Denote identifier"))
    (dolist (option denote-weibian-transclusion-prompt-types)
      (let ((key (denote-weibian--transclusion-option-key option)))
        (when (plist-member options key)
          (push (denote-weibian--format-transclusion-option
                 option
                 (plist-get options key))
                arguments))))
    (format denote-weibian-transclusion-format
            identifier
            (if arguments
                (concat ", " (mapconcat #'identity (nreverse arguments) ", "))
              ""))))

(defun denote-weibian--prompt-transclusion-boolean (prompt default)
  "Prompt for boolean transclusion option with PROMPT and DEFAULT."
  (let ((completion-ignore-case t))
    (string= (completing-read
              (format "%s (default %s): "
                      prompt
                      (denote-weibian--format-transclusion-boolean default))
              '("true" "false")
              nil t nil nil
              (denote-weibian--format-transclusion-boolean default))
             "true")))

(defun denote-weibian--prompt-transclusion-show-metadata ()
  "Prompt for the `show-metadata' transclusion option."
  (denote-weibian--prompt-transclusion-boolean "Show metadata" nil))

(defun denote-weibian--prompt-transclusion-expanded ()
  "Prompt for the `expanded' transclusion option."
  (denote-weibian--prompt-transclusion-boolean "Expanded" t))

(defun denote-weibian--prompt-transclusion-disable-numbering ()
  "Prompt for the `disable-numbering' transclusion option."
  (denote-weibian--prompt-transclusion-boolean "Disable numbering" nil))

(defun denote-weibian--prompt-transclusion-demote-headings ()
  "Prompt for the `demote-headings' transclusion option."
  (let ((value (read-number "Demote headings (default 1): " 1)))
    (unless (natnump value)
      (user-error "Transclusion option `demote-headings' must be a non-negative integer"))
    value))

(defun denote-weibian--prompt-transclusion-option (option)
  "Prompt for transclusion OPTION and return its value."
  (pcase option
    ('show-metadata
     (denote-weibian--prompt-transclusion-show-metadata))
    ('expanded
     (denote-weibian--prompt-transclusion-expanded))
    ('disable-numbering
     (denote-weibian--prompt-transclusion-disable-numbering))
    ('demote-headings
     (denote-weibian--prompt-transclusion-demote-headings))
    (_
     (user-error "Unknown transclusion prompt `%s'" option))))

(defun denote-weibian--prompt-transclusion-options (prompts)
  "Prompt for PROMPTS and return a plist of transclusion options."
  (let (options)
    (dolist (option prompts)
      (setq options
            (plist-put options
                       (denote-weibian--transclusion-option-key option)
                       (denote-weibian--prompt-transclusion-option option))))
    options))

;;;###autoload
(defun denote-weibian-transclude (file &rest options)
  "Insert transclusion to FILE using Typst OPTIONS.

When called interactively, prompt for FILE using completion.  Prompt for
transclusion options according to `denote-weibian-transclusion-prompts'.
With a prefix argument (\\[universal-argument]), prompt for all
available transclusion options once.

When called from Lisp, FILE is a string representing a full file system
path.  OPTIONS is a plist whose recognized keys are
`:show-metadata', `:expanded', `:disable-numbering', and
`:demote-headings'.  Omit a key from OPTIONS to use the default value
defined by the Typst `#tr' template."
  (interactive
   (let* ((file (denote-file-prompt nil "Transclude FILE" nil :has-identifier))
          (prompts (if current-prefix-arg
                       denote-weibian-transclusion-prompt-types
                     denote-weibian-transclusion-prompts))
          (options (denote-weibian--prompt-transclusion-options prompts)))
     (cons file options)))
  (unless (or (denote--file-type-org-extra-p)
              (and buffer-file-name (denote-file-has-supported-extension-p buffer-file-name)))
    (user-error "The current file type is not recognized by Denote"))
  (unless (file-exists-p file)
    (user-error "The transcluded file does not exist"))
  (denote--delete-active-region-content)
  (insert (apply #'denote-weibian-format-transclude file options)))

(defun denote-weibian-contexts-query-regexp (id)
  "Return a regexp to query contexts of file with ID."
  (rx
   line-start
   (zero-or-more blank)
   "#tr("
   (zero-or-more blank)
   "\"wb:"
   (literal id)
   "\""))

(defun denote-weibian--get-all-mention (files regexp)
  "Return hash table of all mention in FILES by identifier using REGEXP."
  (let* ((links-hash-table (make-hash-table :test 'equal))
         (files-by-file-type (denote--get-files-by-file-type files))
         (files (gethash 'weibian files-by-file-type)))
    (dolist (file files)
      (let* ((file-identifiers
              (with-temp-buffer
                (insert-file-contents file)
                (denote-link--collect-identifiers regexp))))
        (dolist (file-identifier file-identifiers)
          (if-let* ((links (gethash file-identifier links-hash-table)))
              (puthash file-identifier (push file links) links-hash-table)
            (puthash file-identifier (list file) links-hash-table)))))
    links-hash-table))

(defun denote-weibian-retrieve-xref-alist-for-mention (identifier regexp)
  "Return xref alist of absolute file paths of matches of REGEXP for IDENTIFIER."
  (let* ((files (denote-directory-files))
         (xref-file-name-display 'abs)
         (xref-matches '()))
    (when-let* ((current-all-mention
                 (gethash identifier
                          (denote-weibian--get-all-mention files regexp)))
                (format-parts (split-string
                               (denote--link-retrieval-format 'weibian)
                               "%VALUE%")) ; Should give two parts
                (query-simple (concat
                               (regexp-quote (nth 0 format-parts))
                               (regexp-quote identifier)
                               (regexp-quote (nth 1 format-parts)))))
      (setq xref-matches
            (append xref-matches
                    (xref-matches-in-files query-simple current-all-mention)))
      (let ((data (xref--analyze xref-matches)))
        (if-let* ((sort denote-query-sorting)
                  (files-matched (mapcar #'car data))
                  (files-sorted (denote-sort-files files-matched sort)))
            (mapcar (lambda (x) (assoc x data)) files-sorted)
          data)))))

(defun denote-weibian--contexts-get-buffer-name (file id)
  "Format a buffer name for `denote-weibian-contexts'.
Use FILE to detect a suitable title with which to name the buffer.  Else
use the ID."
  (denote-format-buffer-name
   (if-let* ((type (denote-filetype-heuristics file))
             (title (denote-retrieve-front-matter-title-value file type)))
       (format "FILE contexts for %S" title)
     (format "FILE contexts for %s" id))
   :special-buffer))

;;;###autoload
(defun denote-weibian-contexts ()
  "Produce a buffer with contexts to the current note.

By contexts, one mean files transcluding the current note. Show the
names of files linking to the current file. Include the content of each
context if the user option `denote-weibian-contexts-show-content' is non-nil.

Place the buffer below the current window or wherever the user option
`denote-weibian-contexts-display-buffer-action' specifies."
  (interactive)
  (if-let* ((file buffer-file-name))
      (if-let* ((identifier (denote-retrieve-filename-identifier file)))
          (when-let* ((query (denote-weibian-contexts-query-regexp identifier)))
            (funcall denote-query-links-buffer-function
                     query nil
                     (denote-weibian--contexts-get-buffer-name file identifier)
                     denote-backlinks-display-buffer-action))
        (user-error "The current file does not have a Denote identifier"))
    (user-error "Buffer `%s' is not associated with a file" (current-buffer))))

(defalias 'denote-weibian-show-contexts-buffer 'denote-weibian-contexts
  "Alias for `denote-weibian-contexts' command.")

(defun denote-weibian-get-contexts (&optional file)
  "Return list of contexts in current or optional FILE.
Also see `denote-get-backlinks'."
  (when-let* ((current-file (or file (buffer-file-name)))
              (id (or (denote-retrieve-filename-identifier current-file)
                      (user-error "The file does not have a Denote identifier")))
              (_ (denote-file-is-in-denote-directory-p current-file))
              (xrefs (denote-weibian-retrieve-xref-alist-for-mention
                      id
                      denote-weibian-transclusion-in-context-regexp)))
    (mapcar #'car xrefs)))

(defun denote-weibian--file-has-contexts-p (file)
  "Return non-nil if FILE has contexts."
  (not (zerop (length (denote-weibian-get-contexts file)))))

;;;###autoload
(defun denote-weibian-find-context ()
  "Use minibuffer completion to visit context to current file.
Visit the file itself, not the location where the link is.  For a
context-sensitive operation, use `denote-weibian-find-context-with-location'.

Alo see `denote-find-link'."
  (declare (interactive-only t))
  (interactive)
  (when-let* ((current-file buffer-file-name)
              (_ (or (denote-retrieve-filename-identifier current-file)
                     (user-error "The current file does not have a Denote identifier")))
              (links (or (denote-weibian-get-contexts current-file)
                         (user-error "No contexts found")))
              (selected (denote-select-from-files-prompt links "Select among CONTEXTS")))
    (find-file selected)))

;;;###autoload
(defun denote-weibian-find-context-with-location ()
  "Like `denote-find-backlink' but jump to the exact location of the link."
  (declare (interactive-only t))
  (interactive)
  (when-let* ((current-file buffer-file-name)
              (id (or (denote-retrieve-filename-identifier current-file)
                      (user-error "The current file does not have a Denote identifier")))
              (query (denote-weibian-contexts-query-regexp id))
              (files (denote-directory-files nil :omit-current :text-only))
              (fetcher (lambda () (xref-matches-in-files query files))))
    (xref-show-definitions-completing-read fetcher nil)))

(define-obsolete-function-alias
  'denote-weibian-backlinks
  'denote-backlinks
  "0.2.0")

(define-obsolete-function-alias
  'denote-weibian-show-backlinks-buffer
  'denote-show-backlinks-buffer
  "0.2.0")

(define-obsolete-function-alias
  'denote-weibian-get-backlinks
  'denote-get-backlinks
  "0.2.0")

(define-obsolete-function-alias
  'denote-weibian-find-backlink
  'denote-find-backlink
  "0.2.0")

(define-obsolete-function-alias
  'denote-weibian-find-backlink-with-location
  'denote-find-backlink-with-location
  "0.2.0")

(provide 'denote-weibian)
;;; denote-weibian.el ends here
