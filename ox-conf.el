;;; ox-conf.el --- Confluence Back-End for Org Export Engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 zeit

;; Author: zeit <zzzeittt at gmail dot com>
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an org export backend that supports Confluence wiki storage format...
;;
;; Prerequisites:
;;   - Confluene page:
;;     - Nothing to prepare by hand: pressing ~f f~ on an org file that
;;       has no ~#+CONF_ID~ creates the page for you.
;;   - Org file:
;;     - Add ~#+CONF_ID~ and ~#+CONF_SPACE~ at the top of the .org file
;;       you want to export, or let ~f f~ create the page and write them
;;       for you.
;;   - Emacs init:
;;     - Install =ox-conf.el=...
;;     - Specify ~org-conf-url~ according to your host URL in your init file (e.g. =init.el=).
;;     - Specify ~org-conf-jira-chart-server~ according to your case.
;;       - Ref: https://confluence.atlassian.com/conf59/jira-chart-macro-792499235.html
;;     - Specify ~org-conf-jria-chart-server-id~ according to your case.
;;     - Optionally specify ~org-conf-root-parent-page~ (a page URL or
;;       id): when set, parent pages are picked from that page's
;;       descendants.  When it is left empty, the picker infers a scope
;;       from the current file's local directory (the common ancestor of
;;       nearby exported .org files, then the parent of the most-used
;;       parent page, then a title match) and starts there.  Menus are
;;       file-like: `..' goes to the parent page, direct children are
;;       entered, and `✓ 就用 …' settles on the current page.
;;       Frequently used parents are offered first.  Candidates show
;;       titles, never URLs; a URL can still be typed by hand.
;;     - Choose the transport with ~org-conf-backend~:
;;         - ~auto~ (default): use the =confluence= CLI when
;;           ~org-conf-cli-program~ is on ~exec-path~, otherwise the
;;           REST API;
;;         - ~rest~: always use the REST API (auth via ~auth-source~);
;;         - ~cli~: always use the =confluence= CLI, which needs a
;;           configured profile (see ~confluence profile list~).
;;       The CLI does not use Emacs credentials, and ~f r~ only clears
;;       the REST auth cache.
;;
;; Exporting:
;;   - Wake up org-export dispatch.
;;   - Press ~f f~ for exporting to the CONF_PAGE you provided.
;;   - If the buffer already has ~#+CONF_ID~, ~f f~ asks whether to upload
;;     to that page (showing its freshly fetched title).  Answering no
;;     runs the create-page flow instead, leaving the old page alone.
;;   - If the buffer has no ~#+CONF_ID~, ~f f~ asks for a space and a
;;     title, lets you settle on a parent page, creates the page, writes
;;     ~#+CONF_SPACE~/~#+CONF_PAGE~/~#+CONF_ID~ into the file's front
;;     matter (after the leading #+ keywords, never above a shebang or
;;     mode line), and then uploads the content in one go.
;;
;; Options:
;;   - ~f b~: "To buffer" org-conf-export-to-buffer
;;   - ~f p~: "Page info" org-conf-export-page-info
;;   - ~f i~: "Image info" org-conf-export-img-info
;;   - ~f f~: "To Confluence" org-conf-export-to-conf
;;   - ~f w~: "To Confluence (w/o POST)" org-conf-export-to-conf-without-post
;;   - ~f r~: "Forget the wrong password" (REST auth cache only)
;;            org-conf-forget-password
;;   - ~f l~: "Show debug log" org-conf-show-log
;;
;; Response:
;;   - Once exported successfully, yout will get ~PUT SUCCEED~ response.
;;
;; Debug log:
;;   - Every backend call appends a timestamped `✅'/`❌' line to
;;     ~org-conf-log-buffer-name~ (default =*ox-conf-log*=), with timing.
;;     CLI calls also log the exact =confluence --json ...= command; a
;;     failed call logs a copy-pasteable elisp form with the active backend.
;;   - Page bodies go to a temporary UTF-8 file.  If the call fails, the
;;     file is kept and its path is logged, so the command can be rerun.
;;   - =f l= opens the log; ~org-conf-log-enabled~ turns it off and
;;     ~org-conf-log-max-lines~ caps its size.  The buffer is a read-only
;;     =special-mode= (Evil normal state, =q= to quit) with ✅/❌
;;     highlighting.
;;
;; Note:
;;   Most of the codes in this file are adapted from =ox-html.el=.
;;   Part of the codes are inspired by =org-jira.el=.
;;
;; Change Log:
;;   - 2025-01-21: Initial commit.
;;   - 2025-02-13: Fix markup not exported as expected.
;;   - 2025-04-01: Ask user when attachment exists on server.
;;   - 2025-04-10: Fixed tags/toc format, added org-conf-inner-template.
;;                 Now can control tags/toc through ~#+OPTIONS: toc/tags~.
;;   - 2025-04-27: Introduced numbered headings, can be set through
;;                 ~#+OPTIONS: num:t~.
;;                 Excluded ~org-conf-toc-headline~.
;;   - 2025-05-12: Fixed wrongly parse latex-fragment element to 'mathblock'.
;;                 Fixed '<', etc char escaping issue in XML exporting.
;;                 Special block fall through to 'info-block'.
;;   - 2025-07-04: Added 'section' block and 'column' block.
;;   - 2025-07-07: Exported svg image as embedded html with <iframe>.
;;   - 2025-11-17: Deprecated 'example-block'.
;;   - 2025-11-27: Added 'footnote' support.
;;   - 2026-05-21: Added 'f o' dispatch action to open conf page.
;;   - 2026-10-10: 'f f' now creates a Confluence page when the buffer has
;;                 no #+CONF_ID: it asks for space/title/parent page,
;;                 POSTs the page, writes the #+CONF_* keywords back, and
;;                 then runs the normal export.  Parent pages are picked
;;                 from the descendants of ~org-conf-root-parent-page~,
;;                 ordered by how often they were chosen before.
;;   - 2026-10-10: Parent pages are offered by title rather than URL.
;;                 With no ~org-conf-root-parent-page~, walk the space's
;;                 tree one level at a time (top-level pages come from
;;                 /rest/api/space/{key}/content?depth=root).  The
;;                 CONF_* keywords now join the file's front matter after
;;                 #+TITLE: instead of being pushed above it.
;;   - 2026-10-24: Fixed parent-page listing: the space-content endpoint
;;                 groups results by content type, so `page.results' is
;;                 now unwrapped and paginated.  CONF_* stays below a
;;                 leading shebang / mode line.  Added the =confluence=
;;                 CLI backend behind ~org-conf-backend~ (auto/rest/cli).
;;   - 2026-10-27: The parent picker infers its scope from the local
;;                 directory (sibling CONF pages, the parent of the
;;                 most-used parent, then a title match) and walks direct
;;                 children with `..'/confirm, like file navigation.
;;                 Frequent parents come first; URLs only appear in the
;;                 manual action.
;;   - 2026-10-28: Added a background debug log (=*ox-conf-log*/, `f l'):
;;                 every backend call logs ✅/❌ with timing, CLI commands,
;;                 elisp reproduction forms, and keeps failed body files.
;;   - 2026-10-28: `f f' now asks before updating an existing page; saying
;;                 no runs the create-page flow.  The log buffer is a
;;                 read-only special-mode with Evil/font-lock support.


;;; Code:

(require 'ox)
(require 'request)
(require 'url-util)

;;; ======================================================================
(defcustom org-conf-url
  "http://localhost:8081/"
  "The address of the confluence host."
  :type 'string
  :group 'org-export-conf)

(defcustom org-conf-jira-chart-server
  "JAC"
  "The server name of your jira host."
  :type 'string
  :group 'org-export-conf)

(defcustom org-conf-jira-chart-server-id
  ""
  "The server id of your jira host."
  :type 'string
  :group 'org-export-conf)

(defcustom org-conf-default-space
  ""
  "Your Confluence space."
  :type 'string
  :group 'org-export-conf)

(defcustom org-conf-toc-headline
  "目录"
  "The headline name of TOC."
  :type 'string
  :group 'org-export-conf)

(defun org-conf--return-api-url (api)
  "Return the full URL for API.
API may be a host-relative path, which is joined to `org-conf-url', or
an absolute URL (Confluence often puts those in `_links.next'), which
passes through unchanged."
  (if (string-match-p "\\`https?://" api)
      api
    (concat (replace-regexp-in-string "/*$" "/" org-conf-url)
            (replace-regexp-in-string "^/*" "" api))))

(defcustom org-conf-log-level -1
  "Logging level for request.
One of `error'/`warn'/`info'/`verbose'/`debug'/`trace'/`blather'.
-1 means no logging."
  :type '(choice (integer :tag "No logging" -1)
                 (const :tag "Level error" error)
                 (const :tag "Level warn" warn)
                 (const :tag "Level info" info)
                 (const :tag "Level Verbose" verbose)
                 (const :tag "Level DEBUG" debug)
                 (const :tag "Level TRACE" trace)
                 (const :tag "Level BLATHER" blather)))

(defcustom org-conf-backend 'auto
  "Which Confluence backend to use.
`auto' prefers the confluence CLI when `org-conf-cli-program' is found
on `exec-path', and falls back to the REST API otherwise."
  :type '(choice (const :tag "Auto (CLI when installed)" auto)
                 (const :tag "REST API (request.el)" rest)
                 (const :tag "confluence CLI" cli))
  :group 'org-export-conf)

(defcustom org-conf-cli-program "confluence"
  "Executable used by the `cli' backend."
  :type 'string
  :group 'org-export-conf)

(defcustom org-conf-log-enabled t
  "When non-nil, ox-conf writes a background debug log."
  :type 'boolean
  :group 'org-export-conf)

(defcustom org-conf-log-buffer-name "*ox-conf-log*"
  "Buffer that receives ox-conf's background debug log."
  :type 'string
  :group 'org-export-conf)

(defcustom org-conf-log-max-lines 2000
  "Maximum number of lines kept in `org-conf-log-buffer-name'."
  :type 'integer
  :group 'org-export-conf)

(defconst org-conf--log-level-def
  '(;; debugging
    (blather . 60) (trace . 50) (debug . 40)
    ;; information
    (verbose . 30) (info . 20)
    ;; errors
    (warn . 10) (error . 0))
  "Named logging levels.")

(defun org-conf--return-id-from-url (_link)
  "Given a confluence page url, return the id."
  (let* ((_id (when (string-match "[0-9]+$" _link)
                (match-string 0 _link)))
         )
    _id
    ))

(defun org-conf-turn-on-debug ()
  "Turn on debug mode."
  (interactive)
  (setq org-conf-log-level 'blather))

(defun org-conf-turn-off-debug ()
  "Turn off debug mode."
  (interactive)
  (setq org-conf-log-level -1))

(defface org-conf-log-timestamp
  '((t :foreground "gray50"))
  "Face for the timestamp at the start of an ox-conf log line."
  :group 'org-export-conf)

(defface org-conf-log-success
  '((t :foreground "green"))
  "Face for successful ox-conf log lines."
  :group 'org-export-conf)

(defface org-conf-log-error
  '((t :foreground "red"))
  "Face for failed ox-conf log lines."
  :group 'org-export-conf)

(defface org-conf-log-command
  '((t :foreground "steel blue"))
  "Face for reproducible commands in the ox-conf log."
  :group 'org-export-conf)

(defvar org-conf-log-font-lock-keywords
  '(("^\\[[0-9:]+\\]" 0 'org-conf-log-timestamp)
    ("✅" 0 'org-conf-log-success)
    ("❌" 0 'org-conf-log-error)
    ("^\\s-+CMD\\b.*" 0 'org-conf-log-command)
    ("^\\s-+ELISP\\b.*" 0 'org-conf-log-command)
    ("^\\s-+STDERR:.*" 0 'org-conf-log-error))
  "Font-lock keywords for `org-conf-log-mode'.")

(define-derived-mode org-conf-log-mode special-mode "ox-conf-log"
  "Major mode for the ox-conf background log.
Read-only and tail-following like `messages-buffer-mode', with
success/error/command highlighting."
  (setq-local window-point-insertion-type t)
  (setq font-lock-defaults '(org-conf-log-font-lock-keywords t))
  (font-lock-mode 1))

(with-eval-after-load 'evil
  (evil-set-initial-state 'org-conf-log-mode 'normal))

(defun org-conf--log-buffer ()
  "Return the ox-conf log buffer, creating it if needed."
  (let ((buffer (get-buffer-create org-conf-log-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'org-conf-log-mode)
        (org-conf-log-mode)))
    buffer))

(defun org-conf-show-log ()
  "Display the ox-conf background log."
  (interactive)
  (let ((buffer (org-conf--log-buffer)))
    (with-current-buffer buffer
      (when (fboundp 'font-lock-ensure)
        (font-lock-ensure)))
    (pop-to-buffer buffer)))

(defun org-conf--log (format-string &rest args)
  "Append a timestamped line to the ox-conf log and echo it.
FORMAT-STRING and ARGS are passed to `format'.  Echoing is skipped in
batch mode; the buffer always receives the line."
  (when org-conf-log-enabled
    (let ((line (apply #'format format-string args)))
      (with-current-buffer (org-conf--log-buffer)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (format-time-string "[%H:%M:%S] ") line "\n")
          (let ((lines (count-lines (point-min) (point-max))))
            (when (> lines org-conf-log-max-lines)
              (goto-char (point-min))
              (forward-line (- lines org-conf-log-max-lines))
              (delete-region (point-min) (point))))))
      (unless noninteractive
        (message "%s" line)))))

(defun org-conf--op-name (op)
  "Return OP's dispatcher name without the leading colon."
  (substring (symbol-name op) 1))

(defun org-conf--format-log-arg (arg)
  "Format ARG for the log; long strings become <N chars>."
  (if (and (stringp arg) (> (length arg) 120))
      (format "<%d chars>" (length arg))
    (prin1-to-string arg)))

(defun org-conf--format-log-args (args)
  "Format ARGS for the log."
  (mapconcat #'org-conf--format-log-arg args " "))

(defun org-conf--elisp-command (op args)
  "Return a copy-pasteable elisp form for OP with ARGS."
  (format "(let ((org-conf-backend '%s)) (org-conf-backend-%s%s))"
          (org-conf--active-backend)
          (org-conf--op-name op)
          (if args
              (concat " " (org-conf--format-log-args args))
            "")))

(defun org-conf--cli-command-string (args)
  "Return a copy-pasteable confluence command for ARGS."
  (combine-and-quote-strings
   (cons (or (ignore-errors (org-conf--cli-program)) org-conf-cli-program)
         (cons "--json" args))))

(defun org-conf--log-call (op args thunk)
  "Run THUNK, logging OP with ARGS and its timing.
On failure, log the reproducible elisp form and re-signal."
  (let ((start (float-time))
        (description (if args
                         (format "%s %s" (org-conf--op-name op)
                                 (org-conf--format-log-args args))
                       (org-conf--op-name op))))
    (condition-case err
        (let ((result (funcall thunk)))
          (org-conf--log "✅ %s (%.2fs)" description (- (float-time) start))
          result)
      (error
       (org-conf--log "❌ %s (%.2fs)\n   ELISP (%s): %s\n   %s"
                      description (- (float-time) start)
                      (org-conf--active-backend)
                      (org-conf--elisp-command op args)
                      (error-message-string err))
       (signal (car err) (cdr err))))))

(define-skeleton ske-conf
  "Quick insert the required options."
  nil
  "#+CONF_SPACE: " (skeleton-read "Your conf space: " org-conf-default-space) \n
  "#+CONF_PAGE: " (setq v1 (skeleton-read "Your conf page url: ")) \n
  "#+CONF_ID: " (org-conf--return-id-from-url v1) \n
  )
;;; ======================================================================


;;; ======================================================================
;;;                            Templates                             
;;; ======================================================================
(defun org-conf-template (contents _info)
  "Return complete document string after CONF conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  contents)

(defun org-conf-inner-template (contents info)
  "Return body of document string after CONF conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when depth (org-conf-toc depth info)))
   ;; Document contents.
   contents
   ;; Footnotes section.
   (org-html-footnote-section info)
   ))

(defun org-conf-toc (depth info)
  "Generate TOC."
  (format
   "<h1>%s</h1>
<ac:structured-macro ac:name=\"toc\">
  <ac:parameter ac:name=\"outline\">true</ac:parameter>
  <ac:parameter ac:name=\"maxLevel\">%s</ac:parameter>
  <ac:parameter ac:name=\"exclude\">^%s$</ac:parameter>
</ac:structured-macro>\n\n"
   org-conf-toc-headline
   (if (eq depth t)
       7
     depth)
   org-conf-toc-headline
   )
  )

(defun org-conf--tags (tags)
  "Format TAGS into HTML."
  (if tags
    (format "\n<span class=\"tag\">%s</span>\n"
            (mapconcat
             (lambda (tag)
               (format
"<ac:structured-macro ac:name=\"status\">
  <ac:parameter ac:name=\"colour\">%s</ac:parameter>
  <ac:parameter ac:name=\"title\">%s</ac:parameter>
  <ac:parameter ac:name=\"subtle\">true</ac:parameter>
</ac:structured-macro>
"
                ;; color
                (cond ((string= "新增" tag) "Green")
                      ((string= "修改" tag) "Yello")
                      ((string= "删除" tag) "Red")
                      ((string= "复制" tag) "Blue")
                      (t "Grey")
                       ) ;; color
                tag ;; tag
                ))
             tags "&#xa0;"))
    ;; No tag, return empty.
    ""))

(defun org-conf-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
    CONTENTS holds the contents of the headline.  INFO is a plist
    holding contextual information."
  (let* ((level (+ (org-export-get-relative-level headline info)))
         (text (org-export-data (org-element-property :title headline) info))
         (numbers (org-export-get-headline-number headline info))
         (id (org-html--reference headline info))
         (tags (if (plist-get info :with-tags)
                   (org-conf--tags (org-export-get-tags headline info))
                 ""))
         (_head (format "\n<h%d>%s%s</h%d>\n"
                  level
                  text
                  tags
                  level))
         (sec-num (plist-get info :section-numbers))
         (_head (if sec-num
                    (format
"<ac:structured-macro ac:name=\"numberedheadings\">
  <ac:parameter ac:name=\"start-numbering-with\">%s</ac:parameter>
  <ac:rich-text-body>%s</ac:rich-text-body>
</ac:structured-macro>
"
                     (mapconcat #'number-to-string numbers ",")
                     _head)
                  ))
         (contents (or contents ""))
         )
    (format "%s%s\n"
            _head 
            contents)
    ))

(defun org-conf-section (section contents info)
  "transcode a section element from org to html.
  contents holds the contents of the section.  info is a plist
  holding contextual information."
  (let ((parent (org-export-get-parent-headline section)))
    ;; before first headline: no container, just return contents.
    (if (not parent) contents
      ;; get div's class and id references.
      (format "%s\n"
              (or contents "")))))

(defun org-conf-latex-fragment-filter (data backend info)
  "function applied to a transcoded latex-fragment.
  each filter is called with three arguments: the transcoded data,
  as a string, the back-end, as a symbol, and the communication
  channel, as a plist.  it must return a string or nil."
  (format
"\n<ac:structured-macro ac:name=\"mathinline\">
  <ac:parameter ac:name=\"body\">%s</ac:parameter>
</ac:structured-macro>\n"
  ;; Remove guiding '\(' and trailing '\)'.
  (replace-regexp-in-string "\\\\(\\|\\\\)" "" (org-html-encode-plain-text data))
))

(defun org-conf-latex-environment-filter (data backend info)
  "Function applied to a transcoded latex-environment."
  (format
"\n<ac:structured-macro ac:name=\"mathblock\">
  <ac:parameter ac:name=\"alignment\">center</ac:parameter>
  <ac:plain-text-body>
    <![CDATA[
%s
    ]]>
  </ac:plain-text-body>
</ac:structured-macro>\n"
  data))

(defun org-conf-export-block-filter (data backend info)
  "Transcode a EXPORT-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (format
"<ac:structured-macro ac:name=\"html\">
  <ac:plain-text-body>
    <![CDATA[%s]]>
  </ac:plain-text-body>
</ac:structured-macro>" data))

(defun org-conf-center-block (_center-block contents _info)
  "Transcode a CENTER-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (replace-regexp-in-string "<p>" "<p style=\"text-align: center;\">" contents))

(defun org-conf-example-block (example-block _contents info)
  "Transcode a EXAMPLE-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let* ((title (car (org-export-get-caption example-block)))
         (code-info (org-export-unravel-code example-block))
         (text (car code-info))
         )
    (if (not title) (setq title "[Example]"))
    ;; ==============================================
    ;; @deprecated: Example block cannot be exported correctly, we deprecated it since 2025-11-17.
    (display-warning 'org-export-conf "Example block cannot be exported correctly, we deprecated it since 2025-11-17! Contents of block will be empty if exported." :warning "*Confluence Export Warning*")
    ;; ==============================================
    (format
"<ac:structured-macro ac:name=\"panel\">
  <ac:parameter ac:name=\"borderColor\">grey</ac:parameter>
  <ac:parameter ac:name=\"borderStyle\">dashed</ac:parameter>
  <ac:parameter ac:name=\"title\">%s</ac:parameter>
  <ac:rich-text-body>
%s
  </ac:rich-text-body>
</ac:structured-macro>" title text)
    ))

;; Only by setting org-babel-exp-code-template will the
;; parameters be passed to exporter.
(setq org-babel-exp-code-template
      "#+begin_src %lang%switches%flags \
:host %host :path %path \
:limit %limit :columns %columns \
:chart-type %chart-type \
:showinfor %showinfor \
:stat-type %stat-type \
:xstat-type %xstat-type \
:ystat-type %ystat-type \
:collapse %collapse\n%body\n\
#+end_src")

;; Plain text
(defun org-conf-plain-text (text info)
  "Transcode a TEXT string from Org to HTML.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (let ((output text))
    ;; Protect following characters: <, >, &.
    (setq output (org-html-encode-plain-text output))
    ;; Handle smart quotes.  Be sure to provide original string since
    ;; OUTPUT may have been modified.
    (when (plist-get info :with-smart-quotes)
      (setq output (org-export-activate-smart-quotes output :html info text)))
    ;; Handle special strings.
    (when (plist-get info :with-special-strings)
      (setq output (org-html-convert-special-strings output)))
    ;; Handle break preservation if required.
    (when (plist-get info :preserve-breaks)
      (setq output
	    (replace-regexp-in-string
	     "\\(\\\\\\\\\\)?[ \t]*\n"
	     (concat (org-html-close-tag "br" nil info) "\n") output)))
    ;; Return value.
    output))

;; Special blocks
(add-to-list 'org-structure-template-alist '("w" . "warning"))
(unless (assoc "n" org-structure-template-alist)
  (add-to-list 'org-structure-template-alist '("n" . "note")))
(add-to-list 'org-structure-template-alist '("t" . "tip"))
(add-to-list 'org-structure-template-alist '("p" . "expand"))
(add-to-list 'org-structure-template-alist '("y" . "history"))
(add-to-list 'org-structure-template-alist '("i" . "info"))
(add-to-list 'org-structure-template-alist '("1" . "section"))
(add-to-list 'org-structure-template-alist '("2" . "column"))

(defun org-conf-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to HTML.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information."
  (let* (
         (block-type (org-element-property :type special-block))
         (pars (org-babel-parse-header-arguments (org-element-property :parameters special-block)))
         (title (car (org-export-get-caption special-block)))
         )
    (cond
     ;; Warning block
     ((string= "warning" block-type)
      (if (not title) (setq title "[Warning]"))
      (format
"<ac:structured-macro ac:name=\"warning\">
  <ac:parameter ac:name=\"title\">%s</ac:parameter>
  <ac:rich-text-body>
%s
  </ac:rich-text-body>
</ac:structured-macro>" title contents))
     ;; Note block
     ((string= "note" block-type)
      (if (not title) (setq title "[Note]"))
      (format
"<ac:structured-macro ac:name=\"note\">
  <ac:parameter ac:name=\"icon\">true</ac:parameter>
  <ac:parameter ac:name=\"title\">%s</ac:parameter>
  <ac:rich-text-body>
%s
  </ac:rich-text-body>
</ac:structured-macro>" title contents))
     ;; Tip block
     ((string= "tip" block-type)
      (if (not title) (setq title "[Tip]"))
      (format
"<ac:structured-macro ac:name=\"tip\">
  <ac:parameter ac:name=\"title\">%s</ac:parameter>
  <ac:rich-text-body>
%s
  </ac:rich-text-body>
</ac:structured-macro>" title contents))
     ;; Expand block
     ((string= "expand" block-type)
      (if (not title) (setq title "[Click to expand...]"))
      (format
"<ac:structured-macro ac:name=\"expand\">
  <ac:parameter ac:name=\"title\">%s</ac:parameter>
  <ac:rich-text-body>
%s
  </ac:rich-text-body>
</ac:structured-macro>" title contents))
     ;; Change history
     ((string= "history" block-type)
      (let ((limit (cdr (assoc :limit pars))))
        (if (not limit) (setq limit 3))
        (format
"<ac:structured-macro ac:name=\"change-history\">
  <ac:parameter ac:name=\"limit\">%s</ac:parameter>
</ac:structured-macro>" limit)))
     ;; Info block
     ((string= "info" block-type)
      (if (not title) (setq title "[Info]"))
      (format
"<ac:structured-macro ac:name=\"info\">
  <ac:parameter ac:name=\"title\">%s</ac:parameter>
  <ac:rich-text-body>
%s
  </ac:rich-text-body>
</ac:structured-macro>" title contents))
     ;; Section
     ((string= "section" block-type)
      (let* ((border (cdr (assoc :border pars)))
             )
        (if (not border)
            ;; border argumet is nil, default to "true"
            (setq border-conf "true")
          ;; User specified border argument
          (cond ((string= border "no")
                 (setq border-conf "false"))
                ((string= border "yes")
                 (setq border-conf "true"))
                (t
                 (error "You need to specify :border as 'yes' or 'no'!"))
                )
          )
        (format
"<ac:structured-macro ac:name=\"section\">
  <ac:parameter ac:name=\"border\">%s</ac:parameter>
  <ac:rich-text-body>
%s
  </ac:rich-text-body>
</ac:structured-macro>" border-conf contents)))
     ;; Column
     ((string= "column" block-type)
      (let* ((width (cdr (assoc :width pars)))
             (width-conf (if width
                             (format "  <ac:parameter ac:name=\"width\">%s</ac:parameter>" width)
                           ""
                           ))
             )
        (format
"<ac:structured-macro ac:name=\"column\">
%s
  <ac:rich-text-body>
%s
  </ac:rich-text-body>
</ac:structured-macro>" width-conf contents)))
     ;; Others
     (block-type contents) ;; Just return the original contents.
     )))

(defun org-conf-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* (
         (lang (or (org-element-property :language src-block) "none"))
         ;; Extract code and references.
         (code-info (org-export-unravel-code src-block))
         (code (car code-info))
         (num-start 1)
         (pars (org-babel-parse-header-arguments (org-element-property :parameters src-block)))
         (host (cdr (assoc :host pars)))
         (path (cdr (assoc :path pars)))
         (collapse (cdr (assoc :collapse pars)))
         (title (org-export-data (org-export-get-caption src-block) info))
         (env-comment "")
         )
    ;; Preprocess some parameters.
    (if (and host (string-match "^%" host))
        (setq host nil))
    (if (and path (string-match "^%" path))
        (setq path nil))
    (if (string= collapse "t")
        (setq collapse "true")
      (setq collapse "false"))
    (if title title
      (setq title ""))
    ;; Format src block.
    (cond
     ;; Jira issues.
     ((string= "jira" lang)
      (let ((limit (cdr (assoc :limit pars)))
            (columns (cdr (assoc :columns pars))))
        (if (not (numberp limit)) (setq limit 3))
        (if (string= "%columns" columns)
            (setq columns
                  (string-join '("type"
                                 "key"
                                 "summary"
                                 "priority"
                                 "status"
                                 "resolution"
                                 "fixversions"
                                 "created"
                                 "updated"
                                 "due"
                                 "assignee"
                                 "reporter"
                                 ) ";")))
        (format
"<ac:structured-macro ac:name=\"jira\">
  <ac:parameter ac:name=\"maximumIssues\">%s</ac:parameter>
  <ac:parameter ac:name=\"columns\">%s</ac:parameter>
  <ac:parameter ac:name=\"jqlQuery\">%s</ac:parameter>
</ac:structured-macro>" limit columns code)))
     ;; Jira Pie chart.
     ((string= "jirachart-pie" lang)
      (let ((showinfor (cdr (assoc :showinfor pars)))
            (stat-type (cdr (assoc :stat-type pars))))
        (if (or (string= "t" showinfor) (string= "%showinfor" showinfor)) (setq showinfor "true"))
        (if (string= "%stat-type" stat-type) (setq stat-type "statuses"))
        (format
"<ac:structured-macro ac:name=\"jirachart\">
  <ac:parameter ac:name=\"isAuthenticated\">true</ac:parameter>
  <ac:parameter ac:name=\"server\">%s</ac:parameter>
  <ac:parameter ac:name=\"serverId\">%s</ac:parameter>
  <ac:parameter ac:name=\"chartType\">pie</ac:parameter>
  <ac:parameter ac:name=\"showinfor\">%s</ac:parameter>
  <ac:parameter ac:name=\"statType\">%s</ac:parameter>
  <ac:parameter ac:name=\"jql\">%s</ac:parameter>
</ac:structured-macro>" org-conf-jira-chart-server org-conf-jira-chart-server-id showinfor stat-type code)))
     ;; Jira 2D chart.
     ((string= "jirachart-2d" lang)
      (let ((limit (cdr (assoc :limit pars)))
            (xstat-type (cdr (assoc :xstat-type pars)))
            (ystat-type (cdr (assoc :ystat-type pars))))
        (if (not (numberp limit)) (setq limit 5))
        (if (string= "%xstat-type" xstat-type) (setq xstat-type "statuses"))
        (if (string= "%ystat-type" ystat-type) (setq ystat-type "labels"))
        (format
"<ac:structured-macro ac:name=\"jirachart\">
  <ac:parameter ac:name=\"isAuthenticated\">true</ac:parameter>
  <ac:parameter ac:name=\"server\">%s</ac:parameter>
  <ac:parameter ac:name=\"serverId\">%s</ac:parameter>
  <ac:parameter ac:name=\"chartType\">twodimensional</ac:parameter>
  <ac:parameter ac:name=\"numberToShow\">%s</ac:parameter>
  <ac:parameter ac:name=\"xstattype\">%s</ac:parameter>
  <ac:parameter ac:name=\"ystattype\">%s</ac:parameter>
  <ac:parameter ac:name=\"jql\">%s</ac:parameter>
</ac:structured-macro>" org-conf-jira-chart-server org-conf-jira-chart-server-id limit xstat-type ystat-type code)))
     (code
      ;; Normal src blocks.
      (cond
       ((member lang '("bash"
                       "csharp"
                       "cpp"
                       "css"
                       "diff"
                       "html/xml"
                       "java"
                       "javascript"
                       "php"
                       "perl"
                       "powershell"
                       "python"
                       "ruby"
                       "scala"
                       "sql"))
        (setq lang lang))
       ((string= "asm" lang)
        (setq lang "c"))
       ((string= "rs" lang)
        (progn
          (setq lang "bash")
          (setq env-comment (format "%s:%s $ " host path))))
       (lang
        (setq lang "none"))
       )
      (format
"<ac:structured-macro ac:name=\"code\">
  <ac:parameter ac:name=\"theme\">Emacs</ac:parameter>
  <ac:parameter ac:name=\"linenumbers\">true</ac:parameter>
  <ac:parameter ac:name=\"language\">%s</ac:parameter>
  <ac:parameter ac:name=\"firstline\">%d</ac:parameter>
  <ac:parameter ac:name=\"collapse\">%s</ac:parameter>
  <ac:parameter ac:name=\"title\">%s</ac:parameter>
  <ac:plain-text-body>
    <![CDATA[
%s%s
    ]]>
  </ac:plain-text-body>
</ac:structured-macro>" lang num-start collapse title env-comment code))
     )))

(defun org-conf--embed-svg (source attributes info)
  "Embed SVG as Base64 iframe with proper closing tag.
Givne filename as SOURCE, org-inlined ATTRIBUTES and export INFO."
  (if (string= "svg" (file-name-extension source))
      ;; SVG handling (always Base64 iframe)
      (with-temp-buffer
        (insert-file-contents source)
        (let* ((base64-content (base64-encode-string (encode-coding-string (buffer-string) 'utf-8)))
               (width nil)
               (height nil)
               (aspect-ratio (when (string-match "viewBox=\"[^\"]*\"" (buffer-string))
                               (let ((viewbox (match-string 0 (buffer-string))))
                                 (when (string-match "\\bviewBox=\"\\([0-9.-]+\\) \\([0-9.-]+\\) \\([0-9.]+\\) \\([0-9.]+\\)\"" viewbox)
                                   (setq width (match-string 3 viewbox))
                                   (setq height (match-string 4 viewbox))
                                   (format "%s/%s" width height)))))
               (iframe-attrs (org-html--make-attribute-string
                              (org-combine-plists
                               attributes
                               `(
                                 :src ,(concat "data:image/svg+xml;base64," base64-content)
                                 :title ,(org-html--reference source info)
                                 :style ,(format "border:none; width:%s; height:auto; aspect-ratio:%s"
                                                 width
                                                 (or aspect-ratio "16/9")
                                                 ))))))
          (concat "<iframe " iframe-attrs "></iframe>")))

    ;; Non-SVG case - call original function
    (funcall fun source attributes info))
)

(defun org-conf-link (link desc info)
  "Link parser"
  (let* ((path (org-element-property :path link))
         ;; (attributes-plist (org-export-read-attribute :attr_html link))
         (type (org-element-property :type link))
           (file-title (org-export-data (plist-get info :title) info))
         (attributes-plist
          (org-combine-plists
           ;; Extract attributes from parent's paragraph.  HACK: Only
           ;; do this for the first link in parent (inner image link
           ;; for inline images).  This is needed as long as
           ;; attributes cannot be set on a per link basis.
           (let* ((parent (org-export-get-parent-element link))
                  (link (let ((container (org-export-get-parent link)))
                          (if (and (eq 'link (org-element-type container))
                                   (org-html-inline-image-p link info))
                              container
                            link))))
             (and (eq link (org-element-map parent 'link #'identity info t))
                  (org-export-read-attribute :attr_html parent)))
           ;; Also add attributes from link itself.  Currently, those
           ;; need to be added programmatically before `org-html-link'
           ;; is invoked, for example, by backends building upon HTML
           ;; export.
           (org-export-read-attribute :attr_html link)))
         )
    (cond
     ((member type '("fuzzy"))
      (let ((destination (org-export-resolve-fuzzy-link link info)))
        (pcase (org-element-type destination)
          ;; Link points to a headline.
          (`headline
           (let ((href (org-html--reference destination info))
                 ;; What description to use?
                 (desc
                  ;; Case 2: Either the headline is un-numbered or
                  ;; LINK has a custom description.  Display LINK's
                  ;; description or headline's title.
                  (or desc
                      (org-export-data
                       (org-element-property :title destination) info))))
             (format "<a href=\"#%s-%s\">%s</a>" file-title desc desc)))
          ;; Fuzzy link points to a target or an element.
          (_
           (let* ((ref (org-html--reference destination info)))
             (format "<a href=\"#%s-%s\">%s</a>" file-title ref desc))))))
     ;; SVG file.
     ((and (org-export-inline-image-p link (plist-get info :html-inline-image-rules))
           (string= "svg" (file-name-extension path)))
      (format
"<ac:structured-macro ac:name=\"html\">
  <ac:plain-text-body>
    <![CDATA[%s]]>
  </ac:plain-text-body>
</ac:structured-macro>" (org-conf--embed-svg path attributes-plist info))
      )
     ;; Image file.
     ((and (plist-get info :html-inline-images)
           (org-export-inline-image-p
            link (plist-get info :html-inline-image-rules)))
      (if (plist-get info :org-conf-do-post)
          (org-conf--upload-img
           (plist-get info :org-conf-id)
           path)
          ;; (message "%s" info)
        )
      (format
"<ac:image ac:width=\"%s\">
  <ri:attachment ri:filename=\"%s\" />
</ac:image>" (or (plist-get attributes-plist :width) "80%")
             (file-name-nondirectory path)))
     ;; External link with a description part.
     ((and path desc)
      (format "<a href=\"%s\">%s</a>"
              (org-html-encode-plain-text path)
              desc))
     ;; External link without a description part.
     (path
      (format "<a href=\"%s\">%s</a>"
              (org-html-encode-plain-text path)
              path))
     )))

(defun org-conf-target (target _contents info)
  "Transcode a TARGET object from Org to HTML.
CONTENTS is nil.  INFO is a plist holding contextual
information."
  (let ((ref (org-html--reference target info)))
    (org-html--anchor ref nil nil info)
    (format
"<ac:structured-macro ac:name=\"anchor\">
  <ac:parameter ac:name=\"\">%s</ac:parameter>
</ac:structured-macro>" ref)
    ))

(defun org-conf--anchor (id desc attributes info)
  "Format a HTML anchor."
  (let* ((name (and (plist-get info :html-allow-name-attribute-in-anchors) id))
         (attributes (concat (and id (format " id=\"%s\"" id))
                             (and name (format " name=\"%s\"" name))
                             attributes)))
    (format "<a%s>%s</a>" attributes (or desc ""))))

(defcustom org-conf-text-markup-alist
  '((bold . "<strong>%s</strong>")
    (code . "<code>%s</code>")
    (italic . "<em>%s</em>")
    (strike-through . "<span style=\"text-decoration: line-through;\">%s</span>")
    (underline . "<u>%s</u>")
    (verbatim . "<code>%s</code>"))
  "alist of html expressions to convert text markup.

    the key must be a symbol among `bold', `code', `italic',
    `strike-through', `underline' and `verbatim'.  the value is
    a formatting string to wrap fontified text with.

    if no association can be found for a given markup, text will be
    returned as-is."
  :type '(alist :key-type (symbol :tag "markup type")
                :value-type (string :tag "format string"))
  :options '(bold code italic strike-through underline verbatim))


;;; ======================================================================
;;;                          Create-page helpers
;;; ======================================================================

(defcustom org-conf-root-parent-page ""
  "Page whose descendants are offered as parent-page choices.
Either a full Confluence page URL or a bare page id.  When it is empty,
creating a page asks for a parent page URL instead of offering a list."
  :type 'string
  :group 'org-export-conf)

(defcustom org-conf-parent-freq-file
  (locate-user-emacs-file "ox-conf-parent-freq.eld")
  "File remembering how often each parent page was chosen.
Choices are offered most-used first."
  :type 'file
  :group 'org-export-conf)

(defcustom org-conf-parent-max-pages 500
  "Maximum number of pages fetched when listing parent-page choices.
Guards against a huge space making the prompt take forever."
  :type 'integer
  :group 'org-export-conf)

(defcustom org-conf-parent-child-page-limit 200
  "How many children to request per Confluence page while walking a tree."
  :type 'integer
  :group 'org-export-conf)

(defcustom org-conf-scope-search-depth 2
  "How many directory levels to climb when inferring the parent scope.
The scope is learned from sibling .org files that already carry
#+CONF_ID, so a file in a new subdirectory still finds its Confluence
subtree."
  :type 'integer
  :group 'org-export-conf)

(defcustom org-conf-recent-parent-limit 5
  "How many frequently used parent pages to offer first."
  :type 'integer
  :group 'org-export-conf)

(defun org-conf--page-path (node table)
  "Return NODE's ancestor chain as a \"Root / Mid / Leaf\" string.
NODE is a plist holding :id, :title and :parent-id.  TABLE maps page
ids to such plists.  The walk stops at a node with no parent, at an id
TABLE does not know about, or when a cycle is detected."
  (let ((titles nil)
        (cur node)
        (seen (make-hash-table :test 'equal)))
    (while (and cur (not (gethash (plist-get cur :id) seen)))
      (puthash (plist-get cur :id) t seen)
      (push (plist-get cur :title) titles)
      (setq cur (gethash (plist-get cur :parent-id) table)))
    (mapconcat #'identity titles " / ")))

(defun org-conf--sort-parent-candidates (candidates freq)
  "Sort CANDIDATES by descending usage in FREQ, then by :path.
CANDIDATES is a list of plists holding at least :id and :path.  FREQ
is an alist of (PAGE-ID . COUNT); ids missing from it count as zero.
The input list is not modified."
  (sort (copy-sequence candidates)
        (lambda (a b)
          (let ((fa (or (cdr (assoc (plist-get a :id) freq)) 0))
                (fb (or (cdr (assoc (plist-get b :id) freq)) 0)))
            (if (= fa fb)
                (string-lessp (or (plist-get a :path) "")
                              (or (plist-get b :path) ""))
              (> fa fb))))))

(defun org-conf--collect-descendants (root fetch-fn max-nodes)
  "Return ROOT plus every descendant reachable through FETCH-FN.
The walk is breadth-first, so near ancestors come before deep ones.
ROOT is a plist holding :id and :title.  FETCH-FN is called with a page
id and must return that page's direct children as plists holding :id
and :title; it may be slow, so it is only called while nodes remain
under the MAX-NODES budget.

Each node in the result gains a :parent-id and a :path (see
`org-conf--page-path').  Walking stops silently once MAX-NODES nodes
have been collected."
  (let* ((table (make-hash-table :test 'equal))
         (root-node (list :id (plist-get root :id)
                          :title (plist-get root :title)
                          :parent-id (plist-get root :parent-id)))
         (queue (list root-node))
         (result nil)
         (count 0))
    (puthash (plist-get root-node :id) root-node table)
    (while (and queue (< count max-nodes))
      (let* ((node (pop queue))
             (parent-id (plist-get node :id))
             (children (funcall fetch-fn parent-id)))
        (push (plist-put node :path (org-conf--page-path node table)) result)
        (setq count (1+ count))
        (dolist (child children)
          (let ((child-node (list :id (plist-get child :id)
                                  :title (plist-get child :title)
                                  :parent-id parent-id)))
            (puthash (plist-get child-node :id) child-node table)
            (setq queue (append queue (list child-node)))))))
    (nreverse result)))

(defconst org-conf--max-child-page-requests 20
  "Safety cap on pagination, so a broken `next' link cannot loop forever.")

(defun org-conf--child-page-api (page-id)
  "Return the API path listing PAGE-ID's direct children."
  (format "/rest/api/content/%s/child/page?limit=%s"
          page-id org-conf-parent-child-page-limit))

(defun org-conf--space-root-page-api (space)
  "Return the API path listing SPACE's top-level pages.
The `depth=root' parameter is what keeps this from walking the whole
space."
  (format "/rest/api/space/%s/content?depth=root&limit=%s"
          space org-conf-parent-child-page-limit))

(defun org-conf--fetch-all-pages (api get-json &optional next-fn)
  "Return every (:id :title) reachable from API, following `next' links.
GET-JSON is called with an API path and must return the parsed JSON, or
nil when the request failed.  At most
`org-conf--max-child-page-requests' requests are issued.

NEXT-FN, when non-nil, is called with the original API path and the
next link to produce the next request path; space-root listings use it
to keep `depth=root' on page two."
  (let ((json (funcall get-json api))
        (pages nil)
        (requests 1))
    (while (and json (<= requests org-conf--max-child-page-requests))
      (setq pages (append pages (org-conf--parse-child-page-json json)))
      (let ((next (org-conf--page-json-next json)))
        (setq json (and next
                        (< requests org-conf--max-child-page-requests)
                        (funcall get-json
                                 (if next-fn
                                     (funcall next-fn api next)
                                   next))))
        (when json (setq requests (1+ requests)))))
    pages))

(defun org-conf--fetch-all-children (page-id get-json)
  "Return PAGE-ID's direct children as a list of (:id :title) plists."
  (org-conf--fetch-all-pages (org-conf--child-page-api page-id) get-json))

(defun org-conf--fetch-all-space-roots (space get-json)
  "Return SPACE's top-level pages as a list of (:id :title) plists."
  (org-conf--fetch-all-pages (org-conf--space-root-page-api space) get-json
                             #'org-conf--space-root-next))

(defun org-conf--page-json-results (json)
  "Return the entry alists in JSON.
Understands both the flat `results' shape used by /child/page and the
grouped shape used by /space/{key}/content, where the entries live under
a content-type key such as `page'."
  (let ((results (or (assoc 'results json)
                     (assoc 'results (cdr (assoc 'page json))))))
    (append (cdr results) nil)))

(defun org-conf--page-json-next (json)
  "Return JSON's next-page link, from the flat or grouped shape."
  (or (org-conf--json-link json 'next)
      (org-conf--json-link (cdr (assoc 'page json)) 'next)))

(defun org-conf--query-param (url name)
  "Return NAME's value from URL's query string, or nil."
  (when (string-match (format "[?&]%s=\\([^&]*\\)" (regexp-quote name)) url)
    (match-string 1 url)))

(defun org-conf--space-root-next (api next)
  "Return the next space-root request after NEXT.
Confluence's grouped next link points at the per-type endpoint
\(/space/{key}/content/page), which drops `depth=root' and re-enumerates
the whole content type.  Rebuild the request from API with NEXT's
`start' offset so the listing stays root-only."
  (let ((start (and next (org-conf--query-param next "start"))))
    (if start
        (format "%s%cstart=%s" api (if (string-match-p "?" api) ?& ??) start)
      next)))

(defun org-conf--parse-child-page-json (json)
  "Turn a child/page JSON response into a list of (:id :title) plists.
Missing or empty results yield nil.  Space-content responses, which group
their entries under a content-type key, are unwrapped too."
  (mapcar (lambda (entry)
            (list :id (cdr (assoc 'id entry))
                  :title (cdr (assoc 'title entry))))
          (org-conf--page-json-results json)))

(defun org-conf--parse-created-page (json)
  "Turn a POST /rest/api/content response into a (:id :webui) plist.
:webui is the page's host-relative path, e.g. \"/spaces/DOCS/pages/1/T\"."
  (list :id (cdr (assoc 'id json))
        :webui (org-conf--json-link json 'webui)))

(defun org-conf--json-link (json rel)
  "Return the REL href from JSON's _links object, or nil.
Confluence puts pagination and page urls there, e.g. `next' and
`webui'."
  (cdr (assoc rel (cdr (assoc '_links json)))))

(defun org-conf--post-page-body (title space parent-id)
  "Build the JSON-encodable payload that creates a Confluence page.
TITLE and SPACE are strings.  PARENT-ID, when non-nil, nests the new
page under that page.  The body is deliberately empty: the real content
is PUT afterwards by the normal export flow.

Note the ancestors value is a vector rather than a list, because
`json-encode' would otherwise collapse a one-element list into a JSON
object, and Confluence rejects that."
  (append
   `(("type" . "page")
     ("title" . ,title)
     ("space" . (("key" . ,space)))
     ("body" . (("storage" . (("value" . "")
                              ("representation" . "storage"))))))
   (when parent-id
     `(("ancestors" . ,(vector (list (cons "id" parent-id))))))))

(defun org-conf--freq-alist-p (freq)
  "Non-nil when FREQ looks like an alist of (PAGE-ID . COUNT)."
  (and (listp freq)
       (catch 'bad
         (dolist (cell freq)
           (unless (and (consp cell)
                        (stringp (car cell))
                        (integerp (cdr cell)))
             (throw 'bad nil)))
         t)))

(defun org-conf--freq-load (file)
  "Read the parent-page usage alist from FILE.
Return nil when FILE is missing, unreadable, or does not hold a valid
usage alist."
  (when (file-readable-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((data (read (current-buffer))))
            (when (org-conf--freq-alist-p data) data)))
      (error nil))))

(defun org-conf--freq-save (file freq)
  "Write FREQ to FILE as a plain elisp alist, creating directories."
  (let ((dir (file-name-directory file)))
    (when dir (make-directory dir t)))
  (with-temp-file file
    (let ((print-level nil)
          (print-length nil))
      (prin1 freq (current-buffer))
      (insert "\n"))))

(defun org-conf--freq-bump (id freq)
  "Return FREQ with ID's usage count increased by one.
FREQ itself is not modified."
  (let ((new (copy-alist freq))
        (cell nil))
    (if (setq cell (assoc id new))
        (setcdr cell (1+ (cdr cell)))
      (push (cons id 1) new))
    new))

(defconst org-conf--front-matter-comment-regexp
  "^[ \t]*\\(?:#!\\|# -*-\\|#\\(?:[ \t]\\|$\\)\\)"
  "A shebang, an Emacs mode line, or an org comment line.")

(defun org-conf--front-matter-start ()
  "Return the position where the leading #+ keyword block begins.
Blank lines, shebangs, mode lines and org comments before the block are
skipped, so nothing is ever inserted above them."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (or (looking-at-p "^[ \t]*$")
                    (looking-at-p org-conf--front-matter-comment-regexp)))
      (forward-line 1))
    (point)))

(defun org-conf--keyword-block-end ()
  "Return the position just after the file's leading #+ keyword block.
That is where new keywords belong: after #+TITLE: and friends, but
before any blank line or body text.  Leading shebangs, mode lines and
org comments are skipped, so the block never starts above them.  Returns
the front-matter start when the file has no keyword block."
  (save-excursion
    (goto-char (org-conf--front-matter-start))
    (let ((end (point)))
      (while (and (not (eobp)) (org-at-keyword-p))
        (forward-line 1)
        (setq end (point)))
      end)))

(defun org-conf--insert-conf-keywords (space page-url id)
  "Write the #+CONF_* keywords for a freshly created page.
SPACE, PAGE-URL and ID describe the new page.

Existing CONF_SPACE, CONF_PAGE and CONF_ID lines are removed first, so
the block never duplicates.  It is then placed at the end of the file's
leading #+ keyword block -- after #+TITLE: and friends, never above
them -- in a canonical order."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#\\+CONF_\\(SPACE\\|PAGE\\|ID\\):.*\n?" nil t)
      (replace-match "" t t))
    (goto-char (org-conf--keyword-block-end))
    (insert (format "#+CONF_SPACE: %s\n#+CONF_PAGE: %s\n#+CONF_ID: %s\n"
                    space page-url id))))

(defcustom org-conf-parent-root-label "（建在 space 根目录）"
  "The choice that creates a page without a parent page."
  :type 'string
  :group 'org-export-conf)

(defun org-conf--unique-labels (pairs)
  "Return PAIRS, an alist of (LABEL . ID), with colliding labels made unique.
A label shared by two or more entries gets its id appended in brackets;
without that, `completing-read' would return the same string for both
and the second entry could never be selected."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (pair pairs)
      (puthash (car pair) (1+ (gethash (car pair) counts 0)) counts))
    (mapcar (lambda (pair)
              (if (> (gethash (car pair) counts 1) 1)
                  (cons (format "%s [%s]" (car pair) (cdr pair)) (cdr pair))
                pair))
            pairs)))

(defcustom org-conf-manual-parent-label "（手动输入页面 URL/ID）"
  "The choice that falls back to typing a parent page URL or id."
  :type 'string
  :group 'org-export-conf)

(defconst org-conf--drill-up-label ".."
  "The choice that climbs to the parent page during drill-down.")

(defun org-conf--drill-confirm-label (title)
  "Return the drill-down choice that settles on TITLE as the parent page."
  (format "✓ 就用「%s」作为父页面" title))

(defun org-conf--drill-choices (current children &optional recent)
  "Build the `completing-read' choices for one drill-down step.
CURRENT is the page being offered for confirmation, or nil when we are
at the space top level.  CHILDREN are CURRENT's child pages, which the
user may descend into.  RECENT are already-built `(use ID)' shortcuts
for frequently used pages.

Return an alist of (DISPLAY . ACTION).  ACTION is `(root)', `(up)',
`(manual)', `(confirm ID)', `(use ID)' or `(descend ID)'.  The file-like
order is `..', the current page, recent pages, direct children, manual."
  (append
   (if current
       (let ((parent-id (plist-get current :parent-id)))
         (list (cons (if parent-id org-conf--drill-up-label
                       org-conf-parent-root-label)
                     (if parent-id '(up) '(root)))
               (cons (org-conf--drill-confirm-label (plist-get current :title))
                     (list 'confirm (plist-get current :id)))))
     (list (cons org-conf-parent-root-label '(root))))
   recent
   (mapcar (lambda (pair) (cons (car pair) (list 'descend (cdr pair))))
           (org-conf--unique-labels
            (mapcar (lambda (c) (cons (plist-get c :title) (plist-get c :id)))
                    children)))
   (list (cons org-conf-manual-parent-label '(manual)))))

(defun org-conf--parent-choices (candidates)
  "Build `completing-read' choices from CANDIDATES.
Return an alist of (DISPLAY . PAGE-ID); a nil id means \"create at the
space root\", which is always offered first."
  (cons (cons org-conf-parent-root-label nil)
        (org-conf--unique-labels
         (mapcar (lambda (c) (cons (plist-get c :path) (plist-get c :id)))
                 candidates))))

(defun org-conf--export-action (conf-id)
  "Return `update' when CONF-ID names an existing page, else `create'.
This is what makes \"f f\" branch between updating and creating."
  (if conf-id 'update 'create))

(defun org-conf--default-title (org-title file-name)
  "Pick a page title from ORG-TITLE, the file's #+TITLE, or nil.
Falls back to the base name of FILE-NAME, then to the empty string.
A whitespace-only ORG-TITLE counts as absent."
  (let ((title (and org-title (string-trim org-title))))
    (or (and title (not (string-empty-p title)) title)
        (and file-name (file-name-base file-name))
        "")))

(defun org-conf--page-url (webui)
  "Expand WEBUI, a page path or an absolute URL, into a full URL.
Absolute URLs (as returned by the CLI backend) pass through unchanged.
Returns the empty string when WEBUI is nil, so a missing _links entry
cannot produce a broken #+CONF_PAGE:."
  (cond
   ((not webui) "")
   ((string-match-p "\\`https?://" webui) webui)
   (t (org-conf--return-api-url webui))))

;;; ======================================================================
;;;                          Request Methods
;;; ======================================================================
(defun org-conf-rest-get-page-json (conf-id)
  "Retrieve a Confluence page's status information using GET method."
  (request-response-data
    (request
      (org-conf--return-api-url (format "/rest/api/content/%s?expand=version.number,space,_links,ancestors" conf-id))
      :auth "basic"
      ;; Parser is like the post-process in export backend,
      ;; the data used in "success" function will be processed
      ;; by parser, then passed to "success". Here, we parse
      ;; response from string to json. By setting "sync" to t,
      ;; we make the data responses after "success".
      :parser 'json-read
      :sync t
      :timeout 2
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (when data (message "Got Page!"))))
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (message "Got error: %S" error-thrown)))
      )))

(defun org-conf--rest-get-json (api)
  "GET API and return the parsed JSON, or nil when the request failed."
  (let ((request-log-level org-conf-log-level)
        (request-message-level org-conf-log-level))
    (request-response-data
     (request
       (org-conf--return-api-url api)
       :auth "basic"
       :parser 'json-read
       :sync t
       :timeout 10
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (when data (message "Got %s" api))))
       :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                             (message "Got error: %S" error-thrown)))))))

(defun org-conf-rest-get-page-node (page-id)
  "Return a (:id :title) plist describing PAGE-ID.
A nil :title means the page could not be read."
  (let ((json (org-conf--rest-get-json (format "/rest/api/content/%s" page-id))))
    (list :id page-id :title (cdr (assoc 'title json)))))

(defun org-conf-rest-get-children (page-id)
  "Return PAGE-ID's direct children as a list of (:id :title) plists."
  (org-conf--fetch-all-children page-id #'org-conf--rest-get-json))

(defun org-conf-rest-get-space-roots (space)
  "Return SPACE's top-level pages as a list of (:id :title) plists."
  (org-conf--fetch-all-space-roots space #'org-conf--rest-get-json))

(defun org-conf-rest-post-page (title space parent-id)
  "Create a page titled TITLE in SPACE under PARENT-ID.
PARENT-ID nil means the space root.  Return a (:id :webui) plist; a nil
:id means Confluence refused to create the page."
  (let ((request-log-level org-conf-log-level)
        (request-message-level org-conf-log-level))
    (org-conf--parse-created-page
     (request-response-data
      (request
        (org-conf--return-api-url "/rest/api/content")
        :auth "basic"
        :type "POST"
        :data (json-encode (org-conf--post-page-body title space parent-id))
        :headers '(("Content-Type" . "application/json"))
        :parser 'json-read
        :sync t
        :timeout 10
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (when data (message "Created page!"))))
        :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                              (message "Got error: %S" error-thrown))))))))

(defun org-conf-rest-put (conf-id title conf-space body version ver-msg)
  (let* ((request-log-level org-conf-log-level)
         (request-message-level org-conf-log-level))
    (request-response-data
     (request
       (org-conf--return-api-url (format "/rest/api/content/%s" conf-id))
       :auth "basic"
       :type "PUT"
       :data (json-encode
              `(("id" . ,conf-id)
                ("type" . "page")
                ("title" . ,title)
                ("space" . (("key" . ,conf-space)))
                ("body" . (("storage" . (("value" . ,body)
                                         ("representation" . "storage")))))
                ("version" . (("number" . ,version)
                              ("message" . ,ver-msg)))))
       :headers '(("Content-Type" . "application/json"))
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (message "Put Succeed!")))
       :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                             (message "Got error: %S" error-thrown)))
       ))
    )
  )

(defun org-conf-rest-get-attach-json (conf-id)
  "Get attachements from CONF-ID page through rest api."
  (request-response-data
    (request
      (org-conf--return-api-url (format "/rest/api/content/%s/child/attachment" conf-id))
      :auth "basic"
      ;; Parser is like the post-process in export backend,
      ;; the data used in "success" function will be processed
      ;; by parser, then passed to "success". Here, we parse
      ;; response from string to json, and convert json to plist.
      ;; By setting "sync" to t, we make the data responses after
      ;; "success".
      :parser (lambda ()
               (let ((json-object-type 'plist))
                 (json-read)))
      :sync t
      :timeout 2
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (when data (message "Got Attachment(s)!"))))
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (message "Got error: %S" error-thrown)))
      )))

(defun org-conf-rest-post-file (conf-id filepath &optional attach-id)
  (let* ((request-log-level org-conf-log-level)
         (request-message-level org-conf-log-level)
         (request-url))
    (if attach-id
        (progn
          (setq request-url
                (org-conf--return-api-url
                 (format "/rest/api/content/%s/child/attachment/%s/data" conf-id attach-id))))
      (progn
        (setq request-url
              (org-conf--return-api-url
               (format "/rest/api/content/%s/child/attachment" conf-id)))))
    (request-response-data
     (request
       request-url
       :auth "basic"
       :type "POST"
       :files `(("file" . ,(expand-file-name filepath)))
       :user-defined-form '(("minorEdit" . "\"ture\""))
       :headers '(("X-Atlassian-Token" . "no-check"))
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (message "Posted!")))
       :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                             (message "Got error: %S" error-thrown)))
       ))
    )
  )

;;; ======================================================================
;;;                          Backend Dispatch
;;; ======================================================================
(defun org-conf--active-backend ()
  "Return the backend to use: `rest' or `cli'.
`auto' prefers the CLI when `org-conf-cli-program' is on `exec-path'."
  (pcase org-conf-backend
    ('cli 'cli)
    ('rest 'rest)
    ('auto (if (executable-find org-conf-cli-program) 'cli 'rest))
    (_ (user-error "Unknown `org-conf-backend': %S" org-conf-backend))))

(defvar org-conf--backend-functions
  '((rest
     (:get-page-info . org-conf-rest-get-page-info)
     (:get-children . org-conf-rest-get-children)
     (:get-space-roots . org-conf-rest-get-space-roots)
     (:create-page . org-conf-rest-post-page)
     (:update-page . org-conf-rest-put)
     (:list-attachments . org-conf-rest-list-attachments)
     (:upload-attachment . org-conf-rest-post-file)
     (:search-pages . org-conf-rest-search-pages))
    (cli
     (:get-page-info . org-conf-cli-get-page-info)
     (:get-children . org-conf-cli-get-children)
     (:get-space-roots . org-conf-cli-get-space-roots)
     (:create-page . org-conf-cli-create-page)
     (:update-page . org-conf-cli-update-page)
     (:list-attachments . org-conf-cli-list-attachments)
     (:upload-attachment . org-conf-cli-upload-attachment)
     (:search-pages . org-conf-cli-search-pages)))
  "Backend implementations, as an alist of BACKEND . ((OP . FUNCTION) ...).")

(defun org-conf--backend-fn (op)
  "Return the function implementing OP for the active backend."
  (or (cdr (assq op (alist-get (org-conf--active-backend)
                               org-conf--backend-functions)))
      (user-error "Backend %s does not implement %s"
                  (org-conf--active-backend) op)))

(defun org-conf-backend-get-page-info (page-id)
  "Return metadata for PAGE-ID from the active backend."
  (org-conf--log-call
   :get-page-info (list page-id)
   (lambda ()
     (funcall (org-conf--backend-fn :get-page-info) page-id))))

(defun org-conf-backend-get-children (page-id)
  "Return PAGE-ID's direct children from the active backend."
  (org-conf--log-call
   :get-children (list page-id)
   (lambda ()
     (funcall (org-conf--backend-fn :get-children) page-id))))

(defun org-conf-backend-get-space-roots (space)
  "Return SPACE's top-level pages from the active backend."
  (org-conf--log-call
   :get-space-roots (list space)
   (lambda ()
     (funcall (org-conf--backend-fn :get-space-roots) space))))

(defun org-conf-backend-create-page (title space parent-id)
  "Create a page through the active backend."
  (org-conf--log-call
   :create-page (list title space parent-id)
   (lambda ()
     (funcall (org-conf--backend-fn :create-page) title space parent-id))))

(defun org-conf-backend-update-page (page-id title space body version message)
  "Update PAGE-ID through the active backend."
  (org-conf--log-call
   :update-page (list page-id title space body version message)
   (lambda ()
     (funcall (org-conf--backend-fn :update-page)
              page-id title space body version message))))

(defun org-conf-backend-list-attachments (page-id)
  "Return PAGE-ID's attachments from the active backend."
  (org-conf--log-call
   :list-attachments (list page-id)
   (lambda ()
     (funcall (org-conf--backend-fn :list-attachments) page-id))))

(defun org-conf-backend-search-pages (space query)
  "Return pages in SPACE whose titles match QUERY."
  (org-conf--log-call
   :search-pages (list space query)
   (lambda ()
     (funcall (org-conf--backend-fn :search-pages) space query))))

(defun org-conf-backend-upload-attachment (page-id filepath &optional attach-id)
  "Upload FILEPATH to PAGE-ID through the active backend."
  (org-conf--log-call
   :upload-attachment (list page-id filepath attach-id)
   (lambda ()
     (funcall (org-conf--backend-fn :upload-attachment)
              page-id filepath attach-id))))

(defun org-conf-rest-get-page-info (page-id)
  "Return PAGE-ID's metadata as a plist from the REST API.
Keys: :id, :title, :version, :space, :parent-id, :ancestors and :url.
:ancestors is the root-first chain of (:id :title) plists above the page."
  (let ((json (org-conf-rest-get-page-json page-id)))
    (when json
      (let* ((raw-ancestors (append (cdr (assoc 'ancestors json)) nil))
             (ancestors (mapcar (lambda (ancestor)
                                  (list :id (cdr (assoc 'id ancestor))
                                        :title (cdr (assoc 'title ancestor))))
                                raw-ancestors)))
        (list :id (or (cdr (assoc 'id json)) page-id)
              :title (cdr (assoc 'title json))
              :version (cdr (assoc 'number (assoc 'version json)))
              :space (cdr (assoc 'key (assoc 'space json)))
              :parent-id (cdr (assoc 'id (car (last raw-ancestors))))
              :ancestors ancestors
              :url (org-conf--page-url (org-conf--json-link json 'webui)))))))

(defun org-conf-rest-list-attachments (page-id)
  "Return PAGE-ID's attachments as (:id :title) plists from the REST API."
  (mapcar (lambda (attachment)
            (list :id (plist-get attachment :id)
                  :title (plist-get attachment :title)))
          (append (plist-get (org-conf-rest-get-attach-json page-id) :results)
                  nil)))

(defun org-conf-rest-search-pages (space query)
  "Return pages in SPACE whose title matches QUERY, via CQL."
  (let* ((cql (format "title ~ \"%s\" and space = \"%s\"" query space))
         (json (org-conf--rest-get-json
                (format "/rest/api/content/search?cql=%s&limit=%s"
                        (url-hexify-string cql)
                        org-conf-parent-child-page-limit))))
    (mapcar (lambda (result)
              (list :id (cdr (assoc 'id result))
                    :title (cdr (assoc 'title result))
                    :type (cdr (assoc 'type result))))
            (append (cdr (assoc 'results json)) nil))))

;;; ======================================================================
;;;                     confluence-cli Process Layer
;;; ======================================================================
(defun org-conf--cli-program ()
  "Return the absolute path of `org-conf-cli-program'."
  (or (executable-find org-conf-cli-program)
      (user-error "Confluence CLI not found: %s (set `org-conf-cli-program')"
                  org-conf-cli-program)))

(defun org-conf--cli-error-message (stderr-file)
  "Return the message in STDERR-FILE, preferring the CLI's JSON `error'."
  (let ((text (when (file-readable-p stderr-file)
                (with-temp-buffer
                  (insert-file-contents stderr-file)
                  (string-trim (buffer-string))))))
    (if (string-empty-p (or text ""))
        "no error output"
      (or (ignore-errors
            (alist-get 'error
                       (json-parse-string text :object-type 'alist)))
          text))))

(defun org-conf--cli-json (&rest args)
  "Run the confluence CLI with ARGS and return the parsed JSON.
The global `--json' flag is prepended to ARGS.  Signals `user-error'
when the CLI exits non-zero or prints something that is not JSON."
  (let* ((coding-system-for-read 'utf-8-unix)
         (coding-system-for-write 'utf-8-unix)
         (program (org-conf--cli-program))
         (stderr (make-temp-file "ox-conf-cli-err-"))
         (start (float-time))
         status output)
    (org-conf--log "   CMD (cli): %s" (org-conf--cli-command-string args))
    (unwind-protect
        (progn
          (with-temp-buffer
            (setq status (apply #'call-process program nil
                                (list (current-buffer) stderr) nil
                                "--json" args))
            (setq output (buffer-string)))
          (if (and (integerp status) (zerop status))
              (org-conf--log "   ✅ exit=0 (%.2fs)"
                             (- (float-time) start))
            (org-conf--log "   ❌ exit=%s (%.2fs)\n   STDERR: %s"
                           status (- (float-time) start)
                           (org-conf--cli-error-message stderr)))
          (unless (and (integerp status) (zerop status))
            (user-error "confluence %s failed: %s"
                        (mapconcat #'identity args " ")
                        (org-conf--cli-error-message stderr)))
          (condition-case err
              (with-temp-buffer
                (insert output)
                (goto-char (point-min))
                (json-parse-buffer :object-type 'alist
                                   :array-type 'list
                                   :null-object nil
                                   :false-object nil))
            (error
             (user-error "confluence %s returned invalid JSON: %s"
                         (mapconcat #'identity args " ")
                         (error-message-string err)))))
      (delete-file stderr))))

(defvar org-conf--cli-json-function #'org-conf--cli-json
  "Function used to run the CLI and parse its JSON.
Rebind this (in tests or an alternative front end) to replace the
transport without redefining `org-conf--cli-json'.")

(defun org-conf--cli-invoke (&rest args)
  "Call `org-conf--cli-json-function' with ARGS."
  (apply org-conf--cli-json-function args))

(defun org-conf--write-utf8-file (text)
  "Write TEXT to a fresh UTF-8 temporary file and return its name."
  (let ((file (make-temp-file "ox-conf-body-" nil ".html"))
        (coding-system-for-write 'utf-8-unix))
    (with-temp-file file
      (insert text))
    file))

(defun org-conf--with-temp-body (text fn)
  "Call FN with a temporary UTF-8 file holding TEXT, then delete it.
If FN signals, keep the file and log its path so a failed CLI command
can be rerun."
  (let ((file (org-conf--write-utf8-file text))
        (ok nil))
    (unwind-protect
        (prog1 (funcall fn file)
          (setq ok t))
      (if ok
          (delete-file file)
        (org-conf--log "   body kept for debugging: %s" file)))))

;;; ======================================================================
;;;                   confluence-cli Backend Operations
;;; ======================================================================
(defun org-conf--cli-api-path (api)
  "Turn a REST API path into one relative to the CLI's `apiPath'."
  (replace-regexp-in-string "\\`/*\\(?:rest/api/\\)?" "" api))

(defun org-conf-cli-get-page-info (page-id)
  "Return PAGE-ID's metadata as a plist from the confluence CLI.
Keys: :id, :title, :version, :space, :parent-id, :ancestors and :url."
  (let* ((json (org-conf--cli-invoke "info" page-id))
         (ancestors (mapcar (lambda (ancestor)
                              (list :id (alist-get 'id ancestor)
                                    :title (alist-get 'title ancestor)))
                            (alist-get 'ancestors json))))
    (list :id (alist-get 'id json)
          :title (alist-get 'title json)
          :version (alist-get 'version json)
          :space (alist-get 'spaceKey json)
          :parent-id (alist-get 'parentId json)
          :ancestors ancestors
          :url (alist-get 'url json))))

(defun org-conf-cli-get-children (page-id)
  "Return PAGE-ID's direct children as (:id :title) plists."
  (mapcar (lambda (child)
            (list :id (alist-get 'id child)
                  :title (alist-get 'title child)))
          (alist-get 'children (org-conf--cli-invoke "children" page-id))))

(defun org-conf-cli-get-space-roots (space)
  "Return SPACE's top-level pages as (:id :title) plists."
  (let* ((base (org-conf--cli-api-path (org-conf--space-root-page-api space)))
         (api base)
         (pages nil)
         (requests 0))
    (while (and api (< requests org-conf--max-child-page-requests))
      (let ((json (org-conf--cli-invoke "api" api)))
        (setq pages (append pages (org-conf--parse-child-page-json json)))
        (setq api (org-conf--space-root-next base (org-conf--page-json-next json)))
        (setq requests (1+ requests))))
    pages))

(defun org-conf-cli-create-page (title space parent-id)
  "Create a page titled TITLE in SPACE under PARENT-ID through the CLI.
Return a (:id :webui) plist; :webui is the CLI's absolute page URL."
  (let ((json (org-conf--with-temp-body
                  ""
                (lambda (file)
                  (if parent-id
                      (org-conf--cli-invoke "create-child" title parent-id
                                            "--file" file)
                    (org-conf--cli-invoke "create" title space "--file" file))))))
    (list :id (alist-get 'id json)
          :webui (alist-get 'url json))))

(defun org-conf-cli-update-page (page-id title space body version message)
  "Update PAGE-ID through the CLI.
SPACE, VERSION and MESSAGE are accepted for interface compatibility and
ignored: the CLI fetches the current version itself."
  (org-conf--with-temp-body
      body
    (lambda (file)
      (apply #'org-conf--cli-invoke
             (append (list "update" page-id)
                     (unless (string-empty-p (or title ""))
                       (list "--title" title))
                     (list "--file" file "--format" "storage"))))))

(defun org-conf-cli-list-attachments (page-id)
  "Return PAGE-ID's attachments as (:id :title) plists from the CLI."
  (mapcar (lambda (attachment)
            (list :id (alist-get 'id attachment)
                  :title (alist-get 'title attachment)))
          (alist-get 'attachments (org-conf--cli-invoke "attachments" page-id))))

(defun org-conf-cli-search-pages (space query)
  "Return pages in SPACE whose title matches QUERY, via the CLI."
  (let ((cql (format "title ~ \"%s\" and space = \"%s\"" query space)))
    (mapcar (lambda (result)
              (list :id (alist-get 'id result)
                    :title (alist-get 'title result)
                    :type (alist-get 'type result)))
            (alist-get 'results
                       (org-conf--cli-invoke
                        "search" cql "--cql"
                        "--limit" (number-to-string
                                   org-conf-parent-child-page-limit))))))

(defun org-conf-cli-upload-attachment (page-id filepath &optional attach-id)
  "Upload FILEPATH to PAGE-ID through the CLI.
ATTACH-ID only signals that an existing attachment should be replaced."
  (apply #'org-conf--cli-invoke
         (append (list "attachment-upload" page-id "--file" filepath)
                 (when attach-id (list "--replace")))))

(defun org-conf--upload-img (conf-id link)
  "Given image file or attachment's path LINK, upload it to conf page with CONF-ID.
Ask for confirmation when an attachment with the same name exists."
  (let* ((attachments (org-conf-backend-list-attachments conf-id))
         (filename (file-name-nondirectory link))
         (attach-id (plist-get (seq-find
                                (lambda (attachment)
                                  (equal filename (plist-get attachment :title)))
                                attachments)
                               :id)))
    (if attach-id
        (when (y-or-n-p (format "Attachment '%s' already exists, replace?" filename))
          (org-conf-backend-upload-attachment conf-id link attach-id))
      (org-conf-backend-upload-attachment conf-id link nil))))


;;; ======================================================================
;;;                          Json Parsers
;;; ======================================================================
(defun org-conf-page-json-parser (page-json)
  (message (format "===> Title is: %s, version is: %s"
                   (cdr (assoc 'title page-json))
                   (cdr (assoc 'number (assoc 'version page-json))))))

(defun org-conf--parse-attach-json (attachs-json &optional filename)
  "Pasrse the attachments' json into string info.
@param ATTACHS-JSON: attachments's json
@param FILENAME:     specify an attachment's filename"
  ;; `append will convert vector to list
  (let ((attachs (append (plist-get attachs-json :results) nil))
        (cur-title nil)
        (cur-id nil))
    (catch 'get-attach-id
      (dolist (ele attachs)
        (setq cur-title (plist-get ele :title))
        (setq cur-id (plist-get ele :id))
        (if filename
            (if (string= cur-title filename) (throw 'get-attach-id cur-id))
          ;; No filename provided, just print attachments.
          (message "===> Image info: title is %s, ID is %s." cur-title cur-id))))))

;;; ======================================================================
;;;                          Export Functions                             
;;; ======================================================================
(defun org-conf-export-page-info
    (&optional async subtreep visible-only body-only ext-plist)
  "Export message about current article's status."
  (interactive)
  ;; Option "conf-id" cannot be retrieved after org-export-to-buffer!
  ;; So put it above exporting.
  (let ((conf-id (plist-get (org-export--get-inbuffer-options 'org-conf-html) :conf-id))
        (user (car (plist-get (org-export--get-inbuffer-options 'org-conf-html) :author)))
        (res nil))
    (if conf-id
        (let ((info (org-conf-backend-get-page-info conf-id)))
          (message "===> Title is: %s, version is: %s"
                   (plist-get info :title) (plist-get info :version)))
      (message "No CONF_ID found!"))))

(defun org-conf-export-img-info
    (&optional async subtreep visible-only body-only ext-plist)
  "Export message about current article's image attachements info."
  (interactive)
  ;; Option "conf-id" cannot be retrieved after org-export-to-buffer!
  ;; So put it above exporting.
  (let ((conf-id (plist-get (org-export--get-inbuffer-options 'org-conf-html) :conf-id))
        (user (car (plist-get (org-export--get-inbuffer-options 'org-conf-html) :author)))
        (res nil))
    (if conf-id
        (dolist (attachment (org-conf-backend-list-attachments conf-id))
          (message "===> Image info: title is %s, ID is %s."
                   (plist-get attachment :title)
                   (plist-get attachment :id)))
      (message "No CONF_ID found!"))))

(defun org-conf--read-space ()
  "Prompt for a Confluence space key, defaulting to `org-conf-default-space'."
  (let ((space (string-trim (read-string "Confluence space: "
                                         org-conf-default-space))))
    (if (string-empty-p space)
        (user-error "A Confluence space key is required")
      space)))

(defun org-conf--read-title ()
  "Prompt for the new page's title.
Defaults to the buffer's #+TITLE, then to its file name."
  (let* ((info (org-export--get-inbuffer-options 'org-conf-html))
         (default (org-conf--default-title
                   (car (plist-get info :title))
                   (buffer-file-name)))
         (title (string-trim (read-string "Page title: " default))))
    (if (string-empty-p title)
        (user-error "A page title is required")
      title)))

(defun org-conf--read-parent-id-manually ()
  "Ask for a parent page URL or id.
Return nil when the answer is blank, meaning the space root."
  (let ((typed (string-trim
                (read-string "Parent page URL/ID (blank = space root): "))))
    (if (string-empty-p typed)
        nil
      (or (org-conf--return-id-from-url typed)
          (user-error "No page id found in %s" typed)))))

(defun org-conf--read-or-nil (function &rest args)
  "Call FUNCTION with ARGS, returning nil and messaging on backend failure.
Only `user-error' is caught: the REST backend already returns nil on a
failed read, while the CLI backend raises."
  (condition-case err
      (apply function args)
    (user-error
     (message "%s" (error-message-string err))
     nil)))

(defvar org-conf--page-info-cache (make-hash-table :test 'equal)
  "Session cache of page metadata, keyed by page id.")

(defvar org-conf--scope-cache (make-hash-table :test 'equal)
  "Session cache of inferred parent scopes, keyed by DIRECTORY\\0SPACE.")

(defun org-conf--cached-page-info (page-id)
  "Return PAGE-ID's metadata, caching successful reads for this session."
  (let ((cached (gethash page-id org-conf--page-info-cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let ((info (org-conf--read-or-nil #'org-conf-backend-get-page-info page-id)))
        (when info
          (puthash page-id info org-conf--page-info-cache))
        info))))

(defun org-conf--common-prefix (a b)
  "Return the common prefix of ancestor lists A and B."
  (let ((prefix nil))
    (while (and a b (equal (plist-get (car a) :id)
                           (plist-get (car b) :id)))
      (push (car a) prefix)
      (setq a (cdr a)
            b (cdr b)))
    (nreverse prefix)))

(defun org-conf--common-ancestor (chains)
  "Return the deepest plist shared by all CHAINS, or nil.
Each CHAIN is a root-first list of (:id :title) plists."
  (let ((chains (delq nil (mapcar (lambda (chain)
                                    (and chain (append chain nil)))
                                  chains))))
    (when chains
      (let ((prefix (car chains)))
        (dolist (chain (cdr chains))
          (setq prefix (org-conf--common-prefix prefix chain)))
        (car (last prefix))))))

(defun org-conf--parent-directory (dir)
  "Return DIR's parent directory, or nil at the filesystem root."
  (let* ((expanded (expand-file-name dir))
         (parent (file-name-directory (directory-file-name expanded))))
    (unless (equal parent expanded)
      parent)))

(defun org-conf--org-files-near (dir)
  "Return .org files in DIR and its immediate subdirectories."
  (append (directory-files dir t "\\.org\\'" t)
          (cl-loop for sub in (directory-files dir t "\\`[^.]" t)
                   when (file-directory-p sub)
                   append (directory-files sub t "\\.org\\'" t))))

(defun org-conf--conf-ids-in-directory (dir space)
  "Return up to 8 page ids from .org files near DIR whose CONF_SPACE is SPACE.
\"Near\" means DIR itself and its immediate subdirectories, so a topic
directory learns from its sibling topic directories.  The current
buffer's file is skipped."
  (let ((current (and buffer-file-name
                      (ignore-errors (file-truename buffer-file-name))))
        (ids nil)
        (count 0))
    (when (file-directory-p dir)
      (dolist (file (org-conf--org-files-near dir))
        (when (and (< count 8)
                   (not (equal (ignore-errors (file-truename file)) current)))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (let ((file-space
                   (when (re-search-forward
                          "^#\\+CONF_SPACE:[ \t]*\\([^ \t\n]+\\)" nil t)
                     (match-string 1))))
              (when (equal space file-space)
                (goto-char (point-min))
                (when (re-search-forward
                       "^#\\+CONF_ID:[ \t]*\\([^ \t\n]+\\)" nil t)
                  (push (match-string 1) ids)
                  (setq count (1+ count)))))))))
    (nreverse ids)))

(defun org-conf--name-matched-scope (space dir)
  "Find a page in SPACE whose title best matches DIR's name.
The shortest matching title wins, so `vml' prefers `HygonVML' over
`HygonVML算法记录'.  Attachments and other non-page results are ignored."
  (let* ((name (file-name-nondirectory (directory-file-name dir)))
         (needle (downcase name))
         (results (org-conf--read-or-nil #'org-conf-backend-search-pages
                                         space name))
         (matches (seq-filter
                   (lambda (page)
                     (and (or (null (plist-get page :type))
                              (equal "page" (plist-get page :type)))
                          (string-match-p
                           (regexp-quote needle)
                           (downcase (or (plist-get page :title) "")))))
                   results))
         (best (car (sort (copy-sequence matches)
                          (lambda (a b)
                            (< (length (plist-get a :title))
                               (length (plist-get b :title))))))))
    (when best
      (org-conf--cached-page-info (plist-get best :id)))))

(defun org-conf--scope-in-directory (dir space)
  "Return the common ancestor page of DIR's exported .org files."
  (let ((ancestor (org-conf--common-ancestor
                   (delq nil
                         (mapcar (lambda (id)
                                   (plist-get (org-conf--cached-page-info id)
                                              :ancestors))
                                 (org-conf--conf-ids-in-directory dir space))))))
    (when ancestor
      (org-conf--cached-page-info (plist-get ancestor :id)))))

(defun org-conf--recent-parent-scope (space)
  "Return the parent of the most-used parent page in SPACE, if any.
This turns recently used parents into a starting point for the picker."
  (let ((freq (sort (copy-sequence
                     (org-conf--freq-load org-conf-parent-freq-file))
                    (lambda (a b) (> (cdr a) (cdr b)))))
        (scope nil)
        (tried 0))
    (while (and freq (not scope) (< tried 5))
      (let ((info (org-conf--cached-page-info (caar freq))))
        (setq tried (1+ tried))
        (when (and info (equal space (plist-get info :space)))
          (let ((parent-id (plist-get info :parent-id)))
            (setq scope
                  (if parent-id
                      (let ((parent (org-conf--cached-page-info parent-id)))
                        (and parent
                             (equal space (plist-get parent :space))
                             parent))
                    info)))))
      (setq freq (cdr freq)))
    scope))

(defun org-conf--infer-parent-scope-uncached (space start)
  "Infer a parent scope for SPACE starting at directory START.
Prefer the current directory's exported siblings, then the parent of
the most-used recent parent, then one or two directories up, then a
title match on the directory name."
  (let ((scope (org-conf--scope-in-directory start space))
        (dir (org-conf--parent-directory start))
        (depth 1))
    (unless scope
      (setq scope (org-conf--recent-parent-scope space)))
    (while (and dir (not scope) (<= depth org-conf-scope-search-depth))
      (setq scope (org-conf--scope-in-directory dir space))
      (setq depth (1+ depth))
      (unless scope
        (setq dir (org-conf--parent-directory dir))))
    (or scope
        (org-conf--name-matched-scope space start))))

(defun org-conf--infer-parent-scope (space)
  "Infer the Confluence scope page for the current file's directory.
The result is cached per directory and space for this session."
  (let* ((start (file-name-directory (or buffer-file-name default-directory)))
         (key (concat start "\0" space))
         (cached (gethash key org-conf--scope-cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let ((scope (org-conf--infer-parent-scope-uncached space start)))
        (puthash key scope org-conf--scope-cache)
        scope))))

(defun org-conf--recent-parent-choices (space &optional exclude)
  "Return `(use ID)' shortcuts for the most-used parents in SPACE.
EXCLUDE lists page ids already offered in the current menu."
  (let ((freq (sort (copy-sequence
                     (org-conf--freq-load org-conf-parent-freq-file))
                    (lambda (a b) (> (cdr a) (cdr b)))))
        (result nil)
        (seen nil))
    (dolist (cell freq)
      (let ((id (car cell))
            (count (cdr cell)))
        (when (and (< (length result) org-conf-recent-parent-limit)
                   (not (member id exclude))
                   (not (member id seen)))
          (let ((info (org-conf--cached-page-info id)))
            (when (and info (equal space (plist-get info :space)))
              (push (cons (format "★ 就用 %s (常用 %d 次)"
                                  (plist-get info :title) count)
                          (list 'use id))
                    result)
              (push id seen))))))
    (nreverse result)))

(defun org-conf--space-roots-or-warn (space)
  "Return SPACE's top-level pages, warning when the listing came back empty.
An empty list means either an empty space or that this Confluence
rejected `depth=root'; either way the user needs to be told rather than
shown a menu with nothing in it."
  (let ((roots (org-conf--read-or-nil #'org-conf-backend-get-space-roots space)))
    (unless roots
      (message "Space %s listed no top-level pages; type a page URL instead."
               space))
    roots))

(defun org-conf--parent-page (page)
  "Return PAGE's parent metadata, or nil at the top."
  (let ((parent-id (plist-get page :parent-id)))
    (when parent-id
      (org-conf--cached-page-info parent-id))))

(defun org-conf--read-parent-by-drill-down (space &optional scope)
  "Walk SPACE's page tree one level at a time; return the parent id.
Start at SCOPE when non-nil; `..' climbs to its parent.  Each menu lists
the current page, frequently used pages, direct children and manual
entry.  Returns nil when the user settles on the space root itself."
  (let ((current scope)
        (trail nil)
        (answer 'pending)
        (children-cache (make-hash-table :test 'equal)))
    (while (eq answer 'pending)
      (let* ((children
              (if current
                  (let ((cached (gethash (plist-get current :id)
                                         children-cache 'miss)))
                    (if (eq cached 'miss)
                        (puthash (plist-get current :id)
                                 (org-conf--read-or-nil
                                  #'org-conf-backend-get-children
                                  (plist-get current :id))
                                 children-cache)
                      cached))
                (org-conf--space-roots-or-warn space)))
             (recent (org-conf--recent-parent-choices
                      space
                      (cons (and current (plist-get current :id))
                            (mapcar (lambda (child) (plist-get child :id))
                                    children))))
             (choices (org-conf--drill-choices current children recent))
             (action (cdr (assoc (completing-read "Parent page: " choices nil t)
                                 choices))))
        (pcase action
          (`(root) (setq answer nil))
          (`(up)
           (if trail
               (setq current (pop trail))
             (setq current (org-conf--parent-page current))))
          (`(manual) (setq answer (org-conf--read-parent-id-manually)))
          (`(confirm ,id) (setq answer id))
          (`(use ,id) (setq answer id))
          (`(descend ,id)
           (let ((child (seq-find (lambda (c) (equal id (plist-get c :id)))
                                  children)))
             (when child
               (push current trail)
               (setq current (plist-put child :parent-id
                                        (plist-get current :id)))))))))
    answer))

(defun org-conf--read-parent-id (space)
  "Return the id of the page to create under, or nil for the space root.
With `org-conf-root-parent-page' set, its descendants are offered as one
flat, most-used-first list.  Otherwise SPACE's page tree is walked one
level at a time; see `org-conf--read-parent-by-drill-down'.  Either way
a URL can still be typed by hand."
  (let ((root (string-trim org-conf-root-parent-page)))
    (cond
     ((string-empty-p root)
      (let ((scope (org-conf--infer-parent-scope space)))
        (when scope
          (message "Parent scope: %s" (plist-get scope :title)))
        (org-conf--read-parent-by-drill-down space scope)))
     ((not (org-conf--return-id-from-url root))
      (message "No page id in `org-conf-root-parent-page' (%s); asking instead."
               root)
      (org-conf--read-parent-id-manually))
     (t
      (let* ((id (org-conf--return-id-from-url root))
             (node (org-conf--read-or-nil #'org-conf-backend-get-page-info id)))
        (if (not (plist-get node :title))
            (progn
              (message "Cannot read Confluence page %s; asking for a parent page instead."
                       id)
              (org-conf--read-parent-id-manually))
          (let* ((candidates (org-conf--collect-descendants
                              node
                              (lambda (page-id)
                                (org-conf--read-or-nil
                                 #'org-conf-backend-get-children page-id))
                              org-conf-parent-max-pages))
                 (sorted (org-conf--sort-parent-candidates
                          candidates
                          (org-conf--freq-load org-conf-parent-freq-file)))
                 (choices (append (org-conf--parent-choices sorted)
                                  (list (cons org-conf-manual-parent-label
                                              '(manual)))))
                 (picked (cdr (assoc (completing-read "Parent page: " choices nil t)
                                     choices))))
            (if (equal picked '(manual))
                (org-conf--read-parent-id-manually)
              picked))))))))

(defvar org-conf--skip-upload-confirm nil
  "Non-nil means `org-conf-export-to-conf' must not ask before uploading.
Bound to t while the create-page flow uploads the page it just made.")

(defun org-conf-create-page-and-export ()
  "Create a Confluence page for the current buffer, then export into it.
Ask for the target space, title and parent page, POST an empty page,
record #+CONF_ID/#+CONF_SPACE/#+CONF_PAGE in the buffer, then run the
normal `org-conf-export-to-conf' flow so the body and any images are
uploaded.  Nothing is written when Confluence refuses the creation."
  (interactive)
  (let* ((space (org-conf--read-space))
         (title (org-conf--read-title))
         (parent-id (org-conf--read-parent-id space))
         (created (org-conf-backend-create-page title space parent-id))
         (page-id (plist-get created :id)))
    (unless page-id
      (user-error "Confluence did not create the page; the buffer is untouched"))
    (org-conf--log "✅ created page %s (%s)" title page-id)
    (when parent-id
      (org-conf--freq-save org-conf-parent-freq-file
                           (org-conf--freq-bump
                            parent-id
                            (org-conf--freq-load org-conf-parent-freq-file))))
    (let ((page-url (org-conf--page-url (plist-get created :webui))))
      (org-conf--insert-conf-keywords space page-url page-id)
      (message "Created page 「%s」 (%s); uploading content..." title page-id)
      (let ((org-conf--skip-upload-confirm t))
        (org-conf-export-to-conf)))))

(defun org-conf-export-to-conf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export to Confluence through the active backend.
When the buffer has no #+CONF_ID yet, create a Confluence page for it
first; see `org-conf-create-page-and-export'."
  (interactive)
  ;; Option "conf-id" cannot be retrieved after org-export-to-buffer!
  ;; So put it above exporting.
  (let ((conf-id (plist-get (org-export--get-inbuffer-options 'org-conf-html) :conf-id))
        (conf-space (plist-get (org-export--get-inbuffer-options 'org-conf-html) :conf-space))
        (title (car (plist-get (org-export--get-inbuffer-options 'org-conf-html) :title)))
        (user (car (plist-get (org-export--get-inbuffer-options 'org-conf-html) :author)))
        (version 0)
        (ver-msg "")
        (res nil)
        (output nil)
        (org-conf-plist nil))
    (org-conf--log "✅ backend: %s, page: %s"
                   (org-conf--active-backend) conf-id)
    (if (eq 'update (org-conf--export-action conf-id))
        (let* ((info (org-conf--read-or-nil
                      #'org-conf-backend-get-page-info conf-id))
               (page-title (or (plist-get info :title)
                               (format "page %s" conf-id))))
          (if (and (not org-conf--skip-upload-confirm)
                   (not (y-or-n-p
                         (format "Upload to 「%s」 (%s)? " page-title conf-id))))
              (progn
                (org-conf--log "↪ new page requested instead of %s" conf-id)
                (org-conf-create-page-and-export))
            (org-conf--log "✅ upload confirmed: %s" page-title)
            ;; Update page version number.
            (setq res info)
            (setq version (1+ (or (plist-get res :version) 0)))
            ;; Pass export parameters through communication channel.
            (setq org-conf-plist
                  (org-combine-plists
                   ext-plist
                   '(:org-conf-do-post t)
                   `(:org-conf-user ,user)
                   `(:org-conf-id ,conf-id)))
            (setq output (org-export-as
                          'org-conf-html subtreep visible-only body-only org-conf-plist))
            (org-conf--log "✅ export: %d chars" (length output))
            ;; Update the page!
            (setq ver-msg (read-string "Version message: "))
            (org-conf-backend-update-page conf-id title conf-space output version ver-msg)))
      (org-conf-create-page-and-export))))

(defun org-conf-export-to-conf-without-post
    (&optional async subtreep visible-only body-only ext-plist)
  "Export to Confluence through the active backend."
  (interactive)
  ;; Option "conf-id" cannot be retrieved after org-export-to-buffer!
  ;; So put it above exporting.
  (let ((conf-id (plist-get (org-export--get-inbuffer-options 'org-conf-html) :conf-id))
        (conf-space (plist-get (org-export--get-inbuffer-options 'org-conf-html) :conf-space))
        (title (car (plist-get (org-export--get-inbuffer-options 'org-conf-html) :title)))
        (user (car (plist-get (org-export--get-inbuffer-options 'org-conf-html) :author)))
        (version 0)
        (ver-msg "")
        (res nil)
        (output nil)
        (org-conf-plist nil))
    (if conf-id
        (progn
          ;; Update page version number.
          (setq res (org-conf-backend-get-page-info conf-id))
          (setq version (1+ (or (plist-get res :version) 0)))
          ;; Pass export parameters through communication channel.
          (setq org-conf-plist
                (org-combine-plists
                 ext-plist
                 '(:org-conf-do-post nil)
                 `(:org-conf-user ,user)
                 `(:org-conf-id ,conf-id)))
          (setq output (org-export-as
                        'org-conf-html subtreep visible-only body-only org-conf-plist))
          (org-conf--log "✅ export: %d chars" (length output))
          ;; Update the page!
          (setq ver-msg (read-string "Version message: "))
          (org-conf-backend-update-page conf-id title conf-space output version ver-msg))
      (message "No CONF_ID found!"))))

(defun org-conf-export-to-buffer
    (&optional async subtreep visible-only body-only ext-plist)
  "Export translated contents to buffer for preview."
  (interactive)
  (org-export-to-buffer 'org-conf-html "*Confluence HTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

(defun org-conf-forget-password
    (&optional async subtreep visible-only body-only ext-plist)
  "Don't worry, let's retry the password... and forget the old one.
This only affects the REST backend; the CLI manages its own credentials."
  (interactive)
  (auth-source-forget-all-cached)
  (message "Password cache has been forgotten!%s"
           (if (eq (org-conf--active-backend) 'cli)
               " The CLI backend manages its own credentials; see `confluence profile list'."
             "")))

(defun org-conf-open-page
    (&optional async subtreep visible-only body-only ext-plist)
  "Open the confluence URL for you."
  (interactive)
  (let ((conf-page (plist-get (org-export--get-inbuffer-options 'org-conf-html) :conf-page))
        )
    (browse-url conf-page)))

;;; ======================================================================
;;;                          Export Backend                             
;;; ======================================================================
(org-export-define-derived-backend 'org-conf-html 'html
  :translate-alist
  '(
    (template . org-conf-template)
    (inner-template . org-conf-inner-template)
    (headline . org-conf-headline)
    (section . org-conf-section)
    (center-block . org-conf-center-block)
    (example-block . org-conf-example-block)
    (src-block . org-conf-src-block)
    (special-block . org-conf-special-block)
    (link . org-conf-link)
    (target . org-conf-target)
    (plain-text . org-html-plain-text)
    (footnote-reference . org-html-footnote-reference)
    )
  :filters-alist
  '(
    (:filter-latex-fragment . org-conf-latex-fragment-filter)
    (:filter-latex-environment . org-conf-latex-environment-filter)
    (:filter-export-block . org-conf-export-block-filter)
    )
  :options-alist
  '(
    (:html-text-markup-alist nil nil org-conf-text-markup-alist nil)
    (:conf-id "CONF_ID" nil nil t)
    (:conf-page "CONF_PAGE" nil nil t)
    (:conf-space "CONF_SPACE" nil org-conf-default-space t)
    )
  :menu-entry
  '(?f "Export to Confluence"
     ((?b "To buffer" org-conf-export-to-buffer)
      (?p "Page info" org-conf-export-page-info)
      (?i "Image info" org-conf-export-img-info)
      (?f "To Confluence (creates page if new)" org-conf-export-to-conf)
      (?w "To Confluence (w/o POST)" org-conf-export-to-conf-without-post)
      (?r "Forget the wrong password" org-conf-forget-password)
      (?o "Open page" org-conf-open-page)
      (?l "Show debug log" org-conf-show-log)
      ))
  )

(provide 'ox-conf)
;;; ox-conf.el ends here
