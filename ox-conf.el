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
;;     - Create one conf page on your Confluence cloud side. (Currently
;;       ox-conf.el doesn't support creating the page from scratch,
;;       instead it updates an existing page when exporting.)
;;   - Org file:
;;     - Add ~#+CONF_ID~ and ~#+CONF_SPACE~ at the top of the .org file
;;       you want to export.
;;   - Emacs init:
;;     - Install =ox-conf.el=...
;;     - Specify ~org-conf-url~ according to your host URL in your init file (e.g. =init.el=).
;;     - Specify ~org-conf-jira-chart-server~ according to your case.
;;       - Ref: https://confluence.atlassian.com/conf59/jira-chart-macro-792499235.html
;;     - Specify ~org-conf-jria-chart-server-id~ according to your case.
;; 
;; Exporting:
;;   - Wake up org-export dispatch.
;;   - Press ~f f~ for exporting to the CONF_PAGE you provided.
;;
;; Options:
;;   - ~f b~: "To buffer" org-conf-export-to-buffer
;;   - ~f p~: "Page info" org-conf-export-page-info
;;   - ~f i~: "Image info" org-conf-export-img-info
;;   - ~f f~: "To Confluence" org-conf-export-to-conf
;;   - ~f w~: "To Confluence (w/o POST)" org-conf-export-to-conf-without-post
;;   - ~f r~: "Forget the wrong password" org-conf-forget-password
;;
;; Response:
;;   - Once exported successfully, yout will get ~PUT SUCCEED~ response.
;;
;; Note:
;;   Most of the codes in this file are adapted from =ox-html.el=.
;;   Part of the codes are inspired by =org-jira.el=.

;;; Code:

(require 'ox)
(require 'request)

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

(defun org-conf--return-api-url (api)
  "Borrowed from org-jira/jiralib.el. Given a api path, return the
full URL with properly concatenated slash '/'."
  (concat (replace-regexp-in-string "/*$" "/" org-conf-url)
          (replace-regexp-in-string "^/*" "" api))
  )

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

(define-skeleton ske-conf
  "Quick insert ox-confluence options."
  nil
  "#+CONF_SPACE: " (skeleton-read "Your conf space: ") \n
  "#+CONF_PAGE: " (setq v1 (skeleton-read "Your conf page url: ")) \n
  "#+CONF_ID: " (org-conf--return-id-from-url v1) \n
  )
;;; ======================================================================


;;; ======================================================================
;;;                            Templates                             
;;; ======================================================================
(defun org-conf-template (contents info)
  (concat
"<h1>目录</h1>
<ac:structured-macro ac:name=\"toc\">
  <ac:parameter ac:name=\"outline\">true</ac:parameter>
</ac:structured-macro>\n"
   contents))

(defun org-conf-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
    CONTENTS holds the contents of the headline.  INFO is a plist
    holding contextual information."
  (let* ((level (+ (org-export-get-relative-level headline info)))
         (text (org-export-data (org-element-property :title headline) info))
         (id (org-html--reference headline info))
         (contents (or contents "")))
    (format "%s%s\n"
            (format "\n<h%d>%s</h%d>\n"
                    level
                    text
                    level)
            contents)))

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
"<ac:structured-macro ac:name=\"mathblock\">
  <ac:parameter ac:name=\"alignment\">left</ac:parameter>
  <ac:plain-text-body>
    <![CDATA[
%s
    ]]>
  </ac:plain-text-body>
</ac:structured-macro>"
  ;; Remove guiding '\(' and trailing '\)'.
  (replace-regexp-in-string "\\\\(\\|\\\\)" "" data)))

(defun org-conf-latex-environment-filter (data backend info)
  "Function applied to a transcoded latex-environment."
  (format
"<ac:structured-macro ac:name=\"mathblock\">
  <ac:parameter ac:name=\"alignment\">center</ac:parameter>
  <ac:plain-text-body>
    <![CDATA[
%s
    ]]>
  </ac:plain-text-body>
</ac:structured-macro>"
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
         (text (car code-info)))
    (if (not title) (setq title "[Example]"))
    (format
"<ac:structured-macro ac:name=\"panel\">
  <ac:parameter ac:name=\"borderColor\">grey</ac:parameter>
  <ac:parameter ac:name=\"borderStyle\">dashed</ac:parameter>
  <ac:parameter ac:name=\"title\">%s</ac:parameter>
  <ac:rich-text-body>
    <pre>%s</pre>
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
     )))

(defun org-conf-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to HTML.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* (
         (lang (org-element-property :language src-block))
         ;; Extract code and references.
         (code-info (org-export-unravel-code src-block))
         (code (car code-info))
         (num-start 1)
         (pars (org-babel-parse-header-arguments (org-element-property :parameters src-block)))
         (host (cdr (assoc :host pars)))
         (path (cdr (assoc :path pars)))
         (collapse (cdr (assoc :collapse pars)))
         (title (car (org-export-get-caption src-block)))
         (env-comment "")
         )
    ;; Preprocess some parameters.
    (if (string-match "^%" host)
        (setq host nil))
    (if (string-match "^%" path)
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
     ;; Image file.
     ((and (plist-get info :html-inline-images)
           (org-export-inline-image-p
            link (plist-get info :html-inline-image-rules)))
      (if (plist-get info :org-conf-do-post)
          (org-conf-img-link-upload
           (plist-get info :org-conf-user)
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
;;;                          Request Methods                             
;;; ======================================================================
(defun org-conf-rest-get-page-json (user conf-id)
  "Retrieve a Confluence page's status information using GET method."
  (request-response-data
    (request
      (org-conf--return-api-url (format "/rest/api/content/%s?&expand=version.number" conf-id))
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

(defun org-conf-rest-put (user conf-id title conf-space body version ver-msg)
  (let* ((request-log-level ox-conf-log-level)
         (request-message-level ox-conf-log-level))
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

(defun org-conf-rest-get-attach-json (user conf-id)
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
                  (when data (message "Got Attachments!"))))
      :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                            (message "Got error: %S" error-thrown)))
      )))

(defun org-conf-rest-post-file (user conf-id filepath &optional attach-id)
  (let* ((request-log-level ox-conf-log-level)
         (request-message-level ox-conf-log-level))
    (request-response-data
     (request
       (if attach-id
           (org-conf--return-api-url (format "/rest/api/content/%s/child/attachment/%s/data" conf-id attach-id))
         (org-conf--return-api-url (format "/rest/api/content/%s/child/attachment" conf-id))
         )
       :auth "basic"
       :type "POST"
       :files `(("file" . ,(expand-file-name filepath)))
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
;;;                          Json Parsers
;;; ======================================================================
(defun org-conf-page-json-parser (page-json)
  (message (format "===> Title is: %s, version is: %s"
                   (cdr (assoc 'title page-json))
                   (cdr (assoc 'number (assoc 'version page-json))))))

(defun org-conf-attachs-json-parser (attachs-json &optional filename)
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

(defun org-conf-img-link-upload (user conf-id link)
  (let* ((attach-json (org-conf-rest-get-attach-json user conf-id))
         (attach-id (org-conf-attachs-json-parser
                     attach-json (file-name-nondirectory link))))
    (org-conf-rest-post-file user conf-id link attach-id)))


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
        (progn
          (setq res (org-conf-rest-get-page-json user conf-id))
          (org-conf-page-json-parser res))
      (message "No CONF_ID found!"))))

(defun org-conf-export-img-info
    (&optional async subtreep visible-only body-only ext-plist)
  "Export message about current article's status."
  (interactive)
  ;; Option "conf-id" cannot be retrieved after org-export-to-buffer!
  ;; So put it above exporting.
  (let ((conf-id (plist-get (org-export--get-inbuffer-options 'org-conf-html) :conf-id))
        (user (car (plist-get (org-export--get-inbuffer-options 'org-conf-html) :author)))
        (res nil))
    (if conf-id
        (progn
          (setq res (org-conf-rest-get-attach-json user conf-id))
          (org-conf-attachs-json-parser res))
      (message "No CONF_ID found!"))))

(defun org-conf-export-to-conf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export to Confluence by calling `org-conf-rest-put function."
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
          (setq res (org-conf-rest-get-page-json user conf-id))
          (setq version (1+ (cdr (assoc 'number (assoc 'version res)))))
          ;; Pass export parameters through communication channel.
          (setq org-conf-plist
                (org-combine-plists
                 ext-plist
                 '(:org-conf-do-post t)
                 `(:org-conf-user ,user)
                 `(:org-conf-id ,conf-id)))
          (setq output (org-export-as
                        'org-conf-html subtreep visible-only body-only org-conf-plist))
          ;; Update the page!
          (setq ver-msg (read-string "Version message: "))
          (org-conf-rest-put user conf-id title conf-space output version ver-msg))
      (message "No CONF_ID found!"))))

(defun org-conf-export-to-conf-without-post
    (&optional async subtreep visible-only body-only ext-plist)
  "Export to Confluence by calling `org-conf-rest-put function."
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
          (setq res (org-conf-rest-get-page-json user conf-id))
          (setq version (1+ (cdr (assoc 'number (assoc 'version res)))))
          ;; Pass export parameters through communication channel.
          (setq org-conf-plist
                (org-combine-plists
                 ext-plist
                 '(:org-conf-do-post nil)
                 `(:org-conf-user ,user)
                 `(:org-conf-id ,conf-id)))
          (setq output (org-export-as
                        'org-conf-html subtreep visible-only body-only org-conf-plist))
          ;; Update the page!
          (setq ver-msg (read-string "Version message: "))
          (org-conf-rest-put user conf-id title conf-space output version ver-msg))
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
  "Don't worry, let's retry the password... and forget the old one."
  (interactive)
  (auth-source-forget-all-cached)
  (message "Password cache has been forgotten!")
  )


;;; ======================================================================
;;;                          Export Backend                             
;;; ======================================================================
(org-export-define-derived-backend 'org-conf-html 'html
  :translate-alist
  '(
    (template . org-conf-template)
    (headline . org-conf-headline)
    (section . org-conf-section)
    (center-block . org-conf-center-block)
    (example-block . org-conf-example-block)
    (src-block . org-conf-src-block)
    (special-block . org-conf-special-block)
    (link . org-conf-link)
    (target . org-conf-target)
    )
  :filters-alist
  '(
    (:filter-latex-fragment . org-conf-latex-fragment-filter)
    (:filter-latex-environment . org-conf-latex-environment-filter)
    (:filter-export-block . org-conf-export-block-filter)
    )
  :options-alist
  '(
    (:with-toc nil)
    (:html-text-markup-alist nil org-conf-text-markup-alist nil)
    (:conf-id "CONF_ID" nil nil t)
    (:conf-space "CONF_SPACE" nil org-conf-default-space t)
    )
  :menu-entry
  '(?f "Export to Confluence"
     ((?b "To buffer" org-conf-export-to-buffer)
      (?p "Page info" org-conf-export-page-info)
      (?i "Image info" org-conf-export-img-info)
      (?f "To Confluence" org-conf-export-to-conf)
      (?w "To Confluence (w/o POST)" org-conf-export-to-conf-without-post)
      (?r "Forget the wrong password" org-conf-forget-password)
      ))
  )

(provide 'ox-conf)
;;; ox-conf.el ends here
