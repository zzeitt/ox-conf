;;; ox-conf-test.el --- ERT tests for ox-conf.el -*- lexical-binding: t; -*-

;; Run with: emacs -Q --batch -L test -l test/run-tests.el

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; ----------------------------------------------------------------------
;;; org-conf--page-path: build a "Root / Mid / Leaf" label for a node.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-page-path-builds-full-ancestor-chain ()
  "A leaf node renders its whole ancestor chain, joined by \" / \"."
  (let ((table (make-hash-table :test 'equal)))
    (puthash "1" '(:id "1" :title "Root" :parent-id nil) table)
    (puthash "2" '(:id "2" :title "Mid" :parent-id "1") table)
    (puthash "3" '(:id "3" :title "Child" :parent-id "2") table)
    (should (equal "Root / Mid / Child"
                   (org-conf--page-path (gethash "3" table) table)))))

(ert-deftest ox-conf-test-page-path-of-root-is-just-its-title ()
  "A node with no parent renders as a bare title."
  (let ((table (make-hash-table :test 'equal)))
    (puthash "1" '(:id "1" :title "Root" :parent-id nil) table)
    (should (equal "Root"
                   (org-conf--page-path (gethash "1" table) table)))))

(ert-deftest ox-conf-test-page-path-tolerates-missing-parent ()
  "A parent id that is absent from TABLE does not crash the chain walk."
  (let ((table (make-hash-table :test 'equal)))
    (puthash "3" '(:id "3" :title "Orphan" :parent-id "999") table)
    (should (equal "Orphan"
                   (org-conf--page-path (gethash "3" table) table)))))

;;; ----------------------------------------------------------------------
;;; org-conf--sort-parent-candidates: frequency first, then path.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-sort-candidates-puts-most-used-first ()
  "A candidate used more often outranks one whose path sorts earlier."
  (let ((candidates '((:id "1" :title "A" :path "Zeta")
                      (:id "2" :title "B" :path "Alpha")))
        (freq '(("1" . 3) ("2" . 7))))
    (should (equal '("2" "1")
                   (mapcar (lambda (c) (plist-get c :id))
                           (org-conf--sort-parent-candidates candidates freq))))))

(ert-deftest ox-conf-test-sort-candidates-falls-back-to-path-order ()
  "With equal frequency, candidates are ordered by path."
  (let ((candidates '((:id "1" :title "A" :path "Zeta")
                      (:id "2" :title "B" :path "Alpha")))
        (freq nil))
    (should (equal '("2" "1")
                   (mapcar (lambda (c) (plist-get c :id))
                           (org-conf--sort-parent-candidates candidates freq))))))

(ert-deftest ox-conf-test-sort-candidates-treats-unknown-id-as-unused ()
  "A candidate absent from FREQ counts as zero, so a known one wins."
  (let ((candidates '((:id "1" :title "A" :path "Alpha")
                      (:id "2" :title "B" :path "Zeta")))
        (freq '(("2" . 1))))
    (should (equal '("2" "1")
                   (mapcar (lambda (c) (plist-get c :id))
                           (org-conf--sort-parent-candidates candidates freq))))))

;;; ----------------------------------------------------------------------
;;; org-conf--collect-descendants: BFS over injected child fetcher.
;;; ----------------------------------------------------------------------

(defconst ox-conf-test--tree
  '(("1" (:id "2" :title "Alpha") (:id "3" :title "Beta"))
    ("2" (:id "4" :title "Alpha-1"))
    ("3")
    ("4" (:id "5" :title "Deep")))
  "Fake page tree, as an alist of (PARENT-ID . CHILDREN).")

(defun ox-conf-test--fetcher (tree)
  "Return a fetch function backed by TREE, an alist of (ID . CHILDREN)."
  (lambda (id) (cdr (assoc id tree))))

(ert-deftest ox-conf-test-collect-descendants-includes-root-first ()
  "The root page heads the candidate list and carries its own path."
  (let* ((root '(:id "1" :title "Root" :parent-id nil))
         (nodes (org-conf--collect-descendants
                 root (ox-conf-test--fetcher ox-conf-test--tree) 500)))
    (should (equal '("1" "2" "3" "4" "5")
                   (mapcar (lambda (n) (plist-get n :id)) nodes)))
    (should (equal "Root" (plist-get (car nodes) :path)))))

(ert-deftest ox-conf-test-collect-descendants-builds-paths-and-parents ()
  "Each descendant knows its parent id and its full title path."
  (let* ((root '(:id "1" :title "Root" :parent-id nil))
         (nodes (org-conf--collect-descendants
                 root (ox-conf-test--fetcher ox-conf-test--tree) 500))
         (deep (seq-find (lambda (n) (equal "5" (plist-get n :id))) nodes))
         (mid (seq-find (lambda (n) (equal "4" (plist-get n :id))) nodes)))
    (should (equal "2" (plist-get mid :parent-id)))
    (should (equal "Root / Alpha / Alpha-1 / Deep" (plist-get deep :path)))))

(ert-deftest ox-conf-test-collect-descendants-stops-at-max-nodes ()
  "MAX-NODES truncates the walk instead of erroring."
  (let* ((root '(:id "1" :title "Root" :parent-id nil))
         (nodes (org-conf--collect-descendants
                 root (ox-conf-test--fetcher ox-conf-test--tree) 3)))
    (should (equal '("1" "2" "3")
                   (mapcar (lambda (n) (plist-get n :id)) nodes)))))

(ert-deftest ox-conf-test-collect-descendants-handles-childless-root ()
  "A root with no children yields just itself."
  (let* ((root '(:id "99" :title "Lonely" :parent-id nil))
         (nodes (org-conf--collect-descendants
                 root (ox-conf-test--fetcher ox-conf-test--tree) 500)))
    (should (equal '("99") (mapcar (lambda (n) (plist-get n :id)) nodes)))))

;;; ----------------------------------------------------------------------
;;; JSON parsers for the create-page flow.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-parse-child-page-json-extracts-id-and-title ()
  "A child/page response becomes a list of (:id :title) plists."
  (let ((json '((results . [((id . "2") (title . "Alpha"))
                            ((id . "3") (title . "Beta"))]))))
    (should (equal '((:id "2" :title "Alpha") (:id "3" :title "Beta"))
                   (org-conf--parse-child-page-json json)))))

(ert-deftest ox-conf-test-parse-child-page-json-handles-no-results ()
  "An empty or results-less response yields nil rather than an error."
  (should (null (org-conf--parse-child-page-json '((results . [])))))
  (should (null (org-conf--parse-child-page-json '()))))

(ert-deftest ox-conf-test-parse-space-root-json-reads-grouped-page-results ()
  "The space-content endpoint groups results under a content-type key."
  (let ((json '((page . ((results . [((id . "1") (title . "Root"))])))
                (blogpost . ((results . []))))))
    (should (equal '((:id "1" :title "Root"))
                   (org-conf--parse-child-page-json json)))))

(ert-deftest ox-conf-test-parse-space-root-json-handles-an-empty-page-group ()
  (should (null (org-conf--parse-child-page-json '((page . ((results . []))))))))

(ert-deftest ox-conf-test-fetch-all-pages-follows-a-grouped-next-link ()
  (let* ((first '((page . ((results . [((id . "1") (title . "A"))])
                           (_links . ((next . "/page/2")))))))
         (second '((page . ((results . [((id . "2") (title . "B"))])))))
         (calls nil)
         (get-json (lambda (api)
                     (push api calls)
                     (if (equal api "/page/2") second first))))
    (should (equal '((:id "1" :title "A") (:id "2" :title "B"))
                   (org-conf--fetch-all-pages "/page/1" get-json)))
    (should (equal '("/page/1" "/page/2") (nreverse calls)))))

(ert-deftest ox-conf-test-parse-created-page-extracts-id-and-webui ()
  "A POST response becomes a plist with the new page's id and web path."
  (let ((json '((id . "123")
                (title . "My Page")
                (_links . ((webui . "/spaces/DOCS/pages/123/My+Page"))))))
    (should (equal '(:id "123" :webui "/spaces/DOCS/pages/123/My+Page")
                   (org-conf--parse-created-page json)))))

;;; ----------------------------------------------------------------------
;;; org-conf--post-page-body: the POST /rest/api/content payload.
;;; Assertions decode the encoded JSON, because that is what the API eats.
;;; ----------------------------------------------------------------------

(defun ox-conf-test--decode-body (title space parent-id)
  "Build a create-page body and decode its JSON back to lisp."
  (json-read-from-string (json-encode (org-conf--post-page-body title space parent-id))))

(ert-deftest ox-conf-test-post-page-body-encodes-ancestors-as-array ()
  "Confluence wants ancestors as a JSON array, not a bare object."
  (let* ((decoded (ox-conf-test--decode-body "T" "DOCS" "123"))
         (ancestors (cdr (assoc 'ancestors decoded))))
    (should (vectorp ancestors))
    (should (equal "123" (cdr (assoc 'id (aref ancestors 0)))))))

(ert-deftest ox-conf-test-post-page-body-omits-ancestors-without-parent ()
  "No parent page means no ancestors key at all."
  (let ((decoded (ox-conf-test--decode-body "T" "DOCS" nil)))
    (should (null (assoc 'ancestors decoded)))))

(ert-deftest ox-conf-test-post-page-body-encodes-type-space-and-empty-body ()
  "The payload declares a page, its space key, and an empty storage body."
  (let* ((decoded (ox-conf-test--decode-body "My Page" "DOCS" nil))
         (storage (cdr (assoc 'storage (cdr (assoc 'body decoded))))))
    (should (equal "page" (cdr (assoc 'type decoded))))
    (should (equal "My Page" (cdr (assoc 'title decoded))))
    (should (equal "DOCS" (cdr (assoc 'key (cdr (assoc 'space decoded))))))
    (should (equal "storage" (cdr (assoc 'representation storage))))
    (should (equal "" (cdr (assoc 'value storage))))))

;;; ----------------------------------------------------------------------
;;; Parent-page usage frequency: bump, load and save.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-freq-bump-increments-an-existing-id ()
  (should (equal '(("1" . 3) ("2" . 1))
                 (org-conf--freq-bump "1" '(("1" . 2) ("2" . 1))))))

(ert-deftest ox-conf-test-freq-bump-adds-an-unseen-id-with-count-one ()
  (should (equal '(("9" . 1) ("1" . 2))
                 (org-conf--freq-bump "9" '(("1" . 2))))))

(ert-deftest ox-conf-test-freq-bump-leaves-its-input-alone ()
  "The caller may hold on to FREQ, so bumping must not mutate it."
  (let ((freq '(("1" . 2))))
    (org-conf--freq-bump "1" freq)
    (should (equal '(("1" . 2)) freq))))

(ert-deftest ox-conf-test-freq-save-and-load-round-trip ()
  (let ((file (make-temp-file "ox-conf-freq" nil ".eld")))
    (unwind-protect
        (progn
          (org-conf--freq-save file '(("1" . 3) ("2" . 1)))
          (should (equal '(("1" . 3) ("2" . 1))
                         (org-conf--freq-load file))))
      (delete-file file))))

(ert-deftest ox-conf-test-freq-save-creates-missing-parent-directory ()
  "The frequency file is configurable, so its directory may not exist yet."
  (let* ((dir (make-temp-name
               (expand-file-name "ox-conf-freq-dir-" temporary-file-directory)))
         (file (expand-file-name "nested/usage.eld" dir)))
    (unwind-protect
        (progn
          (org-conf--freq-save file '(("1" . 1)))
          (should (equal '(("1" . 1)) (org-conf--freq-load file))))
      (delete-directory dir t))))

(ert-deftest ox-conf-test-freq-load-missing-file-is-nil ()
  (let ((missing (make-temp-name
                  (expand-file-name "ox-conf-missing-" temporary-file-directory))))
    (should (null (org-conf--freq-load missing)))))

(ert-deftest ox-conf-test-freq-load-ignores-garbage ()
  "A hand-edited or truncated file must not break the export flow."
  (let ((file (make-temp-file "ox-conf-freq" nil ".eld")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "this is not ((an) (alist))"))
          (should (null (org-conf--freq-load file))))
      (delete-file file))))

;;; ----------------------------------------------------------------------
;;; org-conf--insert-conf-keywords: record the new page in the org buffer.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-insert-keywords-lands-below-the-title ()
  "The block joins the front matter; #+TITLE: keeps the first line."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: My Doc\n\nBody\n")
    (org-conf--insert-conf-keywords "DOCS" "http://x/pages/1/T" "1")
    (should (equal (concat "#+TITLE: My Doc\n"
                           "#+CONF_SPACE: DOCS\n"
                           "#+CONF_PAGE: http://x/pages/1/T\n"
                           "#+CONF_ID: 1\n"
                           "\nBody\n")
                   (buffer-string)))))

(ert-deftest ox-conf-test-insert-keywords-follows-the-whole-front-matter ()
  "Options and other keywords stay above the CONF block."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: My Doc\n#+AUTHOR: Me\n#+OPTIONS: ^:{}\n\nBody\n")
    (org-conf--insert-conf-keywords "DOCS" "http://x/pages/1/T" "1")
    (should (equal (concat "#+TITLE: My Doc\n"
                           "#+AUTHOR: Me\n"
                           "#+OPTIONS: ^:{}\n"
                           "#+CONF_SPACE: DOCS\n"
                           "#+CONF_PAGE: http://x/pages/1/T\n"
                           "#+CONF_ID: 1\n"
                           "\nBody\n")
                   (buffer-string)))))

(ert-deftest ox-conf-test-insert-keywords-hoists-a-stale-block-below-the-title ()
  "A file written by the old code has CONF_* above #+TITLE:; normalise it."
  (with-temp-buffer
    (org-mode)
    (insert "#+CONF_ID: 1\n#+CONF_SPACE: OLD\n#+TITLE: My Doc\n\nBody\n")
    (org-conf--insert-conf-keywords "DOCS" "http://x/pages/1/T" "1")
    (should (equal (concat "#+TITLE: My Doc\n"
                           "#+CONF_SPACE: DOCS\n"
                           "#+CONF_PAGE: http://x/pages/1/T\n"
                           "#+CONF_ID: 1\n"
                           "\nBody\n")
                   (buffer-string)))))

(ert-deftest ox-conf-test-insert-keywords-replaces-stale-ones ()
  "A half-filled file must not end up with duplicate CONF_SPACE lines."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: My Doc\n#+CONF_SPACE: OLD\n\nBody\n")
    (org-conf--insert-conf-keywords "DOCS" "http://x/pages/1/T" "1")
    (should (equal (concat "#+TITLE: My Doc\n"
                           "#+CONF_SPACE: DOCS\n"
                           "#+CONF_PAGE: http://x/pages/1/T\n"
                           "#+CONF_ID: 1\n"
                           "\nBody\n")
                   (buffer-string)))))

(ert-deftest ox-conf-test-insert-keywords-still-works-without-front-matter ()
  "A file that starts with body text has nowhere to go but the top."
  (with-temp-buffer
    (org-mode)
    (insert "Body\n")
    (org-conf--insert-conf-keywords "DOCS" "http://x/pages/1/T" "1")
    (should (equal (concat "#+CONF_SPACE: DOCS\n"
                           "#+CONF_PAGE: http://x/pages/1/T\n"
                           "#+CONF_ID: 1\n"
                           "Body\n")
                   (buffer-string)))))

(ert-deftest ox-conf-test-inserted-keywords-are-readable-by-org ()
  "The whole point: org export must see the new page's id and space."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: My Doc\n")
    (org-conf--insert-conf-keywords "DOCS" "http://x/pages/1/T" "1")
    (let ((options (org-export--get-inbuffer-options 'org-conf-html)))
      (should (equal "1" (plist-get options :conf-id)))
      (should (equal "DOCS" (plist-get options :conf-space)))
      (should (equal "http://x/pages/1/T" (plist-get options :conf-page))))))

(ert-deftest ox-conf-test-insert-keywords-lands-below-a-mode-line ()
  (with-temp-buffer
    (org-mode)
    (insert "# -*- mode: org; -*-\n#+TITLE: My Doc\n\nBody\n")
    (org-conf--insert-conf-keywords "DOCS" "http://x/pages/1/T" "1")
    (should (equal (concat "# -*- mode: org; -*-\n"
                           "#+TITLE: My Doc\n"
                           "#+CONF_SPACE: DOCS\n"
                           "#+CONF_PAGE: http://x/pages/1/T\n"
                           "#+CONF_ID: 1\n"
                           "\nBody\n")
                   (buffer-string)))))

(ert-deftest ox-conf-test-insert-keywords-never-goes-above-a-shebang ()
  "Without keywords, the block still stays below the shebang."
  (with-temp-buffer
    (org-mode)
    (insert "#!/usr/bin/env org\n\nBody\n")
    (org-conf--insert-conf-keywords "DOCS" "http://x/pages/1/T" "1")
    (should (equal (concat "#!/usr/bin/env org\n"
                           "\n"
                           "#+CONF_SPACE: DOCS\n"
                           "#+CONF_PAGE: http://x/pages/1/T\n"
                           "#+CONF_ID: 1\n"
                           "Body\n")
                   (buffer-string)))))

(ert-deftest ox-conf-test-insert-keywords-normalises-stale-lines-below-a-mode-line ()
  (with-temp-buffer
    (org-mode)
    (insert "# -*- mode: org; -*-\n#+CONF_ID: 1\n#+TITLE: T\n\nBody\n")
    (org-conf--insert-conf-keywords "DOCS" "http://x/pages/1/T" "1")
    (should (equal (concat "# -*- mode: org; -*-\n"
                           "#+TITLE: T\n"
                           "#+CONF_SPACE: DOCS\n"
                           "#+CONF_PAGE: http://x/pages/1/T\n"
                           "#+CONF_ID: 1\n"
                           "\nBody\n")
                   (buffer-string)))))

;;; ----------------------------------------------------------------------
;;; org-conf--parent-choices: the completing-read collection.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-parent-choices-head-with-space-root ()
  "The \"no parent\" option comes first and carries a nil id."
  (let ((candidates '((:id "1" :title "Root" :path "Root")
                      (:id "2" :title "Child" :path "Root / Child"))))
    (should (equal `((,org-conf-parent-root-label . nil)
                     ("Root" . "1")
                     ("Root / Child" . "2"))
                   (org-conf--parent-choices candidates)))))

(ert-deftest ox-conf-test-parent-choices-disambiguate-same-path-pages ()
  "Two sibling pages with the same title must stay individually pickable."
  (let ((candidates '((:id "2" :title "Notes" :path "Root / Notes")
                      (:id "3" :title "Notes" :path "Root / Notes"))))
    (should (equal '("Root / Notes [2]" "Root / Notes [3]")
                   (mapcar #'car (cdr (org-conf--parent-choices candidates)))))))

(ert-deftest ox-conf-test-parent-choices-leave-unique-paths-bare ()
  "Only genuinely ambiguous paths get an id suffix."
  (let ((candidates '((:id "2" :title "Notes" :path "Root / Notes")
                      (:id "3" :title "Other" :path "Root / Other"))))
    (should (equal '("Root / Notes" "Root / Other")
                   (mapcar #'car (cdr (org-conf--parent-choices candidates)))))))

;;; ----------------------------------------------------------------------
;;; Small decisions: where f f goes, what to call the page, its url.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-export-action-requires-a-conf-id-to-update ()
  (should (eq 'update (org-conf--export-action "123")))
  (should (eq 'create (org-conf--export-action nil))))

(ert-deftest ox-conf-test-default-title-prefers-the-org-title ()
  (should (equal "My Doc"
                 (org-conf--default-title "My Doc" "/tmp/notes.org"))))

(ert-deftest ox-conf-test-default-title-ignores-a-blank-org-title ()
  "An empty #+TITLE: is as good as no title at all."
  (should (equal "notes"
                 (org-conf--default-title "   " "/tmp/notes.org"))))

(ert-deftest ox-conf-test-default-title-falls-back-to-the-file-name ()
  (should (equal "notes" (org-conf--default-title nil "/tmp/notes.org"))))

(ert-deftest ox-conf-test-default-title-is-empty-when-nothing-is-known ()
  (should (equal "" (org-conf--default-title nil nil))))

(ert-deftest ox-conf-test-page-url-joins-host-and-webui ()
  (let ((org-conf-url "http://wiki.example.com/"))
    (should (equal "http://wiki.example.com/spaces/DOCS/pages/1/T"
                   (org-conf--page-url "/spaces/DOCS/pages/1/T")))))

(ert-deftest ox-conf-test-page-url-of-nothing-is-empty ()
  (let ((org-conf-url "http://wiki.example.com"))
    (should (equal "" (org-conf--page-url nil)))))

(ert-deftest ox-conf-test-page-url-passes-an-absolute-url-through ()
  (let ((org-conf-url "http://wiki.example.com/"))
    (should (equal "https://other.example.com/pages/1"
                   (org-conf--page-url "https://other.example.com/pages/1")))))

;;; ----------------------------------------------------------------------
;;; Backend selection and the REST adapter.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-active-backend-honours-an-explicit-choice ()
  (let ((org-conf-backend 'rest)) (should (eq 'rest (org-conf--active-backend))))
  (let ((org-conf-backend 'cli)) (should (eq 'cli (org-conf--active-backend)))))

(ert-deftest ox-conf-test-active-backend-auto-prefers-the-cli ()
  (let ((org-conf-backend 'auto))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program) "/usr/bin/confluence")))
      (should (eq 'cli (org-conf--active-backend))))))

(ert-deftest ox-conf-test-active-backend-auto-falls-back-to-rest ()
  (let ((org-conf-backend 'auto))
    (cl-letf (((symbol-function 'executable-find) (lambda (_program) nil)))
      (should (eq 'rest (org-conf--active-backend))))))

(ert-deftest ox-conf-test-rest-get-page-info-builds-a-plist ()
  (let ((json '((id . "7")
                (title . "Doc")
                (version . ((number . 3)))
                (space . ((key . "DOCS")))
                (ancestors . [((id . "1") (title . "Root"))
                              ((id . "5") (title . "Parent"))])
                (_links . ((webui . "/spaces/DOCS/pages/7/Doc"))))))
    (cl-letf (((symbol-function 'org-conf-rest-get-page-json)
               (lambda (_id) json)))
      (let ((org-conf-backend 'rest)
            (org-conf-url "http://wiki.example.com/"))
        (should (equal '(:id "7" :title "Doc" :version 3 :space "DOCS"
                         :parent-id "5"
                         :url "http://wiki.example.com/spaces/DOCS/pages/7/Doc")
                       (org-conf-backend-get-page-info "7")))))))

(ert-deftest ox-conf-test-rest-list-attachments-builds-id-title-plists ()
  (let ((json '(:results [(:id "a1" :title "a.png")
                          (:id "a2" :title "b.png")])))
    (cl-letf (((symbol-function 'org-conf-rest-get-attach-json)
               (lambda (_id) json)))
      (let ((org-conf-backend 'rest))
        (should (equal '((:id "a1" :title "a.png")
                         (:id "a2" :title "b.png"))
                       (org-conf-backend-list-attachments "7")))))))

;;; ----------------------------------------------------------------------
;;; The CLI process layer: arguments, JSON, errors, temp bodies.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-cli-json-prefixes-json-and-parses ()
  (let ((captured nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program) "/usr/bin/confluence"))
              ((symbol-function 'call-process)
               (lambda (_program _infile dest _display &rest args)
                 (setq captured args)
                 (with-current-buffer (if (consp dest) (car dest) dest)
                   (insert "{\"id\": \"7\", \"title\": \"Doc\"}"))
                 0)))
      (should (equal '((id . "7") (title . "Doc"))
                     (org-conf--cli-json "info" "7")))
      (should (equal '("--json" "info" "7") captured)))))

(ert-deftest ox-conf-test-cli-json-reports-a-structured-error ()
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_program) "/usr/bin/confluence"))
            ((symbol-function 'call-process)
             (lambda (_program _infile dest _display &rest _)
               (let ((stderr (nth 1 dest)))
                 (when (stringp stderr)
                   (with-temp-file stderr
                     (insert "{\"error\":\"boom\",\"code\":\"API_ERROR\"}"))))
               1)))
    (let ((err (condition-case e
                   (progn (org-conf--cli-json "info" "7") nil)
                 (user-error (error-message-string e)))))
      (should (string-match-p "boom" err)))))

(ert-deftest ox-conf-test-cli-json-rejects-invalid-json ()
  (cl-letf (((symbol-function 'executable-find)
             (lambda (_program) "/usr/bin/confluence"))
            ((symbol-function 'call-process)
             (lambda (_program _infile dest _display &rest _)
               (with-current-buffer (if (consp dest) (car dest) dest)
                 (insert "not json"))
               0)))
    (should-error (org-conf--cli-json "info" "7") :type 'user-error)))

(ert-deftest ox-conf-test-with-temp-body-writes-utf8-and-deletes ()
  (let (path)
    (org-conf--with-temp-body
        "中文 body"
      (lambda (file)
        (setq path file)
        (should (equal "中文 body"
                       (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string))))))
    (should-not (file-exists-p path))))

;;; ----------------------------------------------------------------------
;;; org-conf--json-link: pull a rel out of a JSON _links object.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-json-link-reads-a-rel ()
  (let ((json '((results . [])
                (_links . ((next . "/rest/api/content/1/child/page?limit=200&start=200"))))))
    (should (equal "/rest/api/content/1/child/page?limit=200&start=200"
                   (org-conf--json-link json 'next)))))

(ert-deftest ox-conf-test-json-link-is-nil-when-absent ()
  "The last page of a listing has no next link."
  (should (null (org-conf--json-link '((results . [])) 'next))))

;;; ----------------------------------------------------------------------
;;; Paged listing: the API urls, and the generic next-link walker.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-child-page-api-asks-for-children-at-the-page-limit ()
  (let ((org-conf-parent-child-page-limit 50))
    (should (equal "/rest/api/content/123/child/page?limit=50"
                   (org-conf--child-page-api "123")))))

(ert-deftest ox-conf-test-space-root-page-api-asks-for-roots-only ()
  "depth=root keeps the space listing to top-level pages."
  (let ((org-conf-parent-child-page-limit 50))
    (should (equal "/rest/api/space/DOCS/content?depth=root&limit=50"
                   (org-conf--space-root-page-api "DOCS")))))

(ert-deftest ox-conf-test-fetch-all-pages-follows-next-links ()
  "Results spread over two API pages come back as one flat list."
  (let* ((first '((results . [((id . "2") (title . "A"))])
                  (_links . ((next . "/page/2")))))
         (second '((results . [((id . "3") (title . "B"))])))
         (calls nil)
         (get-json (lambda (api)
                     (push api calls)
                     (if (equal api "/page/2") second first))))
    (should (equal '((:id "2" :title "A") (:id "3" :title "B"))
                   (org-conf--fetch-all-pages "/page/1" get-json)))
    (should (equal '("/page/1" "/page/2") (nreverse calls)))))

(ert-deftest ox-conf-test-fetch-all-pages-stops-on-endless-next-links ()
  "A server that always advertises a next page must not loop forever."
  (let* ((calls 0)
         (get-json (lambda (_api)
                     (setq calls (1+ calls))
                     '((results . [])
                       (_links . ((next . "/more")))))))
    (org-conf--fetch-all-pages "/page/1" get-json)
    (should (<= calls org-conf--max-child-page-requests))))

(ert-deftest ox-conf-test-fetch-all-pages-tolerates-a-failed-first-page ()
  "A nil response means the request failed; report nothing."
  (should (null (org-conf--fetch-all-pages "/page/1" (lambda (_api) nil)))))

(ert-deftest ox-conf-test-fetch-all-children-targets-the-child-page-api ()
  (let* ((calls nil)
         (get-json (lambda (api) (push api calls) '((results . []))))
         (org-conf-parent-child-page-limit 200))
    (org-conf--fetch-all-children "1" get-json)
    (should (equal '("/rest/api/content/1/child/page?limit=200") calls))))

(ert-deftest ox-conf-test-fetch-all-space-roots-targets-the-space-api ()
  (let* ((calls nil)
         (get-json (lambda (api) (push api calls) '((results . []))))
         (org-conf-parent-child-page-limit 200))
    (org-conf--fetch-all-space-roots "DOCS" get-json)
    (should (equal '("/rest/api/space/DOCS/content?depth=root&limit=200")
                   calls))))

;;; ----------------------------------------------------------------------
;;; org-conf-create-page-and-export: the whole "f f" wiring.
;;; Prompts and HTTP are stubbed; everything between them is real.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-create-page-and-export-wires-everything ()
  "Creating posts the page, records it in the buffer, then exports."
  (let* ((org-conf-url "http://wiki.example.com/")
         (org-conf-root-parent-page "999")
         (org-conf-parent-freq-file (make-temp-file "ox-conf-freq" nil ".eld"))
         (posted nil)
         (put nil))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "#+TITLE: My Doc\n\nBody\n")
          (cl-letf (((symbol-function 'read-string)
                     (lambda (prompt &optional _initial &rest _)
                       (if (string-match-p "space" prompt) "DOCS" "My Doc")))
                    ((symbol-function 'completing-read)
                     ;; Pick the root page rather than "create at space root".
                     (lambda (_prompt collection &rest _) (caadr collection)))
                    ((symbol-function 'org-conf-backend-get-page-info)
                     (lambda (id)
                       (if (equal id "999")
                           (list :id id :title "Root" :version 0)
                         (list :id id :title "My Doc" :version 0))))
                    ((symbol-function 'org-conf-backend-get-children)
                     (lambda (_id) nil))
                    ((symbol-function 'org-conf-backend-create-page)
                     (lambda (title space parent-id)
                       (setq posted (list title space parent-id))
                       (list :id "555"
                             :webui "/spaces/DOCS/pages/555/My+Doc")))
                    ((symbol-function 'org-export-as)
                     (lambda (&rest _) "<p>body</p>"))
                    ((symbol-function 'org-conf-backend-update-page)
                     (lambda (&rest args) (setq put args))))
            (org-conf-create-page-and-export)
            (should (equal '("My Doc" "DOCS" "999") posted))
            (let ((options (org-export--get-inbuffer-options 'org-conf-html)))
              (should (equal "555" (plist-get options :conf-id)))
              (should (equal "DOCS" (plist-get options :conf-space)))
              (should (equal "http://wiki.example.com/spaces/DOCS/pages/555/My+Doc"
                             (plist-get options :conf-page))))
            ;; The page was finally filled in by the normal PUT flow.
            (should (equal "555" (nth 0 put)))
            (should (equal "<p>body</p>" (nth 3 put)))
            ;; Choosing a parent page bumps its usage count.
            (should (equal '(("999" . 1))
                           (org-conf--freq-load org-conf-parent-freq-file)))))
      (delete-file org-conf-parent-freq-file))))

;;; ----------------------------------------------------------------------
;;; org-conf--keyword-block-end: where the front matter ends.
;;; ----------------------------------------------------------------------

(defun ox-conf-test--after-line (n)
  "Position just after the newline ending line N, counted from point-min."
  (save-excursion
    (goto-char (point-min))
    (forward-line n)
    (point)))

(ert-deftest ox-conf-test-keyword-block-end-covers-the-leading-keywords ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+AUTHOR: A\n\nBody\n")
    (should (equal (ox-conf-test--after-line 2)
                   (org-conf--keyword-block-end)))))

(ert-deftest ox-conf-test-keyword-block-end-stops-at-a-blank-line ()
  "A blank line ends the front matter, so later keywords are not swept in."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n\n#+AUTHOR: A\n")
    (should (equal (ox-conf-test--after-line 1)
                   (org-conf--keyword-block-end)))))

(ert-deftest ox-conf-test-keyword-block-end-stops-at-body-text ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\nBody\n")
    (should (equal (ox-conf-test--after-line 1)
                   (org-conf--keyword-block-end)))))

(ert-deftest ox-conf-test-keyword-block-end-of-a-file-without-keywords ()
  (with-temp-buffer
    (org-mode)
    (insert "Body\n")
    (should (equal (point-min) (org-conf--keyword-block-end)))))

(ert-deftest ox-conf-test-keyword-block-end-of-an-empty-file ()
  (with-temp-buffer
    (org-mode)
    (should (equal (point-min) (org-conf--keyword-block-end)))))

(ert-deftest ox-conf-test-keyword-block-end-skips-a-mode-line ()
  (with-temp-buffer
    (org-mode)
    (insert "# -*- mode: org; -*-\n#+TITLE: T\n\nBody\n")
    (should (equal (ox-conf-test--after-line 2)
                   (org-conf--keyword-block-end)))))

(ert-deftest ox-conf-test-keyword-block-end-skips-blank-lines-before-keywords ()
  (with-temp-buffer
    (org-mode)
    (insert "# -*- mode: org; -*-\n\n#+TITLE: T\n#+AUTHOR: A\n\nBody\n")
    (should (equal (ox-conf-test--after-line 4)
                   (org-conf--keyword-block-end)))))

;;; ----------------------------------------------------------------------
;;; org-conf--unique-labels: keep every completing-read choice distinct.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-unique-labels-leave-distinct-labels-alone ()
  (should (equal '(("Alpha" . "1") ("Beta" . "2"))
                 (org-conf--unique-labels '(("Alpha" . "1") ("Beta" . "2"))))))

(ert-deftest ox-conf-test-unique-labels-suffix-collisions-with-the-id ()
  "Sibling pages often share a title; both must stay pickable."
  (should (equal '(("Notes [1]" . "1") ("Notes [2]" . "2"))
                 (org-conf--unique-labels '(("Notes" . "1") ("Notes" . "2"))))))

;;; ----------------------------------------------------------------------
;;; org-conf--drill-choices: one step of the walk down the page tree.
;;; Actions are lists: (root) (up) (manual) (confirm ID) (descend ID).
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-drill-choices-at-the-space-top ()
  "With no current page, offer the space root, the roots, and manual entry."
  (let ((children '((:id "1" :title "Alpha") (:id "2" :title "Beta"))))
    (should (equal (list (cons "（建在 space 根目录）" '(root))
                         '("Alpha" descend "1")
                         '("Beta" descend "2")
                         (cons "（手动输入页面 URL/ID）" '(manual)))
                   (org-conf--drill-choices nil children)))))

(ert-deftest ox-conf-test-drill-choices-below-the-top ()
  "Standing on a page, offer to confirm it, go up, descend, or type a url."
  (let ((current '(:id "1" :title "Alpha"))
        (children '((:id "3" :title "Alpha-1"))))
    (should (equal (list (cons "✓ 就用「Alpha」作为父页面" '(confirm "1"))
                         '("↑ 返回上一层" up)
                         '("Alpha-1" descend "3")
                         (cons "（手动输入页面 URL/ID）" '(manual)))
                   (org-conf--drill-choices current children)))))

(ert-deftest ox-conf-test-drill-choices-work-on-a-leaf ()
  "A page with no children can still be confirmed or left."
  (let ((current '(:id "3" :title "Leaf")))
    (should (equal (list (cons "✓ 就用「Leaf」作为父页面" '(confirm "3"))
                         '("↑ 返回上一层" up)
                         (cons "（手动输入页面 URL/ID）" '(manual)))
                   (org-conf--drill-choices current nil)))))

(ert-deftest ox-conf-test-drill-choices-disambiguate-sibling-titles ()
  "Two children with the same title must not collapse into one choice."
  (let ((children '((:id "7" :title "Notes") (:id "8" :title "Notes"))))
    (should (equal '("Notes [7]" "Notes [8]")
                   (mapcar #'car
                           (butlast (cdr (org-conf--drill-choices nil children))))))))

;;; ----------------------------------------------------------------------
;;; org-conf--read-parent-by-drill-down: the interactive walk.
;;; completing-read and the HTTP layer are stubbed.
;;; ----------------------------------------------------------------------

(defun ox-conf-test--drill-stub (answers)
  "Return a cons of a `completing-read' stub and a menu getter.
The stub replays ANSWERS one call at a time, and the getter returns the
list of menus that were shown, in order.  The getter is repeatable."
  (let ((pending (copy-sequence answers))
        (menus nil))
    (cons (lambda (_prompt collection &rest _)
            (push (mapcar #'car collection) menus)
            (or (pop pending)
                (error "drill-down asked for more input than the test supplied")))
          (lambda () (reverse menus)))))

(ert-deftest ox-conf-test-drill-down-walks-to-the-confirmed-page ()
  "Descending twice and confirming the leaf yields that leaf's id."
  (let* ((stub (ox-conf-test--drill-stub
                '("Alpha" "Alpha-1" "✓ 就用「Alpha-1」作为父页面")))
         (menus (cdr stub)))
    (cl-letf (((symbol-function 'completing-read) (car stub))
              ((symbol-function 'org-conf-backend-get-space-roots)
               (lambda (_space) '((:id "1" :title "Alpha"))))
              ((symbol-function 'org-conf-backend-get-children)
               (lambda (id) (cdr (assoc id '(("1" (:id "2" :title "Alpha-1"))))))))
      (should (equal "2" (org-conf--read-parent-by-drill-down "DOCS")))
      ;; Three menus: the space top, inside Alpha, then standing on the leaf.
      (should (equal '(("（建在 space 根目录）" "Alpha" "（手动输入页面 URL/ID）")
                       ("✓ 就用「Alpha」作为父页面" "↑ 返回上一层" "Alpha-1"
                        "（手动输入页面 URL/ID）")
                       ("✓ 就用「Alpha-1」作为父页面" "↑ 返回上一层"
                        "（手动输入页面 URL/ID）"))
                     (funcall menus))))))

(ert-deftest ox-conf-test-drill-down-can-stop-at-the-space-root ()
  (let ((stub (ox-conf-test--drill-stub '("（建在 space 根目录）"))))
    (cl-letf (((symbol-function 'completing-read) (car stub))
              ((symbol-function 'org-conf-backend-get-space-roots)
               (lambda (_space) '((:id "1" :title "Alpha")))))
      (should (null (org-conf--read-parent-by-drill-down "DOCS"))))))

(ert-deftest ox-conf-test-drill-down-can-climb-back-up ()
  "Going up returns to the previous level's menu, not to the top blindly."
  (let* ((stub (ox-conf-test--drill-stub
                '("Alpha" "↑ 返回上一层" "（建在 space 根目录）")))
         (menus (cdr stub)))
    (cl-letf (((symbol-function 'completing-read) (car stub))
              ((symbol-function 'org-conf-backend-get-space-roots)
               (lambda (_space) '((:id "1" :title "Alpha"))))
              ((symbol-function 'org-conf-backend-get-children)
               (lambda (_id) '((:id "2" :title "Alpha-1")))))
      (should (null (org-conf--read-parent-by-drill-down "DOCS")))
      (should (member "Alpha-1" (nth 1 (funcall menus))))
      (should (equal (nth 0 (funcall menus)) (nth 2 (funcall menus)))))))

(ert-deftest ox-conf-test-drill-down-falls-back-to-a-typed-url ()
  "The manual entry keeps working for pages outside the space tree."
  (let ((stub (ox-conf-test--drill-stub '("（手动输入页面 URL/ID）"))))
    (cl-letf (((symbol-function 'completing-read) (car stub))
              ((symbol-function 'read-string)
               (lambda (&rest _) "https://wiki.example.com/pages/777"))
              ((symbol-function 'org-conf-backend-get-space-roots)
               (lambda (_space) '((:id "1" :title "Alpha")))))
      (should (equal "777" (org-conf--read-parent-by-drill-down "DOCS"))))))

;;; ----------------------------------------------------------------------
;;; org-conf--read-parent-id: which source of parent pages is used.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-read-parent-id-walks-the-tree-when-no-root-is-set ()
  "With no `org-conf-root-parent-page', fall back to the space drill-down."
  (let ((org-conf-root-parent-page "")
        (asked nil))
    (cl-letf (((symbol-function 'org-conf--read-parent-by-drill-down)
               (lambda (space) (setq asked space) "42")))
      (should (equal "42" (org-conf--read-parent-id "DOCS")))
      (should (equal "DOCS" asked)))))

(ert-deftest ox-conf-test-read-parent-id-offers-manual-entry-when-root-is-set ()
  "The flat descendant list still has an escape hatch to a typed URL."
  (let ((org-conf-root-parent-page "999")
        (org-conf-parent-freq-file (make-temp-file "ox-conf-freq" nil ".eld"))
        (menus nil))
    (unwind-protect
        (cl-letf (((symbol-function 'org-conf-backend-get-page-info)
                   (lambda (id) (list :id id :title "Root")))
                  ((symbol-function 'org-conf-backend-get-children)
                   (lambda (_id) nil))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) "https://wiki.example.com/pages/777"))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _)
                     (setq menus (mapcar #'car collection))
                     "（手动输入页面 URL/ID）")))
          (should (equal "777" (org-conf--read-parent-id "DOCS")))
          (should (member "（手动输入页面 URL/ID）" menus))
          (should (member "（建在 space 根目录）" menus)))
      (delete-file org-conf-parent-freq-file))))

;;; ox-conf-test.el ends here
