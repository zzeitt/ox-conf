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

(ert-deftest ox-conf-test-return-api-url-passes-an-absolute-url-through ()
  (let ((org-conf-url "http://wiki.example.com/"))
    (should (equal "https://other.example.com/rest/api/content/1/child/page?start=1"
                   (org-conf--return-api-url
                    "https://other.example.com/rest/api/content/1/child/page?start=1")))))

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
                         :ancestors ((:id "1" :title "Root")
                                     (:id "5" :title "Parent"))
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
;;; Parent scope inference, recent parents and the file-like menu.
;;; ----------------------------------------------------------------------

(defun ox-conf-test--reset-parent-caches ()
  (clrhash org-conf--scope-cache)
  (clrhash org-conf--page-info-cache))

(ert-deftest ox-conf-test-common-ancestor-picks-the-deepest-shared-page ()
  (should (equal '(:id "2" :title "Mid")
                 (org-conf--common-ancestor
                  (list '((:id "1" :title "Root")
                          (:id "2" :title "Mid")
                          (:id "3" :title "A"))
                        '((:id "1" :title "Root")
                          (:id "2" :title "Mid")
                          (:id "4" :title "B")))))))

(ert-deftest ox-conf-test-common-ancestor-of-one-chain-is-its-deepest-page ()
  (should (equal '(:id "5" :title "Parent")
                 (org-conf--common-ancestor
                  (list '((:id "1" :title "Root")
                          (:id "5" :title "Parent")))))))

(ert-deftest ox-conf-test-common-ancestor-of-disjoint-chains-is-nil ()
  (should (null (org-conf--common-ancestor
                 (list '((:id "1" :title "A"))
                       '((:id "2" :title "B")))))))

(ert-deftest ox-conf-test-infer-scope-uses-sibling-conf-pages ()
  (let* ((org-conf-parent-freq-file (make-temp-name "/tmp/ox-conf-empty-freq-"))
         (dir (make-temp-file "ox-conf-scope-" t))
         (file (expand-file-name "current.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.org" dir)
            (insert "#+CONF_SPACE: SSC\n#+CONF_ID: 10\n"))
          (with-temp-file (expand-file-name "b.org" dir)
            (insert "#+CONF_SPACE: SSC\n#+CONF_ID: 11\n"))
          (ox-conf-test--reset-parent-caches)
          (with-temp-buffer
            (setq buffer-file-name file)
            (cl-letf (((symbol-function 'org-conf-backend-get-page-info)
                       (lambda (id)
                         (pcase id
                           ("10" (list :id id :title "A" :space "SSC"
                                       :parent-id "2"
                                       :ancestors '((:id "1" :title "Root")
                                                    (:id "2" :title "Mid"))))
                           ("11" (list :id id :title "B" :space "SSC"
                                       :parent-id "3"
                                       :ancestors '((:id "1" :title "Root")
                                                    (:id "2" :title "Mid")
                                                    (:id "3" :title "Leaf"))))
                           ("2" (list :id id :title "Mid" :space "SSC"
                                      :parent-id "1"
                                      :ancestors '((:id "1" :title "Root"))))))))
              (should (equal '(:id "2" :title "Mid" :space "SSC"
                               :parent-id "1"
                               :ancestors ((:id "1" :title "Root")))
                             (org-conf--infer-parent-scope "SSC"))))))
      (delete-directory dir t))))

(ert-deftest ox-conf-test-infer-scope-climbs-a-directory ()
  (let* ((org-conf-parent-freq-file (make-temp-name "/tmp/ox-conf-empty-freq-"))
         (base (make-temp-file "ox-conf-scope-" t))
         (sub (expand-file-name "sub" base))
         (file (expand-file-name "current.org" sub)))
    (make-directory sub)
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "sib.org" base)
            (insert "#+CONF_SPACE: SSC\n#+CONF_ID: 20\n"))
          (ox-conf-test--reset-parent-caches)
          (with-temp-buffer
            (setq buffer-file-name file)
            (cl-letf (((symbol-function 'org-conf-backend-get-page-info)
                       (lambda (id)
                         (pcase id
                           ("20" (list :id id :title "Sib" :space "SSC"
                                       :parent-id "2"
                                       :ancestors '((:id "1" :title "Root")
                                                    (:id "2" :title "Mid"))))
                           ("2" (list :id id :title "Mid" :space "SSC"
                                      :parent-id "1"
                                      :ancestors '((:id "1" :title "Root"))))))))
              (should (equal '(:id "2" :title "Mid" :space "SSC"
                               :parent-id "1"
                               :ancestors ((:id "1" :title "Root")))
                             (org-conf--infer-parent-scope "SSC"))))))
      (delete-directory base t))))

(ert-deftest ox-conf-test-infer-scope-prefers-the-recent-parent ()
  "With no exported siblings, the most-used parent page supplies the scope."
  (let* ((org-conf-parent-freq-file (make-temp-name "/tmp/ox-conf-empty-freq-"))
         (dir (make-temp-file "ox-conf-scope-" t))
         (file (expand-file-name "current.org" dir)))
    (unwind-protect
        (progn
          (org-conf--freq-save org-conf-parent-freq-file '(("42" . 2)))
          (ox-conf-test--reset-parent-caches)
          (with-temp-buffer
            (setq buffer-file-name file)
            (cl-letf (((symbol-function 'org-conf-backend-get-page-info)
                       (lambda (id)
                         (pcase id
                           ("42" (list :id id :title "HygonVML算法记录"
                                       :space "SSC" :parent-id "2"))
                           ("2" (list :id id :title "HygonVML"
                                      :space "SSC" :parent-id "1"
                                      :ancestors '((:id "1" :title "Root"))))))))
              (should (equal '(:id "2" :title "HygonVML" :space "SSC"
                               :parent-id "1"
                               :ancestors ((:id "1" :title "Root")))
                             (org-conf--infer-parent-scope "SSC"))))))
      (delete-file org-conf-parent-freq-file))))

(ert-deftest ox-conf-test-infer-scope-falls-back-to-title-search ()
  (let* ((org-conf-parent-freq-file (make-temp-name "/tmp/ox-conf-empty-freq-"))
         (base (make-temp-file "ox-conf-scope-" t))
         (dir (expand-file-name "vml" base))
         (file (expand-file-name "current.org" dir)))
    (make-directory dir)
    (unwind-protect
        (progn
          (ox-conf-test--reset-parent-caches)
          (with-temp-buffer
            (setq buffer-file-name file)
            (cl-letf (((symbol-function 'org-conf-backend-search-pages)
                       (lambda (_space query)
                         (should (equal "vml" query))
                         '((:id "9" :title "perf_vml.csv" :type "attachment")
                           (:id "2" :title "HygonVML" :type "page")
                           (:id "3" :title "HygonVML算法记录" :type "page"))))
                      ((symbol-function 'org-conf-backend-get-page-info)
                       (lambda (id)
                         (should (equal "2" id))
                         (list :id id :title "HygonVML" :space "SSC"
                               :parent-id "1"
                               :ancestors '((:id "1" :title "Root"))))))
              (should (equal '(:id "2" :title "HygonVML" :space "SSC"
                               :parent-id "1"
                               :ancestors ((:id "1" :title "Root")))
                             (org-conf--infer-parent-scope "SSC"))))))
      (delete-directory base t))))

(ert-deftest ox-conf-test-recent-parent-choices-filters-space-and-sorts ()
  (ox-conf-test--reset-parent-caches)
  (cl-letf (((symbol-function 'org-conf--freq-load)
             (lambda (_file) '(("a" . 3) ("b" . 1) ("c" . 2))))
            ((symbol-function 'org-conf-backend-get-page-info)
             (lambda (id)
               (pcase id
                 ("a" (list :id id :title "HygonVML算法记录" :space "SSC"))
                 ("b" (list :id id :title "Other" :space "OTHER"))
                 ("c" (list :id id :title "HygonVML研发记录" :space "SSC"))))))
    (should (equal (list (cons "★ 就用 HygonVML算法记录 (常用 3 次)" '(use "a"))
                         (cons "★ 就用 HygonVML研发记录 (常用 2 次)" '(use "c")))
                   (org-conf--recent-parent-choices "SSC")))))

(ert-deftest ox-conf-test-recent-parent-choices-dedups-and-limits ()
  (ox-conf-test--reset-parent-caches)
  (let ((freq '(("a" . 5) ("b" . 4) ("c" . 3)
                ("d" . 2) ("e" . 1) ("f" . 1))))
    (cl-letf (((symbol-function 'org-conf--freq-load) (lambda (_file) freq))
              ((symbol-function 'org-conf-backend-get-page-info)
               (lambda (id) (list :id id :title (format "Page-%s" id)
                                  :space "SSC")))
              ((symbol-value 'org-conf-recent-parent-limit) 5))
      (let ((choices (org-conf--recent-parent-choices "SSC" '("a"))))
        (should (= 5 (length choices)))
        (should-not (seq-find (lambda (c) (string-match-p "Page-a" (car c)))
                              choices))))))

(ert-deftest ox-conf-test-drill-choices-are-file-like ()
  (let ((current '(:id "2" :title "HygonVML" :parent-id "1"))
        (children '((:id "3" :title "算法记录") (:id "4" :title "研发记录")))
        (recent (list (cons "★ 就用 算法记录 (常用 2 次)" '(use "3")))))
    (should (equal (list (cons ".." '(up))
                         (cons "✓ 就用「HygonVML」作为父页面" '(confirm "2"))
                         (cons "★ 就用 算法记录 (常用 2 次)" '(use "3"))
                         (cons "算法记录" '(descend "3"))
                         (cons "研发记录" '(descend "4"))
                         (cons "（手动输入页面 URL/ID）" '(manual)))
                   (org-conf--drill-choices current children recent)))))

(ert-deftest ox-conf-test-drill-down-uses-a-recent-entry ()
  (let* ((org-conf-parent-freq-file (make-temp-file "ox-conf-freq" nil ".eld"))
         (stub (ox-conf-test--drill-stub '("★ 就用 目标 (常用 2 次)"))))
    (unwind-protect
        (progn
          (org-conf--freq-save org-conf-parent-freq-file '(("42" . 2)))
          (ox-conf-test--reset-parent-caches)
          (cl-letf (((symbol-function 'completing-read) (car stub))
                    ((symbol-function 'org-conf-backend-get-space-roots)
                     (lambda (_space) '((:id "1" :title "Root"))))
                    ((symbol-function 'org-conf-backend-get-page-info)
                     (lambda (id) (list :id id :title "目标" :space "SSC"))))
            (should (equal "42" (org-conf--read-parent-by-drill-down "SSC")))))
      (delete-file org-conf-parent-freq-file))))

(ert-deftest ox-conf-test-drill-down-dotdot-fetches-the-parent ()
  (let* ((org-conf-parent-freq-file (make-temp-name "/tmp/ox-conf-empty-freq-"))
         (stub (ox-conf-test--drill-stub '(".." "✓ 就用「Parent」作为父页面")))
         (scope '(:id "2" :title "Child" :parent-id "1")))
    (cl-letf (((symbol-function 'completing-read) (car stub))
              ((symbol-function 'org-conf-backend-get-children)
               (lambda (_id) nil))
              ((symbol-function 'org-conf-backend-get-page-info)
               (lambda (id) (list :id id :title "Parent" :parent-id nil))))
      (should (equal "1" (org-conf--read-parent-by-drill-down "SSC" scope))))))

(ert-deftest ox-conf-test-read-parent-id-starts-at-the-inferred-scope ()
  (let ((org-conf-root-parent-page "")
        (scope '(:id "2" :title "HygonVML" :parent-id "1")))
    (cl-letf (((symbol-function 'org-conf--infer-parent-scope)
               (lambda (space) (should (equal "SSC" space)) scope))
              ((symbol-function 'org-conf--read-parent-by-drill-down)
               (lambda (space scope-arg)
                 (should (equal "SSC" space))
                 (should (equal scope scope-arg))
                 "42")))
      (should (equal "42" (org-conf--read-parent-id "SSC"))))))

(ert-deftest ox-conf-test-cli-search-pages-builds-cql ()
  (cl-letf (((symbol-value 'org-conf--cli-json-function)
             (lambda (&rest args)
               (should (equal '("search" "title ~ \"vml\" and space = \"SSC\""
                                "--cql" "--limit" "20")
                              args))
               '((results . (((id . "1") (title . "HygonVML") (type . "page"))))))))
    (let ((org-conf-parent-child-page-limit 20))
      (should (equal '((:id "1" :title "HygonVML" :type "page"))
                     (org-conf-cli-search-pages "SSC" "vml"))))))

(ert-deftest ox-conf-test-rest-search-pages-builds-cql ()
  (let (api)
    (cl-letf (((symbol-function 'org-conf--rest-get-json)
               (lambda (path)
                 (setq api path)
                 '((results . [((id . "1") (title . "HygonVML") (type . "page"))])))))
      (let ((org-conf-parent-child-page-limit 20))
        (should (equal '((:id "1" :title "HygonVML" :type "page"))
                       (org-conf-rest-search-pages "SSC" "vml")))
        (should (string-match-p "\\`/rest/api/content/search[?]cql=" api))
        (should (string-match-p "limit=20" api))))))

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
;;; The CLI backend operations and their command lines.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-cli-api-path-removes-the-rest-prefix ()
  (should (equal "space/DOCS/content?depth=root&limit=200"
                 (org-conf--cli-api-path
                  "/rest/api/space/DOCS/content?depth=root&limit=200")))
  (should (equal "content/1/child/page?limit=200"
                 (org-conf--cli-api-path "/rest/api/content/1/child/page?limit=200"))))

(ert-deftest ox-conf-test-cli-get-page-info-maps-json ()
  (cl-letf (((symbol-value 'org-conf--cli-json-function)
             (lambda (&rest args)
               (should (equal '("info" "7") args))
               '((id . "7") (title . "Doc") (version . 3)
                 (spaceKey . "DOCS") (parentId . "5")
                 (ancestors . (((id . "1") (title . "Root"))
                               ((id . "5") (title . "Parent"))))
                 (url . "http://x/pages/7")))))
    (should (equal '(:id "7" :title "Doc" :version 3 :space "DOCS"
                     :parent-id "5"
                     :ancestors ((:id "1" :title "Root")
                                 (:id "5" :title "Parent"))
                     :url "http://x/pages/7")
                   (org-conf-cli-get-page-info "7")))))

(ert-deftest ox-conf-test-cli-get-children-maps-json ()
  (cl-letf (((symbol-value 'org-conf--cli-json-function)
             (lambda (&rest args)
               (should (equal '("children" "7") args))
               '((children . (((id . "8") (title . "Kid"))))))))
    (should (equal '((:id "8" :title "Kid"))
                   (org-conf-cli-get-children "7")))))

(ert-deftest ox-conf-test-cli-get-space-roots-reads-grouped-api-output ()
  (let ((calls nil)
        (org-conf-parent-child-page-limit 200))
    (cl-letf (((symbol-value 'org-conf--cli-json-function)
               (lambda (&rest args)
                 (push args calls)
                 '((page . ((results . [((id . "1") (title . "Root"))])))))))
      (should (equal '((:id "1" :title "Root"))
                     (org-conf-cli-get-space-roots "DOCS")))
      (should (equal '("api" "space/DOCS/content?depth=root&limit=200")
                     (car calls))))))

(ert-deftest ox-conf-test-cli-get-space-roots-follows-a-grouped-next-link ()
  (let ((calls nil)
        (org-conf-parent-child-page-limit 1))
    (cl-letf (((symbol-value 'org-conf--cli-json-function)
               (lambda (&rest args)
                 (push (cadr args) calls)
                 (if (equal (cadr args) "space/DOCS/content?depth=root&limit=1")
                     '((page . ((results . [((id . "1") (title . "A"))])
                                (_links . ((next . "https://x/next"))))))
                   '((page . ((results . [((id . "2") (title . "B"))]))))))))
      (should (equal '((:id "1" :title "A") (:id "2" :title "B"))
                     (org-conf-cli-get-space-roots "DOCS")))
      (should (equal '("space/DOCS/content?depth=root&limit=1"
                       "https://x/next")
                     (nreverse calls))))))

(ert-deftest ox-conf-test-cli-get-space-roots-rewrites-a-per-type-next-link ()
  "The CLI must not re-enumerate the whole content type on page two."
  (let ((calls nil)
        (org-conf-parent-child-page-limit 1))
    (cl-letf (((symbol-value 'org-conf--cli-json-function)
               (lambda (&rest args)
                 (push (cadr args) calls)
                 (if (equal (cadr args) "space/DOCS/content?depth=root&limit=1")
                     '((page . ((results . [((id . "1") (title . "Root"))])
                                (_links . ((next . "/rest/api/space/DOCS/content/page?limit=1&start=1"))))))
                   '((page . ((results . [((id . "2") (title . "Other"))]))))))))
      (should (equal '((:id "1" :title "Root") (:id "2" :title "Other"))
                     (org-conf-cli-get-space-roots "DOCS")))
      (should (equal '("space/DOCS/content?depth=root&limit=1"
                       "space/DOCS/content?depth=root&limit=1&start=1")
                     (nreverse calls))))))

(ert-deftest ox-conf-test-cli-create-page-uses-create-child-and-a-body-file ()
  (let (captured)
    (cl-letf (((symbol-value 'org-conf--cli-json-function)
               (lambda (&rest args)
                 (setq captured args)
                 '((id . "9") (url . "http://x/pages/9")))))
      (should (equal '(:id "9" :webui "http://x/pages/9")
                     (org-conf-cli-create-page "Doc" "DOCS" "5")))
      (should (equal "create-child" (car captured)))
      (should (equal "Doc" (nth 1 captured)))
      (should (equal "5" (nth 2 captured)))
      (should (member "--file" captured)))))

(ert-deftest ox-conf-test-cli-create-page-at-the-space-root ()
  (let (captured)
    (cl-letf (((symbol-value 'org-conf--cli-json-function)
               (lambda (&rest args)
                 (setq captured args)
                 '((id . "9") (url . "http://x/pages/9")))))
      (org-conf-cli-create-page "Doc" "DOCS" nil)
      (should (equal '("create" "Doc" "DOCS") (seq-take captured 3))))))

(ert-deftest ox-conf-test-cli-update-page-writes-the-body-and-omits-a-blank-title ()
  (let (captured)
    (cl-letf (((symbol-value 'org-conf--cli-json-function)
               (lambda (&rest args)
                 (setq captured args)
                 (let ((file (cadr (member "--file" args))))
                   (should (equal "<p>hi</p>"
                                  (with-temp-buffer
                                    (insert-file-contents file)
                                    (buffer-string)))))
                 '((version . 2)))))
      (org-conf-cli-update-page "9" "" "DOCS" "<p>hi</p>" 1 "")
      (should (equal '("update" "9") (seq-take captured 2)))
      (should (member "--file" captured))
      (should (member "--format" captured))
      (should (equal "storage"
                     (nth (1+ (seq-position captured "--format")) captured)))
      (should-not (member "--title" captured)))))

(ert-deftest ox-conf-test-cli-list-attachments-maps-json ()
  (cl-letf (((symbol-value 'org-conf--cli-json-function)
             (lambda (&rest args)
               (should (equal '("attachments" "9") args))
               '((attachments . (((id . "a1") (title . "a.png"))))))))
    (should (equal '((:id "a1" :title "a.png"))
                   (org-conf-cli-list-attachments "9")))))

(ert-deftest ox-conf-test-cli-upload-attachment-adds-replace ()
  (let (captured)
    (cl-letf (((symbol-value 'org-conf--cli-json-function)
               (lambda (&rest args) (setq captured args) '((uploaded . [])))))
      (org-conf-cli-upload-attachment "9" "/tmp/a.png" "a1")
      (should (equal '("attachment-upload" "9" "--file" "/tmp/a.png" "--replace")
                     captured)))))

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

(ert-deftest ox-conf-test-space-root-next-rewrites-a-per-type-next-link ()
  "The grouped next link drops depth=root; restore it with the next start."
  (should (equal "/rest/api/space/DOCS/content?depth=root&limit=1&start=1"
                 (org-conf--space-root-next
                  "/rest/api/space/DOCS/content?depth=root&limit=1"
                  "/rest/api/space/DOCS/content/page?limit=1&start=1"))))

(ert-deftest ox-conf-test-space-root-next-keeps-a-link-without-start ()
  (should (equal "/next" (org-conf--space-root-next "/api" "/next"))))

(ert-deftest ox-conf-test-fetch-all-space-roots-rewrites-a-grouped-next-link ()
  "A per-type next link must not re-enumerate the whole content type."
  (let* ((calls nil)
         (org-conf-parent-child-page-limit 1)
         (first '((page . ((results . [((id . "1") (title . "Root"))])
                           (_links . ((next . "/rest/api/space/DOCS/content/page?limit=1&start=1")))))))
         (second '((page . ((results . [((id . "2") (title . "Other"))])))))
         (get-json (lambda (api)
                     (push api calls)
                     (if (equal api "/rest/api/space/DOCS/content?depth=root&limit=1&start=1")
                         second first))))
    (should (equal '((:id "1" :title "Root") (:id "2" :title "Other"))
                   (org-conf--fetch-all-space-roots "DOCS" get-json)))
    (should (equal '("/rest/api/space/DOCS/content?depth=root&limit=1"
                     "/rest/api/space/DOCS/content?depth=root&limit=1&start=1")
                   (nreverse calls)))))

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
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
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
  "Standing on a page, offer to go up, confirm it, descend, or type a url."
  (let ((current '(:id "1" :title "Alpha" :parent-id "0"))
        (children '((:id "3" :title "Alpha-1"))))
    (should (equal (list '(".." up)
                         (cons "✓ 就用「Alpha」作为父页面" '(confirm "1"))
                         '("Alpha-1" descend "3")
                         (cons "（手动输入页面 URL/ID）" '(manual)))
                   (org-conf--drill-choices current children)))))

(ert-deftest ox-conf-test-drill-choices-work-on-a-leaf ()
  "A page with no children can still be confirmed or left."
  (let ((current '(:id "3" :title "Leaf" :parent-id "1")))
    (should (equal (list '(".." up)
                         (cons "✓ 就用「Leaf」作为父页面" '(confirm "3"))
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
  (let* ((org-conf-parent-freq-file (make-temp-name "/tmp/ox-conf-empty-freq-"))
         (stub (ox-conf-test--drill-stub
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
                       ("（建在 space 根目录）" "✓ 就用「Alpha」作为父页面"
                        "Alpha-1" "（手动输入页面 URL/ID）")
                       (".." "✓ 就用「Alpha-1」作为父页面"
                        "（手动输入页面 URL/ID）"))
                     (funcall menus))))))

(ert-deftest ox-conf-test-drill-down-can-stop-at-the-space-root ()
  (let* ((org-conf-parent-freq-file (make-temp-name "/tmp/ox-conf-empty-freq-"))
         (stub (ox-conf-test--drill-stub '("（建在 space 根目录）"))))
    (cl-letf (((symbol-function 'completing-read) (car stub))
              ((symbol-function 'org-conf-backend-get-space-roots)
               (lambda (_space) '((:id "1" :title "Alpha")))))
      (should (null (org-conf--read-parent-by-drill-down "DOCS"))))))

(ert-deftest ox-conf-test-drill-down-can-climb-back-up ()
  "Going up returns to the previous level's menu, not to the top blindly."
  (let* ((org-conf-parent-freq-file (make-temp-name "/tmp/ox-conf-empty-freq-"))
         (scope '(:id "2" :title "Alpha" :parent-id "1"))
         (stub (ox-conf-test--drill-stub
                '("Alpha-1" ".." "✓ 就用「Alpha」作为父页面")))
         (menus (cdr stub)))
    (cl-letf (((symbol-function 'completing-read) (car stub))
              ((symbol-function 'org-conf-backend-get-children)
               (lambda (_id) '((:id "3" :title "Alpha-1")))))
      (should (equal "2" (org-conf--read-parent-by-drill-down "DOCS" scope)))
      (should (member "Alpha-1" (nth 0 (funcall menus))))
      (should (equal (nth 0 (funcall menus)) (nth 2 (funcall menus)))))))

(ert-deftest ox-conf-test-drill-down-falls-back-to-a-typed-url ()
  "The manual entry keeps working for pages outside the space tree."
  (let* ((org-conf-parent-freq-file (make-temp-name "/tmp/ox-conf-empty-freq-"))
         (stub (ox-conf-test--drill-stub '("（手动输入页面 URL/ID）"))))
    (cl-letf (((symbol-function 'completing-read) (car stub))
              ((symbol-function 'read-string)
               (lambda (&rest _) "https://wiki.example.com/pages/777"))
              ((symbol-function 'org-conf-backend-get-space-roots)
               (lambda (_space) '((:id "1" :title "Alpha")))))
      (should (equal "777" (org-conf--read-parent-by-drill-down "DOCS"))))))

(ert-deftest ox-conf-test-drill-down-survives-a-failing-space-root-listing ()
  (let* ((org-conf-parent-freq-file (make-temp-name "/tmp/ox-conf-empty-freq-"))
         (stub (ox-conf-test--drill-stub '("（手动输入页面 URL/ID）"))))
    (cl-letf (((symbol-function 'completing-read) (car stub))
              ((symbol-function 'read-string) (lambda (&rest _) ""))
              ((symbol-function 'org-conf-backend-get-space-roots)
               (lambda (_space) (user-error "backend down"))))
      (should (null (org-conf--read-parent-by-drill-down "DOCS"))))))

(ert-deftest ox-conf-test-drill-down-survives-a-failing-child-listing ()
  (let* ((org-conf-parent-freq-file (make-temp-name "/tmp/ox-conf-empty-freq-"))
         (stub (ox-conf-test--drill-stub
                '("Alpha" "✓ 就用「Alpha」作为父页面"))))
    (cl-letf (((symbol-function 'completing-read) (car stub))
              ((symbol-function 'org-conf-backend-get-space-roots)
               (lambda (_space) '((:id "1" :title "Alpha"))))
              ((symbol-function 'org-conf-backend-get-children)
               (lambda (_id) (user-error "children down"))))
      (should (equal "1" (org-conf--read-parent-by-drill-down "DOCS"))))))

;;; ----------------------------------------------------------------------
;;; org-conf--read-parent-id: which source of parent pages is used.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-read-parent-id-walks-the-tree-when-no-root-is-set ()
  "With no `org-conf-root-parent-page', fall back to the space drill-down."
  (let ((org-conf-root-parent-page "")
        (asked nil))
    (cl-letf (((symbol-function 'org-conf--infer-parent-scope)
               (lambda (_space) nil))
              ((symbol-function 'org-conf--read-parent-by-drill-down)
               (lambda (space &optional _scope)
                 (setq asked space) "42")))
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

(ert-deftest ox-conf-test-read-parent-id-falls-back-when-the-root-cannot-be-read ()
  "A CLI read failure must not abort the create flow."
  (let ((org-conf-root-parent-page "999")
        (org-conf-parent-freq-file (make-temp-file "ox-conf-freq" nil ".eld"))
        (manual nil))
    (unwind-protect
        (cl-letf (((symbol-function 'org-conf-backend-get-page-info)
                   (lambda (_id) (user-error "backend down")))
                  ((symbol-function 'org-conf--read-parent-id-manually)
                   (lambda () (setq manual t) "777")))
          (should (equal "777" (org-conf--read-parent-id "DOCS")))
          (should manual))
      (delete-file org-conf-parent-freq-file))))

;;; ----------------------------------------------------------------------
;;; Background log and reproducible commands.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-log-appends-timestamped-lines ()
  (let ((org-conf-log-buffer-name " *ox-conf-test-log*")
        (org-conf-log-enabled t))
    (unwind-protect
        (progn
          (with-current-buffer (org-conf--log-buffer)
            (let ((inhibit-read-only t)) (erase-buffer)))
          (org-conf--log "hello %s" "world")
          (with-current-buffer (org-conf--log-buffer)
            (should (string-match-p "\\`\\[[0-9:]+\\] hello world"
                                    (buffer-string)))))
      (when (get-buffer org-conf-log-buffer-name)
        (kill-buffer org-conf-log-buffer-name)))))

(ert-deftest ox-conf-test-log-disabled-writes-nothing ()
  (let ((org-conf-log-buffer-name " *ox-conf-test-log*")
        (org-conf-log-enabled nil))
    (unwind-protect
        (progn
          (org-conf--log "nope")
          (should-not (get-buffer org-conf-log-buffer-name)))
      (when (get-buffer org-conf-log-buffer-name)
        (kill-buffer org-conf-log-buffer-name)))))

(ert-deftest ox-conf-test-log-trims-to-max-lines ()
  (let ((org-conf-log-buffer-name " *ox-conf-test-log*")
        (org-conf-log-max-lines 3))
    (unwind-protect
        (progn
          (with-current-buffer (org-conf--log-buffer)
            (let ((inhibit-read-only t)) (erase-buffer)))
          (dotimes (i 5) (org-conf--log "line %d" i))
          (with-current-buffer (org-conf--log-buffer)
            (should (= 3 (count-lines (point-min) (point-max))))
            (should-not (string-match-p "line 0" (buffer-string)))
            (should (string-match-p "line 4" (buffer-string)))))
      (when (get-buffer org-conf-log-buffer-name)
        (kill-buffer org-conf-log-buffer-name)))))

(ert-deftest ox-conf-test-cli-command-string-quotes-arguments ()
  (let ((org-conf-cli-program "confluence"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_program) "/usr/bin/confluence")))
      (should (equal "/usr/bin/confluence --json create \"My Doc\" DOCS"
                     (org-conf--cli-command-string
                      '("create" "My Doc" "DOCS")))))))

(ert-deftest ox-conf-test-elisp-command-names-the-backend-and-op ()
  (let ((org-conf-backend 'rest))
    (should (equal
             "(let ((org-conf-backend 'rest)) (org-conf-backend-get-page-info \"7\"))"
             (org-conf--elisp-command :get-page-info '("7"))))))

(ert-deftest ox-conf-test-log-call-reports-success-and-failure ()
  (let ((org-conf-log-buffer-name " *ox-conf-test-log*"))
    (unwind-protect
        (progn
          (org-conf--log-call :get-page-info '("7") (lambda () 'ok))
          (should-error (org-conf--log-call :get-page-info '("7")
                                            (lambda () (user-error "boom"))))
          (with-current-buffer (org-conf--log-buffer)
            (should (string-match-p "✅ get-page-info \"7\"" (buffer-string)))
            (should (string-match-p "❌ get-page-info \"7\"" (buffer-string)))
            (should (string-match-p "ELISP" (buffer-string)))
            (should (string-match-p "boom" (buffer-string)))))
      (when (get-buffer org-conf-log-buffer-name)
        (kill-buffer org-conf-log-buffer-name)))))

(ert-deftest ox-conf-test-cli-json-logs-the-command ()
  (let ((org-conf-log-buffer-name " *ox-conf-test-log*")
        (org-conf-log-enabled t))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (_program) "/usr/bin/confluence"))
                  ((symbol-function 'call-process)
                   (lambda (_program _infile dest _display &rest _)
                     (with-current-buffer (if (consp dest) (car dest) dest)
                       (insert "{\"id\": \"7\"}"))
                     0)))
          (org-conf--cli-json "info" "7")
          (with-current-buffer (org-conf--log-buffer)
            (should (string-match-p
                     "CMD (cli): /usr/bin/confluence --json info 7"
                     (buffer-string)))
            (should (string-match-p "✅ exit=0" (buffer-string)))))
      (when (get-buffer org-conf-log-buffer-name)
        (kill-buffer org-conf-log-buffer-name)))))

(ert-deftest ox-conf-test-with-temp-body-keeps-the-file-on-error ()
  (let ((org-conf-log-buffer-name " *ox-conf-test-log*")
        (path nil))
    (unwind-protect
        (progn
          (should-error
           (org-conf--with-temp-body
               "body"
             (lambda (file) (setq path file) (user-error "boom"))))
          (should (file-exists-p path)))
      (when (and path (file-exists-p path)) (delete-file path))
      (when (get-buffer org-conf-log-buffer-name)
        (kill-buffer org-conf-log-buffer-name)))))

(ert-deftest ox-conf-test-show-log-opens-the-buffer ()
  (let ((org-conf-log-buffer-name " *ox-conf-test-log*"))
    (unwind-protect
        (progn
          (org-conf-show-log)
          (should (equal (buffer-name) org-conf-log-buffer-name)))
      (when (get-buffer org-conf-log-buffer-name)
        (kill-buffer org-conf-log-buffer-name)))))

(ert-deftest ox-conf-test-log-mode-is-read-only-and-special ()
  (let ((org-conf-log-buffer-name " *ox-conf-test-log*"))
    (unwind-protect
        (with-current-buffer (org-conf--log-buffer)
          (should (eq major-mode 'org-conf-log-mode))
          (should (derived-mode-p 'special-mode))
          (should buffer-read-only))
      (when (get-buffer org-conf-log-buffer-name)
        (kill-buffer org-conf-log-buffer-name)))))

(ert-deftest ox-conf-test-log-mode-highlights-success-and-error ()
  (let ((org-conf-log-buffer-name " *ox-conf-test-log*"))
    (unwind-protect
        (progn
          (with-current-buffer (org-conf--log-buffer)
            (let ((inhibit-read-only t)) (erase-buffer)))
          (org-conf--log "✅ ok")
          (org-conf--log "❌ bad")
          (with-current-buffer (org-conf--log-buffer)
            (font-lock-ensure)
            (goto-char (point-min))
            (search-forward "✅")
            (should (eq 'org-conf-log-success
                        (get-text-property (1- (point)) 'face)))
            (goto-char (point-min))
            (search-forward "❌")
            (should (eq 'org-conf-log-error
                        (get-text-property (1- (point)) 'face)))))
      (when (get-buffer org-conf-log-buffer-name)
        (kill-buffer org-conf-log-buffer-name)))))

(ert-deftest ox-conf-test-export-to-conf-prompts-before-updating ()
  (let ((prompt nil)
        (updated nil)
        (export-plist nil))
    (with-temp-buffer
      (org-mode)
      (insert "#+TITLE: My Doc\n#+CONF_ID: 7\n#+CONF_SPACE: DOCS\n\nBody\n")
      (cl-letf (((symbol-function 'org-conf-backend-get-page-info)
                 (lambda (_id) (list :id "7" :title "Doc" :version 3)))
                ((symbol-function 'y-or-n-p)
                 (lambda (text) (setq prompt text) t))
                ((symbol-function 'org-export-as)
                 (lambda (&rest args)
                   (setq export-plist (nth 4 args))
                   "<p>x</p>"))
                ((symbol-function 'read-string) (lambda (&rest _) ""))
                ((symbol-function 'org-conf-backend-update-page)
                 (lambda (&rest args) (setq updated args))))
        (org-conf-export-to-conf)
        (should (string-match-p "Doc" prompt))
        (should (equal "7" (nth 0 updated)))
        (should (equal 4 (nth 4 updated)))
        (should (eq t (plist-get export-plist :org-conf-do-post)))))))

(ert-deftest ox-conf-test-export-to-conf-creates-when-declined ()
  (let ((created nil)
        (updated nil))
    (with-temp-buffer
      (org-mode)
      (insert "#+TITLE: My Doc\n#+CONF_ID: 7\n#+CONF_SPACE: DOCS\n\nBody\n")
      (cl-letf (((symbol-function 'org-conf-backend-get-page-info)
                 (lambda (_id) (list :id "7" :title "Doc" :version 3)))
                ((symbol-function 'y-or-n-p) (lambda (_text) nil))
                ((symbol-function 'org-conf-create-page-and-export)
                 (lambda () (setq created t)))
                ((symbol-function 'org-conf-backend-update-page)
                 (lambda (&rest _) (setq updated t))))
        (org-conf-export-to-conf)
        (should created)
        (should-not updated)))))

(ert-deftest ox-conf-test-create-flow-asks-before-uploading ()
  (let ((prompt nil)
        (updated nil)
        (org-conf--just-created t))
    (with-temp-buffer
      (org-mode)
      (insert "#+TITLE: My Doc\n#+CONF_ID: 7\n#+CONF_SPACE: DOCS\n\nBody\n")
      (cl-letf (((symbol-function 'org-conf-backend-get-page-info)
                 (lambda (_id) (list :id "7" :title "Doc" :version 3)))
                ((symbol-function 'y-or-n-p)
                 (lambda (text) (setq prompt text) t))
                ((symbol-function 'org-export-as)
                 (lambda (&rest _) "<p>x</p>"))
                ((symbol-function 'read-string) (lambda (&rest _) ""))
                ((symbol-function 'org-conf-backend-update-page)
                 (lambda (&rest args) (setq updated args))))
        (org-conf-export-to-conf)
        (should (string-match-p "Doc" prompt))
        (should updated)))))

(ert-deftest ox-conf-test-declining-the-new-page-does-not-create-another ()
  (let ((created nil)
        (updated nil)
        (org-conf--just-created t))
    (with-temp-buffer
      (org-mode)
      (insert "#+TITLE: My Doc\n#+CONF_ID: 7\n#+CONF_SPACE: DOCS\n\nBody\n")
      (cl-letf (((symbol-function 'org-conf-backend-get-page-info)
                 (lambda (_id) (list :id "7" :title "Doc" :version 3)))
                ((symbol-function 'y-or-n-p) (lambda (_text) nil))
                ((symbol-function 'org-conf-create-page-and-export)
                 (lambda () (setq created t)))
                ((symbol-function 'org-conf-backend-update-page)
                 (lambda (&rest _) (setq updated t))))
        (org-conf-export-to-conf)
        (should-not created)
        (should-not updated)))))

(ert-deftest ox-conf-test-insert-keywords-keeps-the-old-page-as-a-comment ()
  "Replacing the page keeps the previous URL as a comment, never as a second ID."
  (with-temp-buffer
    (org-mode)
    (insert (concat "#+TITLE: My Doc\n"
                    "#+CONF_SPACE: OLD\n"
                    "#+CONF_PAGE: http://x/old\n"
                    "#+CONF_ID: 1\n\nBody\n"))
    (org-conf--insert-conf-keywords "NEW" "http://x/new" "2")
    (should (equal (concat "#+TITLE: My Doc\n"
                           "#+CONF_SPACE: NEW\n"
                           "#+CONF_PAGE: http://x/new\n"
                           "#+CONF_ID: 2\n"
                           "# [ox-conf] previous page (kept for reference)\n"
                           "# #+CONF_SPACE: OLD\n"
                           "# #+CONF_PAGE: http://x/old\n"
                           "# #+CONF_ID: 1\n"
                           "\nBody\n")
                   (buffer-string)))
    (should (= 1 (how-many "^#\\+CONF_ID:" (point-min) (point-max))))))

(ert-deftest ox-conf-test-insert-keywords-replaces-the-previous-reference-block ()
  (with-temp-buffer
    (org-mode)
    (insert (concat "#+TITLE: T\n"
                    "#+CONF_SPACE: A\n"
                    "#+CONF_PAGE: http://x/a\n"
                    "#+CONF_ID: 1\n\nBody\n"))
    (org-conf--insert-conf-keywords "B" "http://x/b" "2")
    (org-conf--insert-conf-keywords "C" "http://x/c" "3")
    (should (equal (concat "#+TITLE: T\n"
                           "#+CONF_SPACE: C\n"
                           "#+CONF_PAGE: http://x/c\n"
                           "#+CONF_ID: 3\n"
                           "# [ox-conf] previous page (kept for reference)\n"
                           "# #+CONF_SPACE: B\n"
                           "# #+CONF_PAGE: http://x/b\n"
                           "# #+CONF_ID: 2\n"
                           "\nBody\n")
                   (buffer-string)))
    (should (= 1 (how-many "previous page" (point-min) (point-max))))))

(ert-deftest ox-conf-test-export-without-post-prompts-and-skips-attachments ()
  (let ((prompt nil)
        (updated nil)
        (export-plist nil))
    (with-temp-buffer
      (org-mode)
      (insert "#+TITLE: My Doc\n#+CONF_ID: 7\n#+CONF_SPACE: DOCS\n\nBody\n")
      (cl-letf (((symbol-function 'org-conf-backend-get-page-info)
                 (lambda (_id) (list :id "7" :title "Doc" :version 3)))
                ((symbol-function 'y-or-n-p)
                 (lambda (text) (setq prompt text) t))
                ((symbol-function 'org-export-as)
                 (lambda (&rest args)
                   (setq export-plist (nth 4 args))
                   "<p>x</p>"))
                ((symbol-function 'read-string) (lambda (&rest _) ""))
                ((symbol-function 'org-conf-backend-update-page)
                 (lambda (&rest args) (setq updated args))))
        (org-conf-export-to-conf-without-post)
        (should (string-match-p "Doc" prompt))
        (should (equal "7" (nth 0 updated)))
        (should-not (plist-get export-plist :org-conf-do-post))))))

(ert-deftest ox-conf-test-export-without-post-creates-when-declined ()
  (let ((called nil)
        (upload 'unset))
    (with-temp-buffer
      (org-mode)
      (insert "#+TITLE: My Doc\n#+CONF_ID: 7\n#+CONF_SPACE: DOCS\n\nBody\n")
      (cl-letf (((symbol-function 'org-conf-backend-get-page-info)
                 (lambda (_id) (list :id "7" :title "Doc" :version 3)))
                ((symbol-function 'y-or-n-p) (lambda (_text) nil))
                ((symbol-function 'org-conf-create-page-and-export)
                 (lambda ()
                   (setq called t)
                   (setq upload org-conf--upload-attachments))))
        (org-conf-export-to-conf-without-post)
        (should called)
        (should-not upload)))))

(ert-deftest ox-conf-test-export-without-post-creates-with-no-conf-id ()
  (let ((called nil)
        (upload 'unset))
    (with-temp-buffer
      (org-mode)
      (insert "#+TITLE: My Doc\n#+CONF_SPACE: DOCS\n\nBody\n")
      (cl-letf (((symbol-function 'org-conf-create-page-and-export)
                 (lambda ()
                   (setq called t)
                   (setq upload org-conf--upload-attachments))))
        (org-conf-export-to-conf-without-post)
        (should called)
        (should-not upload)))))

(ert-deftest ox-conf-test-export-to-conf-creates-with-attachments ()
  (let ((called nil)
        (upload 'unset))
    (with-temp-buffer
      (org-mode)
      (insert "#+TITLE: My Doc\n#+CONF_SPACE: DOCS\n\nBody\n")
      (cl-letf (((symbol-function 'org-conf-create-page-and-export)
                 (lambda ()
                   (setq called t)
                   (setq upload org-conf--upload-attachments))))
        (org-conf-export-to-conf)
        (should called)
        (should (eq t upload))))))

(ert-deftest ox-conf-test-org-files-near-tolerates-a-missing-directory ()
  (should (null (org-conf--org-files-near "/definitely/not/here"))))

;;; ----------------------------------------------------------------------
;;; Sync: org text normalization and hashing (watermark hash rules).
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-normalize-strips-a-leading-bom ()
  (should (equal (org-conf--normalize-org-text "#+TITLE: T\nBody\n")
                 (org-conf--normalize-org-text "﻿#+TITLE: T\nBody\n"))))

(ert-deftest ox-conf-test-normalize-unifies-line-endings ()
  (should (equal (org-conf--normalize-org-text "a\nb\n")
                 (org-conf--normalize-org-text "a\r\nb\r\n")))
  (should (equal (org-conf--normalize-org-text "a\nb\n")
                 (org-conf--normalize-org-text "a\rb\r"))))

(ert-deftest ox-conf-test-normalize-strips-trailing-whitespace ()
  (should (equal (org-conf--normalize-org-text "a\nb\n")
                 (org-conf--normalize-org-text "a   \nb\t\n"))))

(ert-deftest ox-conf-test-normalize-drops-conf-keywords ()
  "The watermarks must not make the local hash dirty when they are rewritten."
  (should (equal (org-conf--normalize-org-text "#+TITLE: T\nBody\n")
                 (org-conf--normalize-org-text
                  (concat "#+CONF_ID: 1\n"
                          "#+CONF_VERSION: 3\n"
                          "#+CONF_REMOTE_HASH: sha256:x\n"
                          "#+CONF_LOCAL_HASH: sha256:y\n"
                          "#+CONF_SYNCED_AT: now\n"
                          "#+TITLE: T\nBody\n")))))

(ert-deftest ox-conf-test-normalize-unifies-a-missing-final-newline ()
  (should (equal (org-conf--normalize-org-text "a\n")
                 (org-conf--normalize-org-text "a"))))

(ert-deftest ox-conf-test-sha256-string-matches-known-vectors ()
  (should (equal "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
                 (org-conf--sha256-string "")))
  (should (equal "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
                 (org-conf--sha256-string "abc"))))

;;; ----------------------------------------------------------------------
;;; Sync state machine (acceptance C, table driven).
;;; ----------------------------------------------------------------------

(defvar ox-conf-test--sync-baseline
  '(:conf-id "1" :conf-version 5
    :conf-local-hash "L5" :conf-remote-hash "R5"
    :local-hash "L5"
    :remote-version 5 :remote-hash "R5" :remote-exists t)
  "A page that is fully in sync at version 5.")

(defun ox-conf-test--sync-state (overrides)
  "Run `org-conf--sync-state' with OVERRIDES on the baseline."
  (plist-get (org-conf--sync-state (append overrides ox-conf-test--sync-baseline))
             :state))

(ert-deftest ox-conf-test-sync-state-synced ()
  (should (eq 'synced (ox-conf-test--sync-state nil))))

(ert-deftest ox-conf-test-sync-state-remote-ahead ()
  (should (eq 'remote-ahead
              (ox-conf-test--sync-state
               '(:remote-version 6 :remote-hash "R6")))))

(ert-deftest ox-conf-test-sync-state-local-ahead ()
  (should (eq 'local-ahead
              (ox-conf-test--sync-state '(:local-hash "L6")))))

(ert-deftest ox-conf-test-sync-state-diverged ()
  (should (eq 'diverged
              (ox-conf-test--sync-state
               '(:local-hash "L6" :remote-version 6 :remote-hash "R6")))))

(ert-deftest ox-conf-test-sync-state-remote-deleted ()
  (should (eq 'remote-deleted
              (ox-conf-test--sync-state '(:remote-exists nil)))))

(ert-deftest ox-conf-test-sync-state-local-missing ()
  "No #+CONF_ID means there is nothing local to sync."
  (should (eq 'local-missing (ox-conf-test--sync-state '(:conf-id nil)))))

(ert-deftest ox-conf-test-sync-state-ignores-an-empty-remote-save ()
  "A new version whose body is unchanged is synced, not remote-ahead."
  (should (eq 'synced
              (ox-conf-test--sync-state
               '(:remote-version 6 :remote-hash "R5")))))

(ert-deftest ox-conf-test-sync-state-without-a-baseline-is-diverged ()
  "A file with CONF_ID but no CONF_VERSION needs a human decision."
  (should (eq 'diverged
              (ox-conf-test--sync-state '(:conf-version nil)))))

;;; ----------------------------------------------------------------------
;;; Sync watermarks: #+CONF_VERSION and friends.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-sync-watermarks-are-read-from-the-buffer ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n"
            "#+CONF_ID: 1\n"
            "#+CONF_VERSION: 5\n"
            "#+CONF_REMOTE_HASH: sha256:aa\n"
            "#+CONF_LOCAL_HASH: sha256:bb\n"
            "#+CONF_SYNCED_AT: 2026-11-12\n")
    (should (equal '(:conf-id "1" :conf-version 5
                     :conf-remote-hash "sha256:aa"
                     :conf-local-hash "sha256:bb"
                     :conf-synced-at "2026-11-12")
                   (org-conf--sync-watermarks)))))

(ert-deftest ox-conf-test-set-keywords-updates-existing-and-appends-missing ()
  "Existing keywords keep their place; new ones join the block in order."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+CONF_ID: 1\n#+CONF_VERSION: 5\n\nBody\n")
    (org-conf--set-keywords '(("CONF_VERSION" . "6")
                              ("CONF_REMOTE_HASH" . "sha256:rr")
                              ("CONF_LOCAL_HASH" . "sha256:ll")
                              ("CONF_SYNCED_AT" . "now")))
    (should (equal (concat "#+TITLE: T\n"
                           "#+CONF_ID: 1\n"
                           "#+CONF_VERSION: 6\n"
                           "#+CONF_REMOTE_HASH: sha256:rr\n"
                           "#+CONF_LOCAL_HASH: sha256:ll\n"
                           "#+CONF_SYNCED_AT: now\n"
                           "\nBody\n")
                   (buffer-string)))))

(ert-deftest ox-conf-test-set-keywords-lands-after-the-title ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n\nBody\n")
    (org-conf--set-keywords '(("CONF_VERSION" . "1")))
    (should (equal "#+TITLE: T\n#+CONF_VERSION: 1\n\nBody\n"
                   (buffer-string)))))

(ert-deftest ox-conf-test-rewriting-watermarks-keeps-the-local-hash-stable ()
  "Rewriting the watermarks must not make the page look locally dirty."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+CONF_ID: 1\n\nBody\n")
    (let ((before (org-conf--sha256-string
                   (org-conf--normalize-org-text (buffer-string)))))
      (org-conf--set-keywords '(("CONF_VERSION" . "6")
                                ("CONF_REMOTE_HASH" . "sha256:rr")))
      (should (equal before
                     (org-conf--sha256-string
                      (org-conf--normalize-org-text (buffer-string))))))))

;;; ----------------------------------------------------------------------
;;; Sync: deterministic storage projection for ediff.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-canonicalize-is-idempotent ()
  (let* ((storage "<p>hello <strong>world</strong></p>")
         (once (org-conf--canonicalize-storage storage)))
    (should (equal once (org-conf--canonicalize-storage once)))))

(ert-deftest ox-conf-test-canonicalize-sorts-attributes ()
  (should (equal (org-conf--canonicalize-storage "<a z=\"1\" b=\"2\">x</a>")
                 (org-conf--canonicalize-storage "<a b=\"2\" z=\"1\">x</a>"))))

(ert-deftest ox-conf-test-canonicalize-normalizes-whitespace-and-crlf ()
  (should (equal (org-conf--canonicalize-storage "<p>a b</p>\n")
                 (org-conf--canonicalize-storage "<p>a \r\n  b</p>\r\n"))))

(ert-deftest ox-conf-test-canonicalize-keeps-inline-comment-markers ()
  "The projection is also how M4 shows marker loss in a diff."
  (let ((canonical
         (org-conf--canonicalize-storage
          "<p><ac:inline-comment-marker ac:ref=\"u\">函数性质</ac:inline-comment-marker></p>")))
    (should (string-match-p
             (regexp-quote "<ac:inline-comment-marker ac:ref=\"u\">") canonical))
    (should (string-match-p
             (regexp-quote "</ac:inline-comment-marker>") canonical))
    (should (string-match-p "函数性质" canonical))))

(ert-deftest ox-conf-test-canonicalize-of-empty-storage-is-empty ()
  (should (equal "" (org-conf--canonicalize-storage "")))
  (should (equal "" (org-conf--canonicalize-storage nil))))

;;; ----------------------------------------------------------------------
;;; Sync: CLI result parsing and page plists.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-cli-parse-result-success ()
  (should (equal '(:ok t :data ((a . 1)))
                 (org-conf--cli-parse-result 0 "{\"a\":1}" ""))))

(ert-deftest ox-conf-test-cli-parse-result-carries-the-http-status ()
  (should (equal '(:ok nil :status 404 :code "NOT_FOUND"
                   :message "Request failed with status code 404")
                 (org-conf--cli-parse-result
                  1 ""
                  (concat "{\"error\":\"Request failed with status code 404\","
                          "\"code\":\"NOT_FOUND\",\"status\":404}")))))

(ert-deftest ox-conf-test-cli-parse-result-handles-plain-stderr ()
  (let ((result (org-conf--cli-parse-result 1 "" "boom")))
    (should (null (plist-get result :ok)))
    (should (null (plist-get result :status)))
    (should (null (plist-get result :code)))
    (should (equal "boom" (plist-get result :message)))))

(ert-deftest ox-conf-test-cli-parse-result-rejects-invalid-json ()
  (let ((result (org-conf--cli-parse-result 0 "not json" "")))
    (should (null (plist-get result :ok)))
    (should (equal "INVALID_JSON" (plist-get result :code)))))

(ert-deftest ox-conf-test-page-json-to-plist-extracts-the-sync-facts ()
  (let* ((storage "<p>x</p>")
         (json `((id . "1")
                 (title . "T")
                 (status . "current")
                 (version . ((number . 7)))
                 (space . ((key . "SSC")))
                 (body . ((storage . ((value . ,storage)))))))
         (page (org-conf--page-json-to-plist json)))
    (should (eq t (plist-get page :ok)))
    (should (equal 7 (plist-get page :version)))
    (should (equal storage (plist-get page :storage)))
    (should (equal (org-conf--sha256-string storage) (plist-get page :hash)))
    (should (equal "T" (plist-get page :title)))
    (should (equal "SSC" (plist-get page :space)))
    (should (equal "current" (plist-get page :status)))))

;;; ----------------------------------------------------------------------
;;; Sync: snapshot store and retention.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-snapshot-round-trip ()
  (let ((org-conf-history-directory (make-temp-file "ox-conf-hist" t)))
    (unwind-protect
        (progn
          (org-conf--snapshot-write "123" 5 "<p>hi</p>")
          (should (equal "<p>hi</p>" (org-conf--snapshot-read "123" 5)))
          (should (null (org-conf--snapshot-read "123" 4))))
      (delete-directory org-conf-history-directory t))))

(ert-deftest ox-conf-test-snapshot-prune-keeps-the-newest-n ()
  (let ((org-conf-history-directory (make-temp-file "ox-conf-hist" t))
        (org-conf-history-retain 3))
    (unwind-protect
        (progn
          (dolist (v '(1 2 3 4 5))
            (org-conf--snapshot-write "123" v (format "v%d" v)))
          (org-conf--snapshot-prune "123")
          (should (null (org-conf--snapshot-read "123" 1)))
          (should (null (org-conf--snapshot-read "123" 2)))
          (should (equal "v3" (org-conf--snapshot-read "123" 3)))
          (should (equal "v5" (org-conf--snapshot-read "123" 5))))
      (delete-directory org-conf-history-directory t))))

(ert-deftest ox-conf-test-snapshot-prune-sorts-versions-numerically ()
  "v10 must outrank v9 when retaining a single snapshot."
  (let ((org-conf-history-directory (make-temp-file "ox-conf-hist" t))
        (org-conf-history-retain 1))
    (unwind-protect
        (progn
          (org-conf--snapshot-write "123" 9 "nine")
          (org-conf--snapshot-write "123" 10 "ten")
          (org-conf--snapshot-prune "123")
          (should (null (org-conf--snapshot-read "123" 9)))
          (should (equal "ten" (org-conf--snapshot-read "123" 10))))
      (delete-directory org-conf-history-directory t))))

;;; ----------------------------------------------------------------------
;;; Sync: CLI body fetchers.
;;; ----------------------------------------------------------------------

(defun ox-conf-test--page-json (storage version)
  `((id . "1") (title . "T") (status . "current")
    (version . ((number . ,version)))
    (space . ((key . "SSC")))
    (body . ((storage . ((value . ,storage)))))))

(ert-deftest ox-conf-test-cli-current-body-targets-the-content-api ()
  (let ((asked nil))
    (cl-letf (((symbol-function 'org-conf--cli-json-safe)
               (lambda (&rest args)
                 (setq asked args)
                 (list :ok t :data (ox-conf-test--page-json "<p>x</p>" 7)))))
      (let ((page (org-conf-cli-get-current-body "1")))
        (should (equal 7 (plist-get page :version)))
        (should (equal "<p>x</p>" (plist-get page :storage)))
        (should (equal '("api" "content/1?expand=body.storage,version,space")
                       asked))))))

(ert-deftest ox-conf-test-cli-version-body-asks-for-a-historical-version ()
  (let ((asked nil))
    (cl-letf (((symbol-function 'org-conf--cli-json-safe)
               (lambda (&rest args)
                 (setq asked args)
                 (list :ok t :data (ox-conf-test--page-json "<p>old</p>" 2)))))
      (let ((page (org-conf-cli-get-version-body "1" 2)))
        (should (equal "<p>old</p>" (plist-get page :storage)))
        (should (equal '("api" "content/1?status=historical&version=2&expand=body.storage,version")
                       asked))))))

(ert-deftest ox-conf-test-cli-current-body-reports-a-deleted-page ()
  (cl-letf (((symbol-function 'org-conf--cli-json-safe)
             (lambda (&rest _)
               (list :ok nil :status 404 :code "NOT_FOUND" :message "gone"))))
    (should (equal '(:ok nil :status 404 :code "NOT_FOUND")
                   (org-conf-cli-get-current-body "1")))))

(ert-deftest ox-conf-test-cli-current-body-signals-on-other-failures ()
  (cl-letf (((symbol-function 'org-conf--cli-json-safe)
             (lambda (&rest _)
               (list :ok nil :status 500 :code "SERVER_ERROR" :message "boom"))))
    (should-error (org-conf-cli-get-current-body "1") :type 'user-error)))

(ert-deftest ox-conf-test-cli-list-versions-normalizes-the-list ()
  (let ((asked nil))
    (cl-letf (((symbol-function 'org-conf--cli-json-safe)
               (lambda (&rest args)
                 (setq asked args)
                 (list :ok t
                       :data '((pageId . "1")
                         (versions . (((number . 1) (when . "t1") (by . "me")
                                       (minorEdit . nil) (message . "first"))
                                      ((number . 2) (when . "t2") (by . "you")
                                       (minorEdit . t) (message . "")))))))))
      (should (equal '((:number 1 :when "t1" :by "me" :minor-edit nil :message "first")
                       (:number 2 :when "t2" :by "you" :minor-edit t :message ""))
                     (org-conf-cli-list-versions "1")))
      (should (equal '("versions" "1") asked)))))

;;; ----------------------------------------------------------------------
;;; Sync: local export hashes, baseline adoption and diff files.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-current-local-body-is-deterministic ()
  "Two exports of the same buffer must be byte-identical (acceptance A)."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+CONF_ID: 1\n\n"
            "Body with *bold*, =code= and a [[https://example.com][link]].\n\n"
            "#+begin_src emacs-lisp\n(+ 1 2)\n#+end_src\n\n"
            "| a | b |\n|---+---|\n| 1 | 2 |\n")
    (should (equal (org-conf--current-local-body)
                   (org-conf--current-local-body)))))

(ert-deftest ox-conf-test-local-body-hash-ignores-export-neutral-edits ()
  "A comment line changes the org hash but not what would be published."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+CONF_ID: 1\n\nBody\n")
    (let ((org-hash (org-conf--current-local-hash))
          (body-hash (org-conf--sha256-string (org-conf--current-local-body))))
      (goto-char (point-max))
      (insert "# a comment that org never exports\n")
      (should-not (equal org-hash (org-conf--current-local-hash)))
      (should (equal body-hash
                     (org-conf--sha256-string (org-conf--current-local-body)))))))

(ert-deftest ox-conf-test-sync-adopt-writes-the-watermarks ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+CONF_ID: 1\n\nBody\n")
    (let ((org-conf-history-directory (make-temp-file "ox-conf-hist" t)))
      (unwind-protect
          (let ((remote (list :ok t :version 6 :hash "R6" :storage "<p>x</p>")))
            (org-conf--sync-adopt (org-conf--sync-watermarks) remote)
            (let ((watermarks (org-conf--sync-watermarks)))
              (should (equal 6 (plist-get watermarks :conf-version)))
              (should (equal "R6" (plist-get watermarks :conf-remote-hash)))
              (should (equal (org-conf--current-local-hash)
                             (plist-get watermarks :conf-local-hash))))
            (should (equal "<p>x</p>" (org-conf--snapshot-read "1" 6))))
        (delete-directory org-conf-history-directory t)))))

(ert-deftest ox-conf-test-write-diff-file-round-trips-utf8 ()
  (let ((file (org-conf--write-diff-file "local" "<p>中文</p>\n")))
    (unwind-protect
        (progn
          (should (file-readable-p file))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal "<p>中文</p>\n" (buffer-string)))))
      (delete-file file))))

(ert-deftest ox-conf-test-sync-collect-reports-synced ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+CONF_ID: 1\n\nBody\n")
    (let* ((local-hash (org-conf--current-local-hash))
           (body (org-conf--current-local-body))
           (body-hash (org-conf--sha256-string body)))
      (goto-char (point-max))
      (insert (format "#+CONF_VERSION: 5\n#+CONF_REMOTE_HASH: %s\n#+CONF_LOCAL_HASH: %s\n"
                      body-hash local-hash))
      (cl-letf (((symbol-function 'org-conf-cli-get-current-body)
                 (lambda (_id)
                   (list :ok t :version 5 :hash body-hash :storage body))))
        (should (eq 'synced
                    (plist-get (plist-get (org-conf--sync-collect) :state)
                               :state)))))))

(ert-deftest ox-conf-test-sync-collect-reports-remote-ahead ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+CONF_ID: 1\n\nBody\n")
    (let* ((local-hash (org-conf--current-local-hash))
           (body (org-conf--current-local-body))
           (body-hash (org-conf--sha256-string body)))
      (goto-char (point-max))
      (insert (format "#+CONF_VERSION: 5\n#+CONF_REMOTE_HASH: %s\n#+CONF_LOCAL_HASH: %s\n"
                      body-hash local-hash))
      (cl-letf (((symbol-function 'org-conf-cli-get-current-body)
                 (lambda (_id)
                   (list :ok t :version 6 :hash "different" :storage "<p>new</p>"))))
        (should (eq 'remote-ahead
                    (plist-get (plist-get (org-conf--sync-collect) :state)
                               :state)))))))

(ert-deftest ox-conf-test-sync-collect-reports-a-deleted-page ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+CONF_ID: 1\n\nBody\n")
    (cl-letf (((symbol-function 'org-conf-cli-get-current-body)
               (lambda (_id) (list :ok nil :status 404 :code "NOT_FOUND"))))
      (should (eq 'remote-deleted
                  (plist-get (plist-get (org-conf--sync-collect) :state)
                             :state))))))

;;; ----------------------------------------------------------------------
;;; M1 review fixes: projection, integer parsing, CLI stderr, history dir.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-option-integer-accepts-only-digits ()
  (should (equal 5 (org-conf--option-integer "5")))
  (should (equal 5 (org-conf--option-integer " 5 ")))
  (should (null (org-conf--option-integer "5x")))
  (should (null (org-conf--option-integer "x5")))
  (should (null (org-conf--option-integer "")))
  (should (null (org-conf--option-integer nil))))

(ert-deftest ox-conf-test-history-dir-defaults-next-to-the-org-file ()
  (let ((org-conf-history-directory nil)
        (buffer-file-name "/tmp/notes/doc.org"))
    (should (equal (expand-file-name ".conf-history/1" "/tmp/notes/")
                   (org-conf--history-dir "1")))))

(ert-deftest ox-conf-test-cli-parse-result-handles-a-json-string-stderr ()
  "A JSON string on stderr must not make `alist-get' signal."
  (let ((result (org-conf--cli-parse-result 1 "" "\"boom\"")))
    (should (null (plist-get result :status)))
    (should (null (plist-get result :code)))
    (should (equal "boom" (plist-get result :message)))))

(ert-deftest ox-conf-test-canonicalize-keeps-unparseable-attributes ()
  "Single-quoted or unquoted attributes must not be silently dropped."
  (should (equal "<a b='1'>" (org-conf--canonicalize-tag "<a b='1'>")))
  (should (equal "<a b=\"1\" c='2'>"
                 (org-conf--canonicalize-tag "<a b=\"1\" c='2'>"))))

(ert-deftest ox-conf-test-canonicalize-drops-server-injected-attributes ()
  "Confluence injects these on save; the diff projection must ignore them."
  (should (equal (org-conf--canonicalize-storage
                  "<ac:structured-macro ac:name=\"toc\"><ac:parameter ac:name=\"a\">1</ac:parameter></ac:structured-macro>")
                 (org-conf--canonicalize-storage
                  "<ac:structured-macro ac:macro-id=\"uuid\" ac:schema-version=\"1\" ac:name=\"toc\"><ac:parameter ac:name=\"a\">1</ac:parameter></ac:structured-macro>"))))

(ert-deftest ox-conf-test-canonicalize-sorts-macro-parameters ()
  (should (equal (org-conf--canonicalize-storage
                  "<ac:structured-macro ac:name=\"code\"><ac:parameter ac:name=\"a\">1</ac:parameter><ac:parameter ac:name=\"z\">2</ac:parameter></ac:structured-macro>")
                 (org-conf--canonicalize-storage
                  "<ac:structured-macro ac:name=\"code\"><ac:parameter ac:name=\"z\">2</ac:parameter><ac:parameter ac:name=\"a\">1</ac:parameter></ac:structured-macro>"))))

(ert-deftest ox-conf-test-canonicalize-sorts-parameters-inside-nested-macros ()
  "A macro inside ac:rich-text-body must be sorted too."
  (let* ((outer-open "<ac:structured-macro ac:name=\"outer\">")
         (inner-open "<ac:structured-macro ac:name=\"inner\">")
         (close "</ac:structured-macro>")
         (rich "</ac:rich-text-body>")
         (a (concat outer-open
                    "<ac:parameter ac:name=\"z\">2</ac:parameter>"
                    "<ac:parameter ac:name=\"a\">1</ac:parameter>"
                    "<ac:rich-text-body>" inner-open
                    "<ac:parameter ac:name=\"y\">4</ac:parameter>"
                    "<ac:parameter ac:name=\"b\">3</ac:parameter>"
                    close rich close))
         (b (concat outer-open
                    "<ac:parameter ac:name=\"a\">1</ac:parameter>"
                    "<ac:parameter ac:name=\"z\">2</ac:parameter>"
                    "<ac:rich-text-body>" inner-open
                    "<ac:parameter ac:name=\"b\">3</ac:parameter>"
                    "<ac:parameter ac:name=\"y\">4</ac:parameter>"
                    close rich close)))
    (should (equal (org-conf--canonicalize-storage a)
                   (org-conf--canonicalize-storage b)))))

;;; ----------------------------------------------------------------------
;;; M1 review blocker 2: refresh watermarks after a successful publish.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-adopt-after-publish-updates-watermarks ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+CONF_ID: 1\n\nBody\n")
    (let ((org-conf-history-directory (make-temp-file "ox-conf-hist" t))
          (org-conf-sync-adopt-after-publish t))
      (unwind-protect
          (cl-letf (((symbol-function 'org-conf-cli-get-current-body)
                     (lambda (_id)
                       (list :ok t :version 7 :hash "R7" :storage "<p>x</p>"))))
            (org-conf--maybe-adopt-after-publish)
            (let ((watermarks (org-conf--sync-watermarks)))
              (should (equal 7 (plist-get watermarks :conf-version)))
              (should (equal "R7" (plist-get watermarks :conf-remote-hash)))))
        (delete-directory org-conf-history-directory t)))))

(ert-deftest ox-conf-test-adopt-after-publish-ignores-a-missing-page ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+CONF_ID: 1\n\nBody\n")
    (let ((org-conf-sync-adopt-after-publish t))
      (cl-letf (((symbol-function 'org-conf-cli-get-current-body)
                 (lambda (_id)
                   (list :ok nil :status 404 :code "NOT_FOUND"))))
        (org-conf--maybe-adopt-after-publish)
        (should (null (plist-get (org-conf--sync-watermarks)
                                 :conf-version)))))))

(ert-deftest ox-conf-test-adopt-after-publish-can-be-disabled ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+CONF_ID: 1\n\nBody\n")
    (let ((org-conf-sync-adopt-after-publish nil))
      (cl-letf (((symbol-function 'org-conf-cli-get-current-body)
                 (lambda (_id) (error "must not be called"))))
        (org-conf--maybe-adopt-after-publish)
        (should (null (plist-get (org-conf--sync-watermarks)
                                 :conf-version)))))))

(ert-deftest ox-conf-test-export-adopts-the-baseline-after-a-successful-put ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+CONF_ID: 1\n\nBody\n")
    (let ((org-conf-history-directory (make-temp-file "ox-conf-hist" t))
          (org-conf-sync-adopt-after-publish t)
          (put nil))
      (unwind-protect
          (cl-letf (((symbol-function 'org-conf-backend-get-page-info)
                     (lambda (_id) (list :title "T" :version 5)))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'read-string) (lambda (&rest _) "msg"))
                    ((symbol-function 'org-export-as)
                     (lambda (&rest _) "<p>new</p>"))
                    ((symbol-function 'org-conf-backend-update-page)
                     (lambda (&rest args) (setq put args) t))
                    ((symbol-function 'org-conf-cli-get-current-body)
                     (lambda (_id)
                       (list :ok t :version 6 :hash "R6" :storage "<p>new</p>"))))
            (org-conf--export-to-conf t)
            (should put)
            (should (equal 6 (plist-get (org-conf--sync-watermarks)
                                        :conf-version))))
        (delete-directory org-conf-history-directory t)))))

(ert-deftest ox-conf-test-export-does-not-adopt-after-a-failed-put ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+CONF_ID: 1\n\nBody\n")
    (let ((org-conf-sync-adopt-after-publish t))
      (cl-letf (((symbol-function 'org-conf-backend-get-page-info)
                 (lambda (_id) (list :title "T" :version 5)))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'read-string) (lambda (&rest _) "msg"))
                ((symbol-function 'org-export-as)
                 (lambda (&rest _) "<p>new</p>"))
                ((symbol-function 'org-conf-backend-update-page)
                 (lambda (&rest _) nil))
                ((symbol-function 'org-conf-cli-get-current-body)
                 (lambda (_id)
                   (list :ok nil :status 404 :code "NOT_FOUND"))))
        (org-conf--export-to-conf t)
        (should (null (plist-get (org-conf--sync-watermarks)
                                 :conf-version)))))))

(ert-deftest ox-conf-test-export-protects-markers-before-the-put ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+CONF_ID: 1\n\nBody\n")
    (let ((put nil)
          (calls 0)
          (org-conf-history-directory (make-temp-file "ox-conf-hist" t))
          (org-conf-marker-loss-policy 'error))
      (unwind-protect
          (cl-letf (((symbol-function 'org-conf-backend-get-page-info)
                     (lambda (_id) (list :title "T" :version 5)))
                    ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'read-string) (lambda (&rest _) "msg"))
                    ((symbol-function 'org-export-as)
                     (lambda (&rest _) "<p>anchor text</p>"))
                    ((symbol-function 'org-conf-cli-get-current-body)
                     (lambda (_id)
                       (setq calls (1+ calls))
                       (if (= calls 1)
                           (list :ok t :version 5 :hash "R5"
                                 :storage (concat "<p><ac:inline-comment-marker ac:ref=\"u1\">"
                                                  "anchor text"
                                                  "</ac:inline-comment-marker></p>"))
                         (list :ok t :version 6 :hash "R6" :storage "<p>x</p>"))))
                    ((symbol-function 'org-conf-backend-update-page)
                     (lambda (&rest args) (setq put args) t)))
            (org-conf--export-to-conf t)
            (should (string-match-p "ac:inline-comment-marker ac:ref=\"u1\""
                                    (nth 3 put))))
        (delete-directory org-conf-history-directory t)))))

(ert-deftest ox-conf-test-export-refuses-to-put-when-an-anchor-is-lost ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+CONF_ID: 1\n\nBody\n")
    (let ((put nil)
          (org-conf-marker-loss-policy 'error))
      (cl-letf (((symbol-function 'org-conf-backend-get-page-info)
                 (lambda (_id) (list :title "T" :version 5)))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'read-string) (lambda (&rest _) "msg"))
                ((symbol-function 'org-export-as)
                 (lambda (&rest _) "<p>changed</p>"))
                ((symbol-function 'org-conf-cli-get-current-body)
                 (lambda (_id)
                   (list :ok t :version 5 :hash "R5"
                         :storage (concat "<p><ac:inline-comment-marker ac:ref=\"u1\">"
                                          "anchor text"
                                          "</ac:inline-comment-marker></p>"))))
                ((symbol-function 'org-conf-backend-update-page)
                 (lambda (&rest args) (setq put args) t)))
        (should-error (org-conf--export-to-conf t) :type 'user-error)
        (should (null put))))))

(ert-deftest ox-conf-test-canonicalize-drops-empty-parameters ()
  "Confluence drops empty parameters on save, so the projection must too."
  (should (equal (org-conf--canonicalize-storage
                  "<ac:structured-macro ac:name=\"code\"></ac:structured-macro>")
                 (org-conf--canonicalize-storage
                  "<ac:structured-macro ac:name=\"code\"><ac:parameter ac:name=\"title\"></ac:parameter></ac:structured-macro>"))))

;;; ----------------------------------------------------------------------
;;; M1 leftovers: narrowing-safe hash, self-closing parameters.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-local-hash-ignores-narrowing ()
  "f s in a narrowed buffer must still hash the whole document."
  (with-temp-buffer
    (org-mode)
    (insert "* A\nbody a\n* B\nbody b\n")
    (let ((full (org-conf--current-local-hash)))
      (save-restriction
        (narrow-to-region
         (point-min)
         (save-excursion
           (goto-char (point-min))
           (search-forward "* B")
           (line-beginning-position)))
        (should (equal full (org-conf--current-local-hash)))))))

(ert-deftest ox-conf-test-split-parameter-block-handles-self-closing ()
  (let ((split (org-conf--split-parameter-block
                '("<ac:parameter ac:name=\"x\"/>" "<p>next</p>"))))
    (should (equal '("<ac:parameter ac:name=\"x\"/>") (car split)))
    (should (equal '("<p>next</p>") (cdr split)))))

;;; ----------------------------------------------------------------------
;;; M3: history version picker and three-way ediff.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-version-choices-list-newest-first ()
  (let ((versions '((:number 1 :when "w1" :by "a" :message "first")
                    (:number 2 :when "w2" :by "b" :message ""))))
    (should (equal '("v2  w2  b" "v1  w1  a  — first")
                   (mapcar #'car (org-conf--version-choices versions))))
    (should (equal '(2 1)
                   (mapcar #'cdr (org-conf--version-choices versions))))))

(ert-deftest ox-conf-test-snapshot-ensure-reuses-an-existing-snapshot ()
  "A baseline written by `f a' must not be fetched twice."
  (let ((org-conf-history-directory (make-temp-file "ox-conf-hist" t))
        (calls 0))
    (unwind-protect
        (progn
          (org-conf--snapshot-write "1" 3 "old")
          (cl-letf (((symbol-function 'org-conf-cli-get-version-body)
                     (lambda (&rest _)
                       (setq calls (1+ calls))
                       (error "must not fetch"))))
            (should (equal "old" (org-conf--snapshot-ensure "1" 3)))
            (should (equal 0 calls))))
      (delete-directory org-conf-history-directory t))))

(ert-deftest ox-conf-test-snapshot-ensure-fetches-and-stores-a-missing-version ()
  (let ((org-conf-history-directory (make-temp-file "ox-conf-hist" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'org-conf-cli-get-version-body)
                   (lambda (_id version)
                     (list :ok t :version version :storage "hist"))))
          (should (equal "hist" (org-conf--snapshot-ensure "1" 2)))
          (should (equal "hist" (org-conf--snapshot-read "1" 2))))
      (delete-directory org-conf-history-directory t))))

(ert-deftest ox-conf-test-history-show-diffs-version-local-and-remote ()
  (let ((org-conf-history-directory (make-temp-file "ox-conf-hist" t))
        (args nil))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "#+TITLE: T\n#+CONF_ID: 1\n\nBody text\n")
          (cl-letf (((symbol-function 'org-conf--snapshot-ensure)
                     (lambda (_page version)
                       (format "<p>hist %s</p>" version)))
                    ((symbol-function 'org-conf-cli-get-current-body)
                     (lambda (_page)
                       (list :ok t :version 9 :storage "<p>current</p>")))
                    ((symbol-function 'ediff-files3)
                     (lambda (a b c) (setq args (list a b c)))))
            (org-conf-history-show "1" 2)
            (should (equal 3 (length args)))
            (let ((texts (mapcar (lambda (file)
                                   (with-temp-buffer
                                     (insert-file-contents file)
                                     (buffer-string)))
                                 args)))
              (should (string-match-p "hist 2" (nth 0 texts)))
              (should (string-match-p "Body text" (nth 1 texts)))
              (should (string-match-p "current" (nth 2 texts))))))
      (delete-directory org-conf-history-directory t))))

;;; ----------------------------------------------------------------------
;;; M4: inline comment marker extraction and re-injection.
;;; ----------------------------------------------------------------------

(ert-deftest ox-conf-test-extract-markers-reads-ref-and-text ()
  (let ((storage (concat "<p><ac:inline-comment-marker ac:ref=\"u1\">函数性质</ac:inline-comment-marker></p>"
                         "<p><ac:inline-comment-marker ac:ref=\"u2\">第二处</ac:inline-comment-marker></p>")))
    (should (equal '((:ref "u1" :text "函数性质")
                     (:ref "u2" :text "第二处"))
                   (org-conf--extract-markers storage)))))

(ert-deftest ox-conf-test-extract-markers-ignores-plain-text ()
  (should (null (org-conf--extract-markers "<p>no markers here</p>"))))

(ert-deftest ox-conf-test-reinject-one-match-wraps-it ()
  (let* ((result (org-conf--reinject-markers
                  "<p>Hello anchor.</p>"
                  '((:ref "u1" :text "anchor")))))
    (should (equal (concat "<p>Hello <ac:inline-comment-marker ac:ref=\"u1\">"
                           "anchor</ac:inline-comment-marker>.</p>")
                   (car result)))
    (should (equal '((:ref "u1" :status injected)) (cdr result)))))

(ert-deftest ox-conf-test-reinject-zero-match-is-ambiguous ()
  (let* ((result (org-conf--reinject-markers
                  "<p>nothing here</p>"
                  '((:ref "u1" :text "anchor")))))
    (should (equal "<p>nothing here</p>" (car result)))
    (should (equal '((:ref "u1" :status ambiguous-zero)) (cdr result)))))

(ert-deftest ox-conf-test-reinject-multiple-matches-is-ambiguous ()
  (let* ((result (org-conf--reinject-markers
                  "<p>anchor and anchor</p>"
                  '((:ref "u1" :text "anchor")))))
    (should (equal "<p>anchor and anchor</p>" (car result)))
    (should (equal '((:ref "u1" :status ambiguous-multiple :count 2))
                   (cdr result)))))

(ert-deftest ox-conf-test-reinject-two-distinct-markers ()
  (let* ((result (org-conf--reinject-markers
                  "<p>alpha beta</p>"
                  '((:ref "u1" :text "alpha")
                    (:ref "u2" :text "beta")))))
    (should (equal (concat "<p><ac:inline-comment-marker ac:ref=\"u1\">alpha</ac:inline-comment-marker> "
                           "<ac:inline-comment-marker ac:ref=\"u2\">beta</ac:inline-comment-marker></p>")
                   (car result)))
    (should (equal '((:ref "u1" :status injected)
                     (:ref "u2" :status injected))
                   (cdr result)))))

(ert-deftest ox-conf-test-reinject-formatted-marker-is-ambiguous ()
  "Inline markup that the export renders differently cannot be re-anchored."
  (let* ((markers (org-conf--extract-markers
                   (concat "<p><ac:inline-comment-marker ac:ref=\"u\">"
                           "<strong>bold</strong>"
                           "</ac:inline-comment-marker></p>")))
         (result (org-conf--reinject-markers "<p><b>bold</b></p>" markers)))
    (should (null (string-match-p "ac:inline-comment-marker" (car result))))
    (should (equal 'ambiguous-zero
                   (plist-get (car (cdr result)) :status)))))

(ert-deftest ox-conf-test-protect-markers-passes-through-when-none-exist ()
  (should (equal "<p>x</p>"
                 (org-conf--protect-markers "<p>remote</p>" "<p>x</p>"))))

(ert-deftest ox-conf-test-protect-markers-restores-the-anchor ()
  (should (string-match-p
           "ac:inline-comment-marker ac:ref=\"u1\""
           (org-conf--protect-markers
            "<p><ac:inline-comment-marker ac:ref=\"u1\">anchor</ac:inline-comment-marker></p>"
            "<p>anchor</p>"))))

(ert-deftest ox-conf-test-marker-loss-policy-error-refuses ()
  (let ((org-conf-marker-loss-policy 'error))
    (should-error
     (org-conf--protect-markers
      "<p><ac:inline-comment-marker ac:ref=\"u1\">anchor</ac:inline-comment-marker></p>"
      "<p>changed</p>")
     :type 'user-error)))

(ert-deftest ox-conf-test-marker-loss-policy-warn-continues ()
  (let ((org-conf-marker-loss-policy 'warn))
    (should (equal "<p>changed</p>"
                   (org-conf--protect-markers
                    "<p><ac:inline-comment-marker ac:ref=\"u1\">anchor</ac:inline-comment-marker></p>"
                    "<p>changed</p>")))))

(ert-deftest ox-conf-test-marker-loss-policy-allow-continues ()
  (let ((org-conf-marker-loss-policy 'allow))
    (should (equal "<p>changed</p>"
                   (org-conf--protect-markers
                    "<p><ac:inline-comment-marker ac:ref=\"u1\">anchor</ac:inline-comment-marker></p>"
                    "<p>changed</p>")))))

(ert-deftest ox-conf-test-marker-loss-policy-error-allows-a-clean-publish ()
  "No loss means the strict default must not block publishing."
  (let ((org-conf-marker-loss-policy 'error))
    (should (string-match-p
             "ac:inline-comment-marker"
             (org-conf--protect-markers
              "<p><ac:inline-comment-marker ac:ref=\"u1\">anchor</ac:inline-comment-marker></p>"
              "<p>anchor</p>")))))

;;; ----------------------------------------------------------------------
;;; M4 follow-up: fail closed when the live body cannot be read.
;;; ----------------------------------------------------------------------

(defun ox-conf-test--with-snapshot (version storage body)
  "Run BODY with a snapshot of VERSION STORAGE and a failing CLI read."
  (with-temp-buffer
    (org-mode)
    (insert (format "#+TITLE: T\n#+CONF_ID: 1\n#+CONF_VERSION: %s\n\nBody\n" version))
    (let ((org-conf-history-directory (make-temp-file "ox-conf-hist" t)))
      (unwind-protect
          (progn
            (when storage
              (org-conf--snapshot-write "1" version storage))
            (cl-letf (((symbol-function 'org-conf-cli-get-current-body)
                       (lambda (_id)
                         (list :ok nil :status 404 :code "NOT_FOUND"))))
              (funcall body)))
        (delete-directory org-conf-history-directory t)))))

(ert-deftest ox-conf-test-protect-output-fails-closed-on-snapshot-markers ()
  (let ((org-conf-marker-loss-policy 'error))
    (ox-conf-test--with-snapshot
     5
     (concat "<p><ac:inline-comment-marker ac:ref=\"u1\">"
             "anchor</ac:inline-comment-marker></p>")
     (lambda ()
       (should-error (org-conf--protect-publish-output "1" "<p>x</p>")
                     :type 'user-error)))))

(ert-deftest ox-conf-test-protect-output-continues-when-snapshot-has-no-markers ()
  (let ((org-conf-marker-loss-policy 'error))
    (ox-conf-test--with-snapshot
     5 "<p>no anchors</p>"
     (lambda ()
       (should (equal "<p>x</p>"
                      (org-conf--protect-publish-output "1" "<p>x</p>")))))))

(ert-deftest ox-conf-test-protect-output-continues-without-a-snapshot ()
  (let ((org-conf-marker-loss-policy 'error))
    (ox-conf-test--with-snapshot
     5 nil
     (lambda ()
       (should (equal "<p>x</p>"
                      (org-conf--protect-publish-output "1" "<p>x</p>")))))))

(ert-deftest ox-conf-test-protect-output-warn-publishes-with-snapshot-markers ()
  (let ((org-conf-marker-loss-policy 'warn))
    (ox-conf-test--with-snapshot
     5
     (concat "<p><ac:inline-comment-marker ac:ref=\"u1\">"
             "anchor</ac:inline-comment-marker></p>")
     (lambda ()
       (should (equal "<p>x</p>"
                      (org-conf--protect-publish-output "1" "<p>x</p>")))))))

(ert-deftest ox-conf-test-protect-output-allow-publishes-with-snapshot-markers ()
  (let ((org-conf-marker-loss-policy 'allow))
    (ox-conf-test--with-snapshot
     5
     (concat "<p><ac:inline-comment-marker ac:ref=\"u1\">"
             "anchor</ac:inline-comment-marker></p>")
     (lambda ()
       (should (equal "<p>x</p>"
                      (org-conf--protect-publish-output "1" "<p>x</p>")))))))

(ert-deftest ox-conf-test-export-refuses-to-put-when-the-live-body-is-unreadable ()
  "error policy + a snapshot with markers must abort before the PUT."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n#+CONF_ID: 1\n#+CONF_VERSION: 5\n\nBody\n")
    (let ((org-conf-history-directory (make-temp-file "ox-conf-hist" t))
          (put nil)
          (org-conf-marker-loss-policy 'error))
      (unwind-protect
          (progn
            (org-conf--snapshot-write
             "1" 5
             (concat "<p><ac:inline-comment-marker ac:ref=\"u1\">"
                     "anchor</ac:inline-comment-marker></p>"))
            (cl-letf (((symbol-function 'org-conf-backend-get-page-info)
                       (lambda (_id) (list :title "T" :version 5)))
                      ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                      ((symbol-function 'read-string) (lambda (&rest _) "msg"))
                      ((symbol-function 'org-export-as)
                       (lambda (&rest _) "<p>anchor</p>"))
                      ((symbol-function 'org-conf-cli-get-current-body)
                       (lambda (_id)
                         (list :ok nil :status 404 :code "NOT_FOUND")))
                      ((symbol-function 'org-conf-backend-update-page)
                       (lambda (&rest args) (setq put args) t)))
              (should-error (org-conf--export-to-conf t) :type 'user-error)
              (should (null put))))
        (delete-directory org-conf-history-directory t)))))

;;; ----------------------------------------------------------------------
;;; M2: comment parsing, threading, sidecar rendering.
;;; ----------------------------------------------------------------------

(defconst ox-conf-test--comments-json
  '((pageId . "1")
    (commentCount . 3)
    (comments .
              (((id . "r1") (parentId . nil) (title . "Re: T")
                (location . "inline") (resolution . "open")
                (body . "<p>root</p>") (bodyText . "root")
                (createdAt . "2026-11-12T10:00:00") (author . ((displayName . "Tao")))
                (inlineProperties . ((originalSelection . "anchor") (markerRef . "u1"))))
               ((id . "c1") (parentId . "r1") (title . "Re: T")
                (location . "footer") (resolution . "open")
                (body . "<p>reply</p>") (bodyText . "reply")
                (createdAt . "2026-11-12T10:01:00") (author . ((displayName . "Tao")))
                (inlineProperties . nil))
               ((id . "r2") (parentId . nil) (title . "Re: T")
                (location . "inline") (resolution . "dangling")
                (body . "<p>old</p>") (bodyText . "old")
                (createdAt . "2026-11-12T09:00:00") (author . ((displayName . "Tao")))
                (inlineProperties . ((originalSelection . "gone") (markerRef . "u2"))))))))

(ert-deftest ox-conf-test-parse-comment-extracts-the-sidecar-facts ()
  (let ((comment (org-conf--parse-comment
                  (nth 0 (alist-get 'comments ox-conf-test--comments-json)))))
    (should (equal "r1" (plist-get comment :id)))
    (should (null (plist-get comment :parent-id)))
    (should (equal "inline" (plist-get comment :location)))
    (should (equal "open" (plist-get comment :resolution)))
    (should (equal "root" (plist-get comment :body-text)))
    (should (equal "Tao" (plist-get comment :author)))
    (should (equal "u1" (plist-get comment :marker-ref)))
    (should (equal "anchor" (plist-get comment :anchor-text)))))

(ert-deftest ox-conf-test-aggregate-comments-groups-replies-by-parent ()
  (let* ((comments (mapcar #'org-conf--parse-comment
                           (alist-get 'comments ox-conf-test--comments-json)))
         (threads (org-conf--aggregate-comments comments)))
    (should (equal '("r1" "r2")
                   (mapcar (lambda (thread)
                             (plist-get (plist-get thread :root) :id))
                           threads)))
    (should (equal '("c1")
                   (mapcar (lambda (reply) (plist-get reply :id))
                           (plist-get (nth 0 threads) :replies))))
    (should (null (plist-get (nth 1 threads) :replies)))))

(ert-deftest ox-conf-test-comment-thread-status ()
  (should (eq 'dangling
              (org-conf--comment-thread-status
               '(:location "inline" :resolution "dangling" :marker-ref "u")
               '("u") t)))
  (should (eq 'dangling
              (org-conf--comment-thread-status
               '(:location "inline" :resolution nil :marker-ref "u")
               '() t)))
  (should (eq 'open
              (org-conf--comment-thread-status
               '(:location "inline" :resolution nil :marker-ref "u")
               '("u") t)))
  (should (eq 'open
              (org-conf--comment-thread-status
               '(:location "footer" :resolution nil :marker-ref nil)
               '() t)))
  ;; When the body could not be read, only the server's resolution is trusted.
  (should (eq 'open
              (org-conf--comment-thread-status
               '(:location "inline" :resolution nil :marker-ref "u")
               '() nil))))

(ert-deftest ox-conf-test-org-body-escaping-round-trips ()
  (let* ((text "* not a heading\n#+not: a keyword\nplain")
         (escaped (org-conf--org-escape-body text)))
    (should (equal ",* not a heading\n,#+not: a keyword\nplain" escaped))
    (should (equal text (org-conf--org-unescape-body escaped)))))

(ert-deftest ox-conf-test-org-text-to-storage-escapes-and-wraps ()
  (should (equal "<p>a &lt;b&gt; &amp; c</p>\n<p>second</p>"
                 (org-conf--org-text-to-storage "a <b> & c\n\nsecond"))))

(ert-deftest ox-conf-test-comments-render-writes-properties-and-replies ()
  (let* ((comments (mapcar #'org-conf--parse-comment
                           (alist-get 'comments ox-conf-test--comments-json)))
         (rendered (org-conf--comments-render
                    "1" "T" comments '("u1") t nil)))
    (should (string-match-p "^#\\+CONF_PAGE_ID: 1$" rendered))
    (should (string-match-p "^\\* Re: T  :inline:$" rendered))
    (should (string-match-p "^:CONF_COMMENT_ID: r1$" rendered))
    (should (string-match-p "^:CONF_STATUS: open$" rendered))
    (should (string-match-p "^:CONF_ANCHOR_TEXT: anchor$" rendered))
    (should (string-match-p "^:CONF_STATUS: dangling$" rendered))
    (should (string-match-p "^\\*\\* Re: T  :reply:$" rendered))
    (should (string-match-p "^:CONF_PARENT_ID: r1$" rendered))))

(ert-deftest ox-conf-test-comments-render-preserves-answered ()
  (let* ((comments (mapcar #'org-conf--parse-comment
                           (alist-get 'comments ox-conf-test--comments-json)))
         (rendered (org-conf--comments-render
                    "1" "T" comments '("u1") t '(("r1" . answered)))))
    (should (string-match-p "^:CONF_STATUS: answered$" rendered))))

(ert-deftest ox-conf-test-comments-preserved-only-keeps-answered ()
  (let ((file (make-temp-file "ox-conf-comments" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "* T\n:PROPERTIES:\n:CONF_COMMENT_ID: r1\n:CONF_STATUS: answered\n:END:\n")
            (insert "* U\n:PROPERTIES:\n:CONF_COMMENT_ID: r2\n:CONF_STATUS: dangling\n:END:\n"))
          (should (equal '(("r1" . answered))
                         (org-conf--comments-preserved file))))
      (delete-file file))))

(ert-deftest ox-conf-test-comment-drafts-reads-id-less-replies ()
  (with-temp-buffer
    (org-mode)
    (insert "* Thread  :inline:\n"
            ":PROPERTIES:\n:CONF_COMMENT_ID: r1\n:CONF_PARENT_ID:\n:END:\n"
            "root body\n\n"
            "** Reply  :reply:\n"
            ":PROPERTIES:\n:CONF_COMMENT_ID:\n:CONF_PARENT_ID: r1\n:END:\n"
            "my reply\n")
    (let ((drafts (org-conf--comment-drafts-in-buffer)))
      (should (equal 1 (length drafts)))
      (should (equal "r1" (plist-get (car drafts) :parent-id)))
      (should (equal "my reply" (plist-get (car drafts) :body))))))

(ert-deftest ox-conf-test-post-comment-reply-records-the-returned-id ()
  (with-temp-buffer
    (org-mode)
    (insert "** Reply  :reply:\n"
            ":PROPERTIES:\n:CONF_COMMENT_ID:\n:CONF_PARENT_ID: r1\n:END:\n"
            "hello <world>\n")
    (goto-char (point-min))
    (let ((posted nil)
          (uploaded nil)
          (draft (list :point (point-min) :parent-id "r1" :body "hello <world>")))
      (cl-letf (((symbol-function 'org-conf--cli-json-safe)
                 (lambda (&rest args)
                   (setq posted args)
                   (let ((file (nth (1+ (cl-position "-f" args :test #'equal)) args)))
                     (with-temp-buffer
                       (insert-file-contents file)
                       (setq uploaded (buffer-string))))
                   (list :ok t :data '((id . "new-1"))))))
        (should (equal "new-1" (org-conf--post-comment-reply "1" draft))))
      (should (equal '("comment" "1" "--parent" "r1") (seq-take posted 4)))
      (should (equal "<p>hello &lt;world&gt;</p>" uploaded))
      (should (equal "new-1" (org-entry-get nil "CONF_COMMENT_ID")))
      (should (equal "answered" (org-entry-get nil "CONF_STATUS"))))))

;;; ----------------------------------------------------------------------
;;; Export-layer transcoders.
;;; These drive the real exporter so element parsing, transcoding and
;;; filters are exercised together.
;;; ----------------------------------------------------------------------

(defun ox-conf-test--export (body &optional options)
  "Export BODY through `org-conf-html' and return the string.
OPTIONS, when non-nil, is inserted as a #+OPTIONS: line.  Uploads are
disabled so the test never touches Confluence."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: T\n")
    (when options
      (insert (format "#+OPTIONS: %s\n" options)))
    (insert "\n" body "\n")
    (org-export-as 'org-conf-html nil nil nil
                   '(:org-conf-do-post nil :org-conf-id "1"))))

(ert-deftest ox-conf-test-src-block-maps-a-known-language ()
  (let ((output (ox-conf-test--export
                 "#+begin_src python\nprint(1)\n#+end_src"
                 "num:nil toc:nil")))
    (should (string-match-p "ac:name=\"code\"" output))
    (should (string-match-p "ac:name=\"language\">python<" output))
    (should (string-match-p "print(1)" output))))

(ert-deftest ox-conf-test-src-block-maps-asm-to-c ()
  (should (string-match-p
           "ac:name=\"language\">c<"
           (ox-conf-test--export "#+begin_src asm\nmov eax, 1\n#+end_src"
                                 "num:nil toc:nil"))))

(ert-deftest ox-conf-test-src-block-maps-rs-to-bash-with-a-host-prompt ()
  (let ((output (ox-conf-test--export
                 "#+begin_src rs :host example.com :path /tmp\nls\n#+end_src"
                 "num:nil toc:nil")))
    (should (string-match-p "ac:name=\"language\">bash<" output))
    (should (string-match-p (regexp-quote "example.com:/tmp $ ls") output))))

(ert-deftest ox-conf-test-src-block-maps-an-unknown-language-to-none ()
  (should (string-match-p
           "ac:name=\"language\">none<"
           (ox-conf-test--export "#+begin_src made-up\nx\n#+end_src"
                                 "num:nil toc:nil"))))

(ert-deftest ox-conf-test-src-block-renders-the-caption-and-collapse ()
  (let ((output (ox-conf-test--export
                 (concat "#+caption: My code\n"
                         "#+begin_src python :collapse t\nx\n#+end_src")
                 "num:nil toc:nil")))
    (should (string-match-p "ac:name=\"title\">My code<" output))
    (should (string-match-p "ac:name=\"collapse\">true<" output))))

(ert-deftest ox-conf-test-src-block-jira-macro ()
  (let ((output (ox-conf-test--export
                 "#+begin_src jira :limit 5 :columns key,summary\nproject = ABC\n#+end_src"
                 "num:nil toc:nil")))
    (should (string-match-p "ac:name=\"jira\"" output))
    (should (string-match-p "ac:name=\"maximumIssues\">5<" output))
    (should (string-match-p "ac:name=\"columns\">key,summary<" output))
    (should (string-match-p "ac:name=\"jqlQuery\">project = ABC<" output))))

(ert-deftest ox-conf-test-src-block-jirachart-pie ()
  (let ((output (ox-conf-test--export
                 "#+begin_src jirachart-pie :showinfor t :stat-type statuses\nproject = ABC\n#+end_src"
                 "num:nil toc:nil")))
    (should (string-match-p "ac:name=\"jirachart\"" output))
    (should (string-match-p "ac:name=\"chartType\">pie<" output))
    (should (string-match-p "ac:name=\"showinfor\">true<" output))
    (should (string-match-p "ac:name=\"statType\">statuses<" output))))

(ert-deftest ox-conf-test-src-block-jirachart-2d ()
  (let ((output (ox-conf-test--export
                 "#+begin_src jirachart-2d :limit 5 :xstat-type statuses :ystat-type labels\nproject = ABC\n#+end_src"
                 "num:nil toc:nil")))
    (should (string-match-p "ac:name=\"chartType\">twodimensional<" output))
    (should (string-match-p "ac:name=\"numberToShow\">5<" output))
    (should (string-match-p "ac:name=\"xstattype\">statuses<" output))
    (should (string-match-p "ac:name=\"ystattype\">labels<" output))))

(ert-deftest ox-conf-test-special-block-panels ()
  (dolist (case '(("warning" . "[Warning]")
                  ("note" . "[Note]")
                  ("tip" . "[Tip]")
                  ("expand" . "[Click to expand...]")
                  ("info" . "[Info]")))
    (let ((output (ox-conf-test--export
                   (format "#+begin_%s\nbody\n#+end_%s" (car case) (car case))
                   "num:nil toc:nil")))
      (should (string-match-p (format "ac:name=\"%s\"" (car case)) output))
      (should (string-match-p
               (regexp-quote (format "ac:name=\"title\">%s<" (cdr case)))
               output))
      (should (string-match-p "body" output)))))

(ert-deftest ox-conf-test-special-block-history-limit ()
  (let ((default (ox-conf-test--export "#+begin_history\n#+end_history"
                                       "num:nil toc:nil"))
        (custom (ox-conf-test--export "#+begin_history :limit 5\n#+end_history"
                                      "num:nil toc:nil")))
    (should (string-match-p "ac:name=\"change-history\"" default))
    (should (string-match-p "ac:name=\"limit\">3<" default))
    (should (string-match-p "ac:name=\"limit\">5<" custom))))

(ert-deftest ox-conf-test-special-block-section-border ()
  (should (string-match-p
           "ac:name=\"border\">true<"
           (ox-conf-test--export "#+begin_section\nx\n#+end_section"
                                 "num:nil toc:nil")))
  (should (string-match-p
           "ac:name=\"border\">false<"
           (ox-conf-test--export "#+begin_section :border no\nx\n#+end_section"
                                 "num:nil toc:nil")))
  (should (string-match-p
           "ac:name=\"border\">true<"
           (ox-conf-test--export "#+begin_section :border yes\nx\n#+end_section"
                                 "num:nil toc:nil"))))

(ert-deftest ox-conf-test-special-block-column-width ()
  (let ((with-width (ox-conf-test--export
                     "#+begin_column :width 50%\nx\n#+end_column"
                     "num:nil toc:nil"))
        (without (ox-conf-test--export
                  "#+begin_column\nx\n#+end_column"
                  "num:nil toc:nil")))
    (should (string-match-p "ac:name=\"width\">50%<" with-width))
    (should-not (string-match-p "ac:name=\"width\"" without))))

(ert-deftest ox-conf-test-special-block-unknown-passes-contents-through ()
  (let ((output (ox-conf-test--export "#+begin_foo\nkeep me\n#+end_foo"
                                      "num:nil toc:nil")))
    (should (string-match-p "keep me" output))
    (should-not (string-match-p "ac:structured-macro" output))))

(ert-deftest ox-conf-test-headline-numbering-macro ()
  (let ((on (ox-conf-test--export "* H\nbody" "num:t toc:nil"))
        (off (ox-conf-test--export "* H\nbody" "num:nil toc:nil")))
    (should (string-match-p "ac:name=\"numberedheadings\"" on))
    (should (string-match-p "ac:name=\"start-numbering-with\">1<" on))
    (should-not (string-match-p "numberedheadings" off))
    (should (string-match-p "<h1>H</h1>" off))))

(ert-deftest ox-conf-test-tags-map-to-status-colours ()
  (dolist (case '(("新增" . "Green")
                  ("修改" . "Yellow")
                  ("删除" . "Red")
                  ("复制" . "Blue")
                  ("其他" . "Grey")))
    (let ((output (ox-conf-test--export
                   (format "* H :%s:\nbody" (car case))
                   "num:nil toc:nil")))
      (should (string-match-p "ac:name=\"status\"" output))
      (should (string-match-p
               (format "ac:name=\"colour\">%s<" (cdr case)) output))
      (should (string-match-p
               (regexp-quote (format "ac:name=\"title\">%s<" (car case)))
               output)))))

(ert-deftest ox-conf-test-toc-macro-content ()
  (let ((output (org-conf-toc 2 nil)))
    (should (string-match-p "<h1>目录</h1>" output))
    (should (string-match-p "ac:name=\"toc\"" output))
    (should (string-match-p "ac:name=\"maxLevel\">2<" output))
    (should (string-match-p (regexp-quote "ac:name=\"exclude\">^目录$<")
                            output))))

(ert-deftest ox-conf-test-toc-depth-t-becomes-seven ()
  (should (string-match-p "ac:name=\"maxLevel\">7<" (org-conf-toc t nil))))

(ert-deftest ox-conf-test-latex-fragment-becomes-mathinline ()
  (let ((output (ox-conf-test--export "inline $x^2$ here" "num:nil toc:nil")))
    (should (string-match-p "ac:name=\"mathinline\"" output))
    (should (string-match-p (regexp-quote ">x^2") output))))

(ert-deftest ox-conf-test-latex-environment-becomes-mathblock ()
  (let ((output (ox-conf-test--export
                 (concat "\\begin{equation}\n"
                         "x^2\n"
                         "\\end{equation}")
                 "num:nil toc:nil")))
    (should (string-match-p "ac:name=\"mathblock\"" output))
    (should (string-match-p "x\\^2" output))))

(ert-deftest ox-conf-test-export-block-becomes-html-macro ()
  (let ((output (ox-conf-test--export
                 "#+begin_export html\n<div>x</div>\n#+end_export"
                 "num:nil toc:nil")))
    (should (string-match-p "ac:name=\"html\"" output))
    (should (string-match-p (regexp-quote "<div>x</div>") output))))

(ert-deftest ox-conf-test-link-external-with-and-without-description ()
  (should (string-match-p
           "<a href=\"https://example.com\">desc</a>"
           (ox-conf-test--export "[[https://example.com][desc]]"
                                 "num:nil toc:nil")))
  (should (string-match-p
           "<a href=\"https://example.com\">https://example.com</a>"
           (ox-conf-test--export "[[https://example.com]]"
                                 "num:nil toc:nil"))))

(ert-deftest ox-conf-test-link-image-becomes-attachment-macro ()
  (let ((output (ox-conf-test--export "[[file:img.png]]"
                                      "num:nil toc:nil html-inline-images:t")))
    (should (string-match-p "<ac:image ac:width=\"80%\">" output))
    (should (string-match-p "ri:filename=\"img.png\"" output))))

(ert-deftest ox-conf-test-link-image-width-attribute ()
  (let ((output (ox-conf-test--export
                 "#+attr_html: :width 300\n[[file:img.png]]"
                 "num:nil toc:nil html-inline-images:t")))
    (should (string-match-p "ac:width=\"300\"" output))))

(ert-deftest ox-conf-test-target-becomes-anchor-macro ()
  (let ((output (ox-conf-test--export "<<my-anchor>>" "num:nil toc:nil")))
    (should (string-match-p "ac:name=\"anchor\"" output))))

(ert-deftest ox-conf-test-center-block-centers-paragraphs ()
  (let ((output (ox-conf-test--export
                 "#+begin_center\nhello\n#+end_center"
                 "num:nil toc:nil")))
    (should (string-match-p "<p style=\"text-align: center;\">" output))
    (should (string-match-p "hello" output))))

(ert-deftest ox-conf-test-template-is-a-passthrough ()
  (should (equal "<p>x</p>" (org-conf-template "<p>x</p>" nil))))

;;; ox-conf-test.el ends here
