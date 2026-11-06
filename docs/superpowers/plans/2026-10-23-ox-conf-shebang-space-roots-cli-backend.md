# ox-conf: Space-Root Parsing, Shebang-Aware CONF Keywords, and confluence-cli Backend

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix the empty parent-page picker and the CONF keywords landing above the shebang, then add a `confluence` CLI backend behind a forward-compatible dispatch seam while keeping the existing REST path intact.

**Architecture:** Three independent fixes in `ox-conf.el`, each landed with tests:
1. Normalize Confluence's grouped space-content JSON (`{page: {results: …}}`) so the parent picker actually lists remote pages.
2. Make front-matter detection skip a leading shebang / Emacs mode line / org comments before finding the `#+` keyword block, so `CONF_*` lands after `#+TITLE:` and friends.
3. Introduce a small backend registry (`org-conf-backend` = `auto` / `rest` / `cli`) with seven stable operations. REST keeps its current `request.el` implementation; the CLI backend shells out to the user's configured `confluence` executable with `call-process` and parses its `--json` output. This is the seam a later refactor can split into separate front-end/back-end files.

**Tech Stack:** Emacs Lisp (Emacs 27+ APIs), ERT, existing `request` dependency, `confluence` CLI 2.29.0 (verified installed at `d:/MyProgramFiles/npm-global/confluence.cmd`).

**Spec:** Inline "Design Decisions" below, derived from the user's 2026-10-23 request:
- parent picker must read remote pages instead of showing only the root/manual placeholders;
- `CONF_ID` / `CONF_SPACE` / `CONF_PAGE` stay `#+` keywords and must sit after the reserved keyword block, never above the shebang;
- add a confluence-cli backend, forward compatible, with a path to later decouple front and back ends;
- develop and verify in a worktree; midnight start; read-only live verification only.

## Global Constraints

- All work happens in a git worktree created from the current working-tree state (see Task 0). Do not modify the main checkout.
- No new Emacs package dependencies. `request` stays as-is.
- `CONF_*` remains `#+` keyword syntax. Do **not** move it into a property drawer.
- Placement rule: after leading shebang / `# -*-- … -*-` / org comment lines, then after the **entire** leading `#+` keyword block.
- `org-conf-backend` default is `auto`; `auto` resolves to `cli` only when `(executable-find org-conf-cli-program)` finds the CLI, otherwise `rest`.
- All CLI invocations use `call-process` with an argument list. Never build a shell command string.
- CLI I/O is UTF-8 (`coding-system-for-read` / `coding-system-for-write` bound to `utf-8-unix`).
- Page bodies are passed to the CLI through a temporary file via `--file`, never as a command-line argument (Windows command-line length limit).
- Verification is **read-only**: no page creation, update, deletion, or attachment upload against the real Confluence.
- Test command: `emacs -Q --batch -L test -l test/run-tests.el`.
- Commit after every task.

## Review Focus

These are the input classes most likely to break a real user, each pinned by a test in the task that owns the code:

1. **Grouped space-content response** — `/rest/api/space/{key}/content?depth=root` returns `{page: {results: […]}}`, not a top-level `results`; pagination may hang off `page._links.next`. Task 1.
2. **Files that start with a mode line / shebang and a blank line** — `# -*- … -*-`, `#!/usr/bin/env …`, or `# comment`, optionally followed by blank lines, then `#+TITLE`. The CONF block must not go above them. Task 2.
3. **Non-ASCII titles and paths through the Windows CLI** — Chinese page titles must survive as command arguments; verified live with `confluence find "Claude Code 备忘录"`. Task 4 and Task 6.
4. **Large storage bodies** — must go through `--file`, not `--content`, because Windows caps the command line. Task 5.
5. **CLI missing, non-zero exit, or invalid JSON** — `auto` must fall back to REST when absent; when selected explicitly, failures must raise a clear error instead of silently reporting success. Task 3 and Task 4.

---

## Design Decisions

### D1 — Space-root listing

`org-conf--parse-child-page-json` currently reads only `(assoc 'results json)`. Confluence Server's space-content endpoint returns content grouped by type:

```json
{"page": {"results": [{"id": "1", "title": "Root"}], "_links": {"self": "…"}},
 "blogpost": {"results": []}}
```

Add two pure helpers that understand both the flat shape used by `/content/{id}/child/page` and the grouped shape:

- `org-conf--page-json-results` → list of entry alists
- `org-conf--page-json-next` → next-page href, from the top level or from `page._links.next`

`org-conf--fetch-all-pages` and `org-conf--parse-child-page-json` use them.

### D2 — Front-matter placement

`org-conf--keyword-block-end` returns `point-min` whenever the first line is not `#+`, which is why a `# -*- … -*-` line pushes the CONF block above it. Add `org-conf--front-matter-start`:

- skip blank lines, `#!` shebangs, `# -*- … -*-` mode lines, and org comments (`#` followed by space/tab or end of line);
- stop at the first `#+` keyword or body line;
- the keyword block is then scanned from there with the existing `org-at-keyword-p` loop.

Stale `#+CONF_*` lines are still removed first (whole buffer), then the block is inserted at the computed end. The reader is unchanged: `org-export--get-inbuffer-options` already sees `#+` keywords.

### D3 — Backend seam

```elisp
(defcustom org-conf-backend 'auto
  :type '(choice (const :tag "Auto (CLI when installed)" auto)
                 (const :tag "REST API (request.el)" rest)
                 (const :tag "confluence CLI" cli)))
(defcustom org-conf-cli-program "confluence" …)
```

Seven operations, all returning plain plists/lists:

| Operation | Signature | Returns |
|---|---|---|
| get-page-info | `(page-id)` | `(:id ID :title T :version N :space KEY :parent-id ID :url URL)` |
| get-children | `(page-id)` | list of `(:id ID :title T)` |
| get-space-roots | `(space)` | list of `(:id ID :title T)` |
| create-page | `(title space parent-id)` | `(:id ID :webui PATH-OR-URL)` |
| update-page | `(page-id title space body version message)` | non-nil on success |
| list-attachments | `(page-id)` | list of `(:id ID :title NAME)` |
| upload-attachment | `(page-id filepath &optional attach-id)` | non-nil on success |

`org-conf--backend-functions` is an alist `(BACKEND . ((OP . FUNCTION) …))`. `org-conf--active-backend` resolves `auto`. Public dispatchers are `org-conf-backend-<op>`. Adding a backend later is one alist entry; moving the CLI functions to a separate file is a mechanical cut.

### D4 — CLI contract

- Invocation: `confluence --json <command> <args…>` via `call-process`.
- Success: exit 0, stdout is one JSON object, parsed with `json-parse-buffer :object-type 'alist :array-type 'list`.
- Failure: non-zero exit; stderr is a JSON error object `{error, code, status, details}`. `org-conf--cli-json` raises `user-error` containing `error` when present, otherwise the raw stderr text.
- Bodies go to a UTF-8 temp file (`--file`), deleted in `unwind-protect`.
- `api` calls use a path relative to the CLI's configured `apiPath`: strip a leading `/rest/api/` (and any leading slash) from the existing REST API path. `next` links are absolute same-origin URLs and are passed through unchanged.
- CLI `update` derives the version itself; `version` and `message` are accepted and ignored. `space` is accepted and ignored.

### D5 — Verification

Unit tests cover all parsing, placement, dispatch, command construction, and failure paths. Live verification is a separate batch Emacs script that only calls read operations against `https://conf.hygon.cn/` for page `283548469` and space `SSC`, under both `cli` and `rest`.

---

## File Structure

- `ox-conf.el` — production code. New sections:
  - front-matter helpers next to `org-conf--keyword-block-end`
  - grouped-JSON helpers next to `org-conf--parse-child-page-json`
  - backend options near the other `defcustom`s
  - backend registry + REST adapters after the REST section
  - CLI process layer + CLI operations after the REST section
- `test/ox-conf-test.el` — all new ERT tests, grouped by the section they exercise.
- `README.org`, `CLAUDE.md` — document `org-conf-backend`, `org-conf-cli-program`, and the new placement rule.

No file split yet; the registry is the seam for the later front-end/back-end split.

---

### Task 0: Worktree and baseline

**Files:** none (repository setup).

**Interfaces:**
- Consumes: current working tree of `C:/Users/zhongtao/.emacs.d/forOrgs/myscripts/ox-conf` (HEAD `6595653` plus uncommitted `ox-conf.el`, `README.org`, `CLAUDE.md`, `test/`).
- Produces: a worktree whose baseline tests pass before any feature work.

- [ ] **Step 1: Create the worktree**

Use the `superpowers:using-git-worktrees` skill. With `EnterWorktree`, create name `ox-conf-cli-backend`. Record the worktree path as `$WT` (normally `.claude/worktrees/ox-conf-cli-backend` under the repository).

- [ ] **Step 2: Import the current working-tree state**

The parent-page feature and its tests exist only as uncommitted changes, so the worktree must receive them:

```bash
SRC=/c/Users/zhongtao/.emacs.d/forOrgs/myscripts/ox-conf
cp "$SRC/ox-conf.el" "$SRC/README.org" "$SRC/CLAUDE.md" "$WT/"
cp -r "$SRC/test" "$WT/"
mkdir -p "$WT/docs/superpowers/plans"
cp "$SRC/docs/superpowers/plans/2026-10-23-ox-conf-shebang-space-roots-cli-backend.md" \
   "$WT/docs/superpowers/plans/"
```

Do **not** copy `.memsearch/` or `ox-conf.el.bak`.

- [ ] **Step 3: Run the baseline suite in the worktree**

Run: `cd "$WT" && emacs -Q --batch -L test -l test/run-tests.el`

Expected: all existing ERT tests pass. If any fail, stop and report — the baseline is not clean.

- [ ] **Step 4: Commit the imported baseline**

```bash
git add ox-conf.el README.org CLAUDE.md test docs/superpowers/plans
git commit -m "chore: import current working tree as feature baseline"
```

---

### Task 1: Parse grouped space-root responses

**Files:**
- Modify: `ox-conf.el` (`org-conf--parse-child-page-json`, `org-conf--fetch-all-pages`, add two helpers near line 952)
- Test: `test/ox-conf-test.el` (add after the existing child-page JSON tests, around line 131)

**Interfaces:**
- Consumes: parsed JSON alists with symbol keys, as produced by `json-read`.
- Produces:
  - `(org-conf--page-json-results JSON)` → list of entry alists
  - `(org-conf--page-json-next JSON)` → string or nil
  - `org-conf--parse-child-page-json` keeps its name and return type.

- [ ] **Step 1: Write the failing tests**

```elisp
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
```

- [ ] **Step 2: Run the new tests and verify they fail**

Run: `emacs -Q --batch -L test -l test/run-tests.el -l test/ox-conf-test.el --eval '(ert-run-tests-batch-and-exit "ox-conf-test-parse-space-root\\|ox-conf-test-fetch-all-pages-follows-a-grouped")'`

Expected: FAIL — grouped fixtures yield nil (or the next link is not followed).

- [ ] **Step 3: Implement the helpers and wire them in**

```elisp
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
```

Change `org-conf--parse-child-page-json` to map over `(org-conf--page-json-results json)`, and change `org-conf--fetch-all-pages` to read the next link with `org-conf--page-json-next`. Keep `org-conf--json-link` unchanged.

- [ ] **Step 4: Run the full suite**

Run: `emacs -Q --batch -L test -l test/run-tests.el`

Expected: all tests pass, including the pre-existing flat-shape tests.

- [ ] **Step 5: Commit**

```bash
git add ox-conf.el test/ox-conf-test.el
git commit -m "fix(ox-conf): parse grouped space-root listings"
```

---

### Task 2: Place CONF keywords below the shebang and the reserved keyword block

**Files:**
- Modify: `ox-conf.el` (`org-conf--keyword-block-end` at ~1035, `org-conf--insert-conf-keywords` at ~1048; add `org-conf--front-matter-start`)
- Test: `test/ox-conf-test.el` (the keyword-placement section at ~228-303 and `org-conf--keyword-block-end` section at ~487-529)

**Interfaces:**
- Consumes: the current buffer, in `org-mode`.
- Produces: `(org-conf--front-matter-start)` → position after leading blank lines / shebang / mode line / org comments, before the first keyword or body line. `org-conf--keyword-block-end` keeps its name and return meaning.

- [ ] **Step 1: Write the failing tests**

Add to the keyword-block-end section:

```elisp
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
```

Add to the insertion section:

```elisp
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
```

- [ ] **Step 2: Run the new tests and verify they fail**

Run: `emacs -Q --batch -L test -l test/run-tests.el --eval '(ert-run-tests-batch-and-exit "shebang\\|mode-line\\|skips-blank-lines-before-keywords")'`

Expected: FAIL — the CONF block appears above the mode line / shebang.

- [ ] **Step 3: Implement front-matter detection**

```elisp
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
```

Change `org-conf--keyword-block-end` to start at `(org-conf--front-matter-start)` instead of `point-min`, keeping the existing `org-at-keyword-p` loop and returning the start position when there is no keyword block.

`org-conf--insert-conf-keywords` keeps its current body: remove stale `#+CONF_*` lines, then insert at `(org-conf--keyword-block-end)`.

- [ ] **Step 4: Run the full suite**

Run: `emacs -Q --batch -L test -l test/run-tests.el`

Expected: all pass, including the unchanged tests `...-lands-below-the-title`, `...-follows-the-whole-front-matter`, `...-replaces-stale-ones`, `...-still-works-without-front-matter`, and `...-inserted-keywords-are-readable-by-org`.

- [ ] **Step 5: Commit**

```bash
git add ox-conf.el test/ox-conf-test.el
git commit -m "fix(ox-conf): keep CONF keywords below shebang and front matter"
```

---

### Task 3: Add the backend registry and REST adapter

**Files:**
- Modify: `ox-conf.el`
  - options near the other `defcustom`s (~146)
  - new registry + adapters after the REST section (~1320)
  - call sites: `org-conf--space-roots-or-warn` (~1411), `org-conf--read-parent-by-drill-down` (~1422), `org-conf--read-parent-id` (~1448), `org-conf-create-page-and-export` (~1485), `org-conf-export-to-conf` (~1509), `org-conf-export-to-conf-without-post` (~1545), `org-conf-export-page-info` (~1351), `org-conf-export-img-info` (~1366), `org-conf--upload-img` (~1306)
  - `org-conf-rest-get-page-json` expand string (~1154)
  - `org-conf--page-url` (~1139)
- Test: `test/ox-conf-test.el`

**Interfaces:**
- Consumes: existing `org-conf-rest-*` functions (unchanged signatures).
- Produces: the seven dispatchers from D3, `org-conf--active-backend`, `org-conf--backend-functions`, `org-conf-rest-get-page-info`, `org-conf-rest-list-attachments`.

- [ ] **Step 1: Write the failing tests**

```elisp
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

(ert-deftest ox-conf-test-page-url-passes-an-absolute-url-through ()
  (let ((org-conf-url "http://wiki.example.com/"))
    (should (equal "https://other.example.com/pages/1"
                   (org-conf--page-url "https://other.example.com/pages/1")))))
```

Then update the existing interactive tests to stub the dispatchers instead of REST functions:

- `ox-conf-test-create-page-and-export-wires-everything`: replace the four `org-conf-rest-*` stubs with `org-conf-backend-get-page-info`, `org-conf-backend-get-children`, `org-conf-backend-create-page`, and `org-conf-backend-update-page`. `get-page-info` returns `(list :id id :title "Root" :version 0)` for the parent and `(:id "555" :title "My Doc" :version 0)` for the created page; `update-page` records its args.
- All `ox-conf-test-drill-down-*` tests: stub `org-conf-backend-get-space-roots` and `org-conf-backend-get-children`.
- `ox-conf-test-read-parent-id-*`: same substitution; flat mode uses `org-conf-backend-get-page-info` and `org-conf-backend-get-children`.

- [ ] **Step 2: Run the new tests and verify they fail**

Run: `emacs -Q --batch -L test -l test/run-tests.el --eval '(ert-run-tests-batch-and-exit "active-backend\\|rest-get-page-info\\|rest-list-attachments\\|absolute-url")'`

Expected: FAIL — `org-conf--active-backend` and the new adapters are undefined.

- [ ] **Step 3: Add options and the registry**

```elisp
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
```

Add `org-conf--active-backend`, `org-conf--backend-fn`, `org-conf-backend-*` dispatchers, and `org-conf--backend-functions` with only the `rest` entry at this stage. Register the existing REST functions directly where the signature matches, and add:

```elisp
(defun org-conf-rest-get-page-info (page-id) …)   ; uses version/space/ancestors/_links
(defun org-conf-rest-list-attachments (page-id) …) ; maps :results to (:id :title)
```

Change the REST expand string to `"/rest/api/content/%s?expand=version.number,space,_links,ancestors"`. Change `org-conf--page-url` to return `webui` unchanged when it matches `\\`https?://`.

- [ ] **Step 4: Rewire the call sites**

Replace direct REST calls exactly as listed in **Files** above. Preserve behavior:
- `org-conf--read-parent-id` flat mode: `(plist-get (org-conf-backend-get-page-info id) :title)` replaces `org-conf-rest-get-page-node`'s `:title` check; descendants still come from `org-conf-backend-get-children`.
- `org-conf-export-to-conf` and `...-without-post`: version is `(1+ (or (plist-get (org-conf-backend-get-page-info conf-id) :version) 0))`; the PUT/POST call becomes `org-conf-backend-update-page`.
- `org-conf-create-page-and-export`: `org-conf-backend-create-page`.
- `org-conf-export-page-info`: print `:title` and `:version` from `org-conf-backend-get-page-info`.
- `org-conf-export-img-info` and `org-conf--upload-img`: use `org-conf-backend-list-attachments`.
- `org-conf--upload-img`: use `org-conf-backend-upload-attachment` for both replace and new uploads.

- [ ] **Step 5: Run the full suite**

Run: `emacs -Q --batch -L test -l test/run-tests.el`

Expected: all pass. Because the default is `auto` and this machine has the CLI installed, any test that does not stub the dispatchers would take the CLI path — that is why Step 1 updates every interactive test.

- [ ] **Step 6: Commit**

```bash
git add ox-conf.el test/ox-conf-test.el
git commit -m "refactor(ox-conf): add backend dispatch seam with REST implementation"
```

---

### Task 4: Add the CLI process layer

**Files:**
- Modify: `ox-conf.el` (new section after the backend registry)
- Test: `test/ox-conf-test.el`

**Interfaces:**
- Consumes: `org-conf-cli-program`.
- Produces:
  - `(org-conf--cli-program)` → absolute program path or `user-error`
  - `(org-conf--cli-json &rest ARGS)` → parsed alist; raises `user-error` on non-zero exit or invalid JSON
  - `(org-conf--write-utf8-file TEXT)` → temp file path
  - `(org-conf--with-temp-body TEXT FN)` → calls `(FN PATH)`, deletes the file in `unwind-protect`

- [ ] **Step 1: Write the failing tests**

```elisp
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
```

- [ ] **Step 2: Run the new tests and verify they fail**

Run: `emacs -Q --batch -L test -l test/run-tests.el --eval '(ert-run-tests-batch-and-exit "cli-json\\|temp-body")'`

Expected: FAIL — functions undefined.

- [ ] **Step 3: Implement the process layer**

- `org-conf--cli-program`: `(or (executable-find org-conf-cli-program) (user-error "…"))`.
- `org-conf--cli-json`: bind `coding-system-for-read` and `coding-system-for-write` to `utf-8-unix`; create a stderr temp file; call `(apply #'call-process program nil (list (current-buffer) stderr) nil "--json" args)`; on non-zero, read stderr, try `json-parse-string` to extract `error`, and `user-error`; on zero, `json-parse-buffer` with `:object-type 'alist :array-type 'list :null-object nil :false-object nil` (wrap parse errors into a `user-error` naming the command); delete the stderr file in `unwind-protect`.
- `org-conf--write-utf8-file` / `org-conf--with-temp-body`: `make-temp-file "ox-conf-body-"`, `with-temp-file` binding `coding-system-for-write` to `utf-8-unix`, then `unwind-protect` the thunk and delete the file.

- [ ] **Step 4: Run the full suite**

Run: `emacs -Q --batch -L test -l test/run-tests.el`

Expected: all pass.

- [ ] **Step 5: Commit**

```bash
git add ox-conf.el test/ox-conf-test.el
git commit -m "feat(ox-conf): add confluence-cli process layer"
```

---

### Task 5: Implement the CLI backend operations

**Files:**
- Modify: `ox-conf.el` (CLI operations + register them in `org-conf--backend-functions`)
- Test: `test/ox-conf-test.el`

**Interfaces:**
- Consumes: `org-conf--cli-json`, `org-conf--with-temp-body`, `org-conf--page-json-results`, `org-conf--page-json-next`, `org-conf--cli-api-path`.
- Produces: `org-conf-cli-get-page-info`, `org-conf-cli-get-children`, `org-conf-cli-get-space-roots`, `org-conf-cli-create-page`, `org-conf-cli-update-page`, `org-conf-cli-list-attachments`, `org-conf-cli-upload-attachment`, plus the pure helper `org-conf--cli-api-path`.

- [ ] **Step 1: Write the failing tests**

```elisp
(ert-deftest ox-conf-test-cli-api-path-removes-the-rest-prefix ()
  (should (equal "space/DOCS/content?depth=root&limit=200"
                 (org-conf--cli-api-path
                  "/rest/api/space/DOCS/content?depth=root&limit=200")))
  (should (equal "content/1/child/page?limit=200"
                 (org-conf--cli-api-path "/rest/api/content/1/child/page?limit=200"))))

(ert-deftest ox-conf-test-cli-get-page-info-maps-json ()
  (cl-letf (((symbol-function 'org-conf--cli-json)
             (lambda (&rest args)
               (should (equal '("info" "7") args))
               '((id . "7") (title . "Doc") (version . 3)
                 (spaceKey . "DOCS") (parentId . "5")
                 (url . "http://x/pages/7")))))
    (should (equal '(:id "7" :title "Doc" :version 3 :space "DOCS"
                     :parent-id "5" :url "http://x/pages/7")
                   (org-conf-cli-get-page-info "7")))))

(ert-deftest ox-conf-test-cli-get-children-maps-json ()
  (cl-letf (((symbol-function 'org-conf--cli-json)
             (lambda (&rest args)
               (should (equal '("children" "7") args))
               '((children . (((id . "8") (title . "Kid"))))))))
    (should (equal '((:id "8" :title "Kid"))
                   (org-conf-cli-get-children "7")))))

(ert-deftest ox-conf-test-cli-get-space-roots-reads-grouped-api-output ()
  (let ((calls nil)
        (org-conf-parent-child-page-limit 200))
    (cl-letf (((symbol-function 'org-conf--cli-json)
               (lambda (&rest args)
                 (push args calls)
                 '((page . ((results . [((id . "1") (title . "Root"))])))))))
      (should (equal '((:id "1" :title "Root"))
                     (org-conf-cli-get-space-roots "DOCS")))
      (should (equal '("api" "space/DOCS/content?depth=root&limit=200")
                     (nreverse (car calls)))))))

(ert-deftest ox-conf-test-cli-get-space-roots-follows-a-grouped-next-link ()
  (let ((calls nil)
        (org-conf-parent-child-page-limit 1))
    (cl-letf (((symbol-function 'org-conf--cli-json)
               (lambda (&rest args)
                 (push (cadr args) calls)
                 (if (equal (cadr args) "space/DOCS/content?depth=root&limit=1")
                     '((page . ((results . [((id . "1") (title . "A"))])
                                (_links . ((next . "https://x/next"))))))
                   '((page . ((results . [((id . "2") (title . "B"))])))))))
      (should (equal '((:id "1" :title "A") (:id "2" :title "B"))
                     (org-conf-cli-get-space-roots "DOCS")))
      (should (equal '("space/DOCS/content?depth=root&limit=1"
                       "https://x/next")
                     (nreverse calls))))))

(ert-deftest ox-conf-test-cli-create-page-uses-create-child-and-a-body-file ()
  (let (captured)
    (cl-letf (((symbol-function 'org-conf--cli-json)
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
    (cl-letf (((symbol-function 'org-conf--cli-json)
               (lambda (&rest args)
                 (setq captured args)
                 '((id . "9") (url . "http://x/pages/9")))))
      (org-conf-cli-create-page "Doc" "DOCS" nil)
      (should (equal '("create" "Doc" "DOCS") (seq-take captured 3))))))

(ert-deftest ox-conf-test-cli-update-page-writes-the-body-and-omits-a-blank-title ()
  (let (captured)
    (cl-letf (((symbol-function 'org-conf--cli-json)
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
  (cl-letf (((symbol-function 'org-conf--cli-json)
             (lambda (&rest args)
               (should (equal '("attachments" "9") args))
               '((attachments . (((id . "a1") (title . "a.png"))))))))
    (should (equal '((:id "a1" :title "a.png"))
                   (org-conf-cli-list-attachments "9")))))

(ert-deftest ox-conf-test-cli-upload-attachment-adds-replace ()
  (let (captured)
    (cl-letf (((symbol-function 'org-conf--cli-json)
               (lambda (&rest args) (setq captured args) '((uploaded . [])))))
      (org-conf-cli-upload-attachment "9" "/tmp/a.png" "a1")
      (should (equal '("attachment-upload" "9" "--file" "/tmp/a.png" "--replace")
                     captured)))))
```

- [ ] **Step 2: Run the new tests and verify they fail**

Run: `emacs -Q --batch -L test -l test/run-tests.el --eval '(ert-run-tests-batch-and-exit "cli-\\(api-path\\|get-\\|create-\\|update-\\|list-\\|upload-\\)")'`

Expected: FAIL — CLI operations undefined.

- [ ] **Step 3: Implement the operations**

Exact CLI shapes (verified against confluence-cli 2.29.0):

| Operation | Arguments |
|---|---|
| get-page-info | `("info" PAGE-ID)` → `id,title,version,spaceKey,parentId,url` |
| get-children | `("children" PAGE-ID)` → `children[].id/title` |
| get-space-roots | `("api" RELATIVE-SPACE-CONTENT-PATH)` → grouped `page.results`, next via `page._links.next` |
| create-page (parent) | `("create-child" TITLE PARENT-ID "--file" TEMP)` |
| create-page (root) | `("create" TITLE SPACE "--file" TEMP)` |
| update-page | `("update" PAGE-ID ["--title" TITLE] "--file" TEMP "--format" "storage")` |
| list-attachments | `("attachments" PAGE-ID)` → `attachments[].id/title` |
| upload-attachment | `("attachment-upload" PAGE-ID "--file" FILE ["--replace"])` |

- `create-page` writes an empty body to the temp file (the normal export PUT/POST fills it in immediately afterwards) and returns `(:id ID :webui URL)` with the CLI's absolute URL.
- `update-page` writes BODY to the temp file; `--title` is included only when TITLE is a non-empty string. Return the parsed JSON (non-nil on success).
- `get-space-roots` starts from `(org-conf--cli-api-path (org-conf--space-root-page-api space))`, then follows `org-conf--page-json-next` until nil or `org-conf--max-child-page-requests` calls.
- Register all seven in the `cli` entry of `org-conf--backend-functions`.

- [ ] **Step 4: Run the full suite**

Run: `emacs -Q --batch -L test -l test/run-tests.el`

Expected: all pass.

- [ ] **Step 5: Commit**

```bash
git add ox-conf.el test/ox-conf-test.el
git commit -m "feat(ox-conf): implement confluence-cli backend operations"
```

---

### Task 6: Document the backend and run read-only live verification

**Files:**
- Modify: `ox-conf.el` (commentary + `f r` message), `README.org`, `CLAUDE.md`
- Verification: temporary script outside the repo (do not commit)

**Interfaces:**
- Consumes: everything from Tasks 1-5.
- Produces: user-facing documentation and evidence that both backends read the same real pages.

- [ ] **Step 1: Document `org-conf-backend` and `org-conf-cli-program`**

In `ox-conf.el` commentary, `README.org`, and `CLAUDE.md`, add:
- `org-conf-backend` values `auto` (default) / `rest` / `cli`;
- `org-conf-cli-program` default `"confluence"`;
- that the CLI backend needs a configured `confluence` profile (`confluence profile list`);
- that `f r` only clears Emacs' REST auth cache; CLI credentials are managed by the CLI;
- that `CONF_*` is placed after the leading `#+` keyword block and never above a shebang/mode line.

Update `org-conf-forget-password` to also `message` that the CLI backend manages its own credentials when `(eq (org-conf--active-backend) 'cli)`.

- [ ] **Step 2: Run the full unit suite**

Run: `emacs -Q --batch -L test -l test/run-tests.el`

Expected: all pass. Record the test count.

- [ ] **Step 3: Run the read-only live check for both backends**

Create `/tmp/ox-conf-live-check.el` (outside the repo):

```elisp
(dolist (d (directory-files (expand-file-name "~/.emacs.d/elpa") t "\\`[^.]"))
  (add-to-list 'load-path d))
(add-to-list 'load-path (expand-file-name "."))
(require 'ox-conf)
(setq org-conf-url "https://conf.hygon.cn/")
(dolist (backend '(cli rest))
  (let ((org-conf-backend backend))
    (condition-case err
        (progn
          (message "%s info: %S" backend
                   (org-conf-backend-get-page-info "283548469"))
          (message "%s children: %S" backend
                   (org-conf-backend-get-children "283548469"))
          (message "%s roots: %S" backend
                   (org-conf-backend-get-space-roots "SSC"))
          (message "%s attachments: %S" backend
                   (org-conf-backend-list-attachments "283548469")))
      (error (message "%s FAILED: %S" backend err)))))
(message "auto resolves to: %S"
         (let ((org-conf-backend 'auto)) (org-conf--active-backend)))
```

Run from the worktree: `emacs -Q --batch -L . -l /tmp/ox-conf-live-check.el`

Expected:
- `cli` returns page title `Claude Code 备忘录`, version 10, space `SSC`, exactly 2 attachments, and at least one SSC root.
- `rest` returns the same page/title/space/attachments (credentials may come from `auth-source`; if REST auth fails, report it as an environment limitation, not a code failure — the CLI path is the mandatory evidence).
- `auto resolves to: cli`.
- No write operation is performed.

- [ ] **Step 4: Verify the picker on a real space through the CLI path**

Run a second read-only batch check that calls `org-conf-backend-get-space-roots "SSC"` and asserts the first entry is the real root page (`软件与安全中心`), proving the two-placeholder menu is gone:

```elisp
(let ((org-conf-backend 'cli))
  (let ((roots (org-conf-backend-get-space-roots "SSC")))
    (message "roots=%S" roots)
    (unless (and roots (stringp (plist-get (car roots) :title)))
      (error "space roots still empty"))))
```

Expected: at least one real page and no error.

- [ ] **Step 5: Commit the documentation**

```bash
git add ox-conf.el README.org CLAUDE.md
git commit -m "docs(ox-conf): document backend selection and CONF placement"
```

---

## Self-Review

- **Spec coverage:** Issue 1 → Task 1 (grouped JSON) and Task 6 Step 4 (real roots). Issue 2 → Task 2. Issue 3 → Tasks 3-6. Worktree/verification → Task 0 and Task 6.
- **Step scan:** every task starts with a failing test, a run command, an implementation, a full-suite run, and a commit. No TBD or "handle edge cases" lines.
- **Type consistency:** the seven dispatchers use the signatures in D3 everywhere; `:webui` remains the create-page return key; `:version` is an integer; attachments are `(:id :title)`.
- **Review Focus:** each of the five lines maps to a named test (Task 1 grouped fixtures; Task 2 mode-line/shebang fixtures; Task 4 UTF-8 temp-file test and Task 6 live find; Task 5 `--file` assertions; Task 3 auto-fallback and Task 4 non-zero/invalid-JSON tests).
- **Proportion:** the plan defines interfaces, exact CLI shapes, test names/assertions, and only the algorithm-bearing bodies (front-matter scan, grouped JSON helpers).
