# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

`ox-conf.el` is an Org Mode export backend that translates Org documents to Confluence Wiki Storage Format (XHTML with `ac:` macros). It is derived from the built-in `ox-html` backend and adds Confluence integration (REST API or the `confluence` CLI) to update existing Confluence pages on export.

## How to use

Load `ox-conf.el` in Emacs (e.g., via `load-file` or `require`). Then in any `.org` file with `#+CONF_ID`, `#+CONF_SPACE`, and `#+CONF_PAGE` set at the top:

- `M-x org-export-dispatch` → `f` opens the Confluence export menu:
  - `f b` — export to a preview buffer
  - `f p` — fetch page info (title, version) from Confluence and print it
  - `f i` — list image attachments on the target Confluence page
  - `f f` — full export: transcode the Org buffer and PUT it to Confluence (uploads images via POST). **If the buffer has no `#+CONF_ID`, it creates the page first** — see below.
  - `f w` — export and PUT without uploading images
  - `f r` — clear cached Confluence REST credentials (calls `auth-source-forget-all-cached`); the CLI backend manages its own credentials
  - `f o` — open the Confluence page in the browser (new in 2026-05)

### Backends (2026-10-24)

`org-conf-backend` selects the transport:

- `auto` (default) — use the `confluence` CLI when `org-conf-cli-program`
  (`"confluence"` by default) is found on `exec-path`, otherwise REST.
- `rest` — always use the REST API (credentials via `auth-source`).
- `cli` — always use the `confluence` CLI; it needs a configured profile
  (`confluence profile list`).

The export flow calls seven dispatchers (`org-conf-backend-get-page-info`,
`-get-children`, `-get-space-roots`, `-create-page`, `-update-page`,
`-list-attachments`, `-upload-attachment`), resolved through
`org-conf--backend-functions`. The CLI path runs `confluence --json …` via
`call-process`, sends page bodies through a temporary UTF-8 `--file`, and never
uses Emacs credentials.

### Creating a page from scratch (2026-10-10)

`f f` branches on whether the buffer has a `#+CONF_ID` (`org-conf--export-action`).
With no id it runs `org-conf-create-page-and-export`, which asks for the target
space, the page title (defaulting to `#+TITLE:`, then the file name) and a parent
page, POSTs an empty page, writes `#+CONF_SPACE:`/`#+CONF_PAGE:`/`#+CONF_ID:` into
the buffer (unsaved — you keep control), and then calls the normal
`org-conf-export-to-conf` flow, so Confluence ends up with version 2 holding the
real content.

The `#+CONF_*` keywords are placed by `org-conf--insert-conf-keywords` at the end of
the file's leading keyword block, found with `org-conf--keyword-block-end` (org's
`org-at-keyword-p`). They therefore land *after* `#+TITLE:`/`#+OPTIONS:` rather than
above them; existing CONF lines are removed first, so a file written by the older
code gets normalised too. Leading blank lines, a shebang, an Emacs mode line
(`# -*- … -*-`) and org comments are skipped first by
`org-conf--front-matter-start`, so the block is never pushed above them. Note that
ox reads these only as `#+` keywords — `org-export--get-inbuffer-options` ignores
property drawers entirely, so `org-set-property` is not an option here.

Parent pages are chosen by *title*, never by URL, and there are two modes:

- `org-conf-root-parent-page` set: a flat, most-used-first list of that page's
  descendants, built by BFS over `/rest/api/content/{id}/child/page` and labelled
  `Root / Mid / Leaf` (`org-conf--collect-descendants`, `org-conf--parent-choices`).
- unset: infer a scope with `org-conf--infer-parent-scope` — the common ancestor
  of exported `.org` files in the current directory (or one/two levels up), then
  the parent of the most-used page in `org-conf-parent-freq-file`, then a title
  match on the directory name — and start `org-conf--read-parent-by-drill-down`
  there. Menus are file-like: `..` goes to the parent page, direct children are
  entered on selection, `✓ 就用 …` confirms the current page; frequently used
  parents in the same space come first. `org-conf--drill-choices` builds it.
  The space-top fallback lists the space roots from
  `/rest/api/space/{key}/content?depth=root` via `org-conf-backend-get-space-roots`.

The space-content endpoint groups its results by content type
(`{"page": {"results": …}}`); `org-conf--page-json-results` and
`org-conf--page-json-next` unwrap both that shape and the flat `/child/page`
shape, so the top-level menu lists real pages instead of only the synthetic
root/manual entries.

Both modes append a "type a URL/ID" escape hatch. Usage counts live in
`org-conf-parent-freq-file`. Choices whose labels collide (same-titled siblings, or
same-path pages) get an `[id]` suffix from `org-conf--unique-labels`, otherwise
`completing-read` would return one string for two entries. Relevant options:
`org-conf-root-parent-page`, `org-conf-parent-freq-file`, `org-conf-parent-max-pages`,
`org-conf-parent-child-page-limit`, `org-conf-parent-root-label`,
`org-conf-manual-parent-label`.

## Tests

```sh
emacs -Q --batch -L test -l test/run-tests.el
```

`test/run-tests.el` bootstraps `load-path` (adding `~/elpa` for `request`) and runs
the ERT suite (117 tests) in `test/ox-conf-test.el`. The suite covers the pure
helpers — path building, frequency sorting/persistence, grouped and flat JSON
parsing, the POST payload, BFS collection, shebang-aware keyword placement, the
drill-down decision table, backend selection and the REST adapters, the CLI
command lines and error handling, parent-scope inference (common ancestor,
sibling/recent/title-search fallbacks), recent-parent filtering, and the file-like
menu (`..`, confirm, recent, children, manual).
`org-conf-create-page-and-export`, `org-conf--read-parent-by-drill-down` and
`org-conf--read-parent-id` are covered with `cl-letf` stubs at the prompt and
backend boundaries; the drill-down stubs also assert which menus were shown.
CLI-operation tests rebind `org-conf--cli-json-function`, the transport seam.

The REST wrappers themselves (`org-conf-rest-*`, `org-conf--rest-get-json`) are not
unit-tested — they are real HTTP. `org-conf--fetch-all-pages` is tested separately
with an injected `get-json`, so the pagination logic is covered even though the
wrapper is not. The CLI process layer is tested by stubbing `call-process`; real
CLI/REST reads are exercised only by the read-only live check.

## Architecture

The entire backend lives in a single file. Its structure:

### 1. User-visible options (`defcustom`)
- `org-conf-url` — Confluence host base URL
- `org-conf-jira-chart-server` / `org-conf-jira-chart-server-id` — Jira integration for chart macros
- `org-conf-default-space` — fallback Confluence space key
- `org-conf-toc-headline` — heading text for the generated TOC (defaults to Chinese "目录")
- `org-conf-log-level` — controls request logging verbosity (`-1` = silent)
- `org-conf-backend` — transport: `auto` (default), `rest`, or `cli`
- `org-conf-cli-program` — executable used by the `cli` backend (default `"confluence"`)
- `org-conf-root-parent-page` — page (URL or id) whose descendants are offered as parent-page choices when creating
- `org-conf-parent-freq-file` — where parent-page usage counts are persisted
- `org-conf-parent-max-pages` — cap on pages fetched while listing parent-page choices
- `org-conf-parent-child-page-limit` — page size when walking `/child/page`
- `org-conf-scope-search-depth` — how many directory levels to climb when inferring the parent scope
- `org-conf-recent-parent-limit` — how many frequently used parents to offer first
- `org-conf-parent-root-label` — the display string for the "no parent page" choice
- `org-conf-manual-parent-label` — the display string for the "type a URL/ID" escape hatch

### 2. Template layer
`org-conf-template` (outer wrapper), `org-conf-inner-template` (prepends TOC, appends footnotes), `org-conf-headline` (wraps in `numberedheadings` macro when `#+OPTIONS: num:t`), `org-conf-toc` (generates `ac:toc` macro), and `org-conf--tags` (maps Chinese tags to Confluence status macros colored Green/Yellow/Red/Blue).

### 3. Element transcoders
Override specific elements from `ox-html`. The most complex is `org-conf-special-block`, which dispatches on block type:
- `warning`, `note`, `tip`, `expand`, `info` → Confluence panel macros
- `history` → `change-history` macro
- `section` → `section` macro with `:border` parameter (yes/no)
- `column` → `column` macro with `:width` parameter
- Any unknown type falls through, returning contents unchanged

`org-conf-src-block` has three paths: Jira issue queries (`jira` language), Jira charts (`jirachart-pie` / `jirachart-2d`), and standard code blocks (maps Org src languages to Confluence code macro language IDs; special-cases `asm` → `c`, `rs` → `bash` with a host/path prompt comment).

### 4. Filters (`:filters-alist`)
- `org-conf-latex-fragment-filter` — wraps inline LaTeX in `mathinline` macro
- `org-conf-latex-environment-filter` — wraps display LaTeX in `mathblock` macro
- `org-conf-export-block-filter` — wraps raw HTML export blocks in an `html` macro with CDATA

### 5. Backend layer: REST and confluence-cli

The stable interface is eight dispatchers — `org-conf-backend-get-page-info`,
`-get-children`, `-get-space-roots`, `-create-page`, `-update-page`,
`-list-attachments`, `-upload-attachment`, `-search-pages` — resolved by
`org-conf--active-backend` and the `org-conf--backend-functions` alist.

REST implementation (synchronous, uses the `request` library):
- `org-conf-rest-get-page-json` — GET page metadata (version, space, ancestors, links)
- `org-conf-rest-put` — PUT updated page content as JSON (body in storage format)
- `org-conf-rest-get-attach-json` — GET attachment list (parsed as plist)
- `org-conf-rest-post-file` — POST new attachments or update existing ones (`X-Atlassian-Token: no-check` header)
- `org-conf--rest-get-json` — generic authenticated GET returning parsed JSON (or nil on failure)
- `org-conf-rest-get-page-node` — GET a page's `(:id :title)`
- `org-conf-rest-get-children` — GET a page's direct children, paginating via `_links.next`
- `org-conf-rest-get-space-roots` — GET a space's top-level pages (`depth=root`), same pagination
- `org-conf-rest-post-page` — POST a new (empty) page under an optional parent, returning `(:id :webui)`
- `org-conf-rest-get-page-info` / `org-conf-rest-list-attachments` — adapters that
  normalize REST JSON into the dispatcher plists
- `org-conf-rest-search-pages` — CQL title search (`title ~ … and space = …`)

CLI implementation (`confluence` 2.29.0, `--json` output):
- `org-conf--cli-json` — runs `confluence --json …` via `call-process`, captures
  stderr in a temp file, and raises `user-error` on non-zero exit or invalid JSON
- `org-conf--cli-invoke` — applies `org-conf--cli-json-function`, the rebindable
  transport seam
- `org-conf--with-temp-body` — writes UTF-8 page bodies to a temp file for `--file`
- `org-conf-cli-get-page-info`, `-get-children`, `-get-space-roots`,
  `-create-page`, `-update-page`, `-list-attachments`, `-upload-attachment`,
  `-search-pages`

`org-conf--fetch-all-pages` is the shared paginator behind both REST listings: it
walks `_links.next` up to `org-conf--max-child-page-requests` requests and flattens
`results` into `(:id :title)` plists. `org-conf--page-json-results` and
`org-conf--page-json-next` handle both the flat `/child/page` shape and the grouped
space-content shape. The API paths themselves are built by the pure
`org-conf--child-page-api` and `org-conf--space-root-page-api`;
`org-conf--cli-api-path` strips the REST prefix for the CLI's `api` command.

Parent discovery is built on top of these: `org-conf--infer-parent-scope`
infers the starting page from the local directory, `org-conf--common-ancestor`
computes the deepest shared ancestor of exported siblings,
`org-conf--recent-parent-choices` turns the frequency file into `★ 就用 …`
shortcuts, and `org-conf--drill-choices` builds the file-like menu.

REST authentication uses HTTP Basic Auth through Emacs' `auth-source`, which caches credentials. `org-conf-forget-password` clears that cache only; the CLI manages its own profile credentials.

### 6. Export backend registration
`org-export-define-derived-backend` at the end of the file defines `org-conf-html` as derived from `html`, with the `:translate-alist`, `:filters-alist`, `:options-alist`, and `:menu-entry` that tie everything together.

## Org file requirements

Every `.org` file to be exported must include:
```
#+CONF_SPACE: <space-key>
#+CONF_PAGE: <full-confluence-page-url>
#+CONF_ID: <page-id>
```
Use `M-x ske-conf` for a quick interactive template that prompts for space and page URL and auto-extracts the ID.

## Edge cases and gotchas

- **Example blocks are intentionally deprecated** (2025-11-17) — they emit a warning and produce empty content when exported. Use `#+begin_src` instead.
- **`org-conf--embed-svg`** references an undefined `fun` variable (line 673 in the `.bak` file) — this appears to be a latent bug where the non-SVG fallback would break.
- **Image uploads**: When doing `f f` (full export), images are uploaded via POST before the page content is PUT. If an image already exists on the server, the user is prompted interactively (`y-or-n-p`) to confirm replacement.
- **Version management**: The export reads the current version number from Confluence, increments it by 1, and sends it with the PUT request. This implements optimistic concurrency — two simultaneous exports may conflict.
- **Babel code template**: `org-babel-exp-code-template` is modified at the top level to pass Jira-specific parameters (`host`, `path`, `limit`, `columns`, `chart-type`, etc.) through to the src block transcoder.

## CI/CD

- `.github/workflows/opencode.yml` — triggers on issue/PR comments containing `/oc` or `/opencode`; runs an AI agent for code review
- `.github/workflows/summary.yml` — uses `actions/ai-inference@v1` to auto-summarize new issues and post a comment
