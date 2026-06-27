# Notes Migration & Repo Cleanup Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Move the existing Org-mode dev-notes site to a new repo `mandoo180/notes` served at `notes.anonpengling.org` (history preserved), then strip `mandoo180.github.io` down to a clean slate ready for the Astro studio hub.

**Architecture:** The current `mandoo180.github.io` repo *is* the notes site today, so migration is a history-preserving push of `main` into a fresh empty repo (both repos then share history up to the split point — no filter-repo needed). The notes repo keeps its existing Emacs `publish.yml` and builds itself. A custom subdomain (`notes.anonpengling.org`) is attached via GitHub Pages settings + a Route53 CNAME. Finally, the org pipeline and content are deleted from this repo in a normal commit (history retained).

**Tech Stack:** `gh` CLI (GitHub repo + Pages API), `git`, `aws` CLI (Route53), existing Emacs build (`build-site.el` / `publish.yml`) runs unchanged in the notes repo.

## Global Constraints

- Notes repo: `mandoo180/notes`, public, homepage `https://notes.anonpengling.org`.
- Notes URL: `notes.anonpengling.org` (subdomain custom domain on a **project** Pages site).
- History: **preserve** (push `main` history into the new repo; do not squash).
- This repo (`mandoo180.github.io`) is the **user/apex** Pages site → its custom domain (set later in the hub plan) is `anonpengling.org`.
- DNS lives in AWS Route53 hosted zone `anonpengling.org.` — requires the user's AWS credentials available in the shell.
- GitHub Pages IPv4 for apex (reference, used in hub plan): `185.199.108.153`, `185.199.109.153`, `185.199.110.153`, `185.199.111.153`.
- Do **not** touch existing `fulang.*` / `storyfluent.*` DNS records.
- Work happens on branch `feat/anonpengling-studio-hub` (already created).

---

### Task 1: Create the notes repo and push history

**Files:**
- None in this repo (creates external repo `mandoo180/notes`).

**Interfaces:**
- Produces: a GitHub repo `mandoo180/notes` whose `main` branch == this repo's `main` history (the Org notes site).

- [ ] **Step 1: Confirm `gh` is authenticated and `main` is the notes site**

Run: `gh auth status && git log --oneline -1 main`
Expected: `Logged in to github.com`; the `main` tip is the notes commit (e.g. `14e2b9c TODO: renew all docs`) — NOT a hub/spec commit.

- [ ] **Step 2: Create the empty remote repo**

Run:
```bash
gh repo create mandoo180/notes --public \
  --description "Personal dev notes, dotfiles, and literate Emacs/tooling configs" \
  --homepage "https://notes.anonpengling.org"
```
Expected: `✓ Created repository mandoo180/notes on GitHub` (no local clone, no push yet).

- [ ] **Step 3: Push the full `main` history into the notes repo**

Run:
```bash
git push https://github.com/mandoo180/notes.git main:main
```
Expected: push succeeds; output ends with `* [new branch] main -> main`.

- [ ] **Step 4: Verify history landed**

Run: `gh api repos/mandoo180/notes/commits --jq 'length'`
Expected: a number > 1 (full history present, not a single squashed commit).

---

### Task 2: Build & publish the notes site at notes.anonpengling.org

**Files:**
- None in this repo (configures `mandoo180/notes` Pages + Route53).

**Interfaces:**
- Consumes: `mandoo180/notes` from Task 1 (its `publish.yml` triggers an Emacs build → `gh-pages` branch on push to `main`).
- Produces: live site at `https://notes.anonpengling.org`.

- [ ] **Step 1: Trigger / confirm the notes build ran**

The push in Task 1 triggers `.github/workflows/publish.yml` (build → deploy to `gh-pages`).
Run: `gh run list --repo mandoo180/notes --limit 1`
Expected: one workflow run; wait until `completed / success`. If it has not started, re-run: `gh workflow run publish.yml --repo mandoo180/notes`.

- [ ] **Step 2: Verify the `gh-pages` branch exists**

Run: `gh api repos/mandoo180/notes/branches/gh-pages --jq '.name'`
Expected: `gh-pages`.

- [ ] **Step 3: Enable Pages from `gh-pages` and set the custom domain**

Run:
```bash
gh api -X POST repos/mandoo180/notes/pages \
  -f 'source[branch]=gh-pages' -f 'source[path]=/' 2>/dev/null || true
gh api -X PUT repos/mandoo180/notes/pages -f cname=notes.anonpengling.org -F https_enforced=true
```
Expected: final call returns JSON containing `"cname": "notes.anonpengling.org"`. (The first call is idempotent — ignore "already enabled".)

- [ ] **Step 4: Add the Route53 CNAME for the subdomain**

Run:
```bash
ZONE_ID=$(aws route53 list-hosted-zones-by-name --dns-name anonpengling.org. \
  --query "HostedZones[?Name=='anonpengling.org.'].Id" --output text | sed 's#/hostedzone/##')
aws route53 change-resource-record-sets --hosted-zone-id "$ZONE_ID" --change-batch '{
  "Comment":"notes subdomain -> GitHub Pages project site",
  "Changes":[{"Action":"UPSERT","ResourceRecordSet":{
    "Name":"notes.anonpengling.org","Type":"CNAME","TTL":300,
    "ResourceRecords":[{"Value":"mandoo180.github.io"}]}}]}'
```
Expected: returns a `ChangeInfo` with `"Status": "PENDING"`.

- [ ] **Step 5: Verify DNS resolves and the site serves**

Run: `dig +short notes.anonpengling.org` then `curl -sI https://notes.anonpengling.org | head -1`
Expected: `dig` shows `mandoo180.github.io.` then the Pages IPs; `curl` shows `HTTP/2 200` (allow a few minutes for DNS + cert issuance; GitHub shows the cert provisioning state under repo Settings → Pages).

---

### Task 3: Strip the Org pipeline & content from this repo

**Files:**
- Delete: `content/`, `build-site.el`, `build.sh`, `build.ps1`, `tangle.sh`, `tangle-claude.sh`, `tangle-configs.el`, `split-config.py`, `retarget-skills.sh`, `README.org`, `.github/workflows/publish.yml`, `public/`, `.packages/`, `tmp/`, `.agent-shell/`, `AGENTS.md` (already staged-deleted)
- Modify: `.gitignore`

**Interfaces:**
- Consumes: a verified-live `notes.anonpengling.org` from Task 2 (notes are safe elsewhere before deletion here).
- Produces: a clean repo containing only `docs/`, `CLAUDE.md`, `.gitignore`, `.mcp.json`, `.claude/`, `.git/`, `.github/` (empty of workflows) — ready for the hub plan.

- [ ] **Step 1: Gate — confirm notes are safely live before deleting**

Run: `curl -s https://notes.anonpengling.org | grep -qi "<html" && echo SAFE_TO_DELETE`
Expected: `SAFE_TO_DELETE`. Do not proceed otherwise.

- [ ] **Step 2: Remove the Org site, build pipeline, and build artifacts**

Run:
```bash
git rm -r --quiet content build-site.el build.sh build.ps1 \
  tangle.sh tangle-claude.sh tangle-configs.el split-config.py \
  retarget-skills.sh README.org .github/workflows/publish.yml 2>/dev/null
git rm -r --cached --quiet public .packages tmp .agent-shell 2>/dev/null || true
rm -rf public .packages tmp .agent-shell
git rm --quiet --ignore-unmatch AGENTS.md
```
Expected: files staged for deletion; no error (the `--ignore-unmatch`/`|| true` guards already-absent paths).

- [ ] **Step 3: Update `.gitignore` for the Astro toolchain**

Replace the file contents with:
```gitignore
# OS
.DS_Store

# Node / Astro
node_modules/
dist/
.astro/

# Claude Code - ignore personal settings, keep project configs
.claude/settings.local.json
.mcp.json

# Brainstorm scratch
.superpowers/

# Agent + cache leftovers
.agent-shell/
.packages/
tmp/
```
Note: `public/` is intentionally **no longer ignored** — Astro uses `public/` for static assets (CNAME, favicon, app images).

- [ ] **Step 4: Verify the working tree is clean of Org content**

Run: `ls && git status --short`
Expected: no `content/`, `build-site.el`, etc. remain; `git status` shows the deletions + the `.gitignore` change staged/unstaged.

- [ ] **Step 5: Commit the cleanup**

Run:
```bash
git add -A
git commit -m "$(cat <<'EOF'
chore: strip Org-mode notes pipeline; notes moved to mandoo180/notes

Dev notes now live at https://notes.anonpengling.org (mandoo180/notes,
history preserved). This repo is now a clean slate for the anonpengling
studio hub. Astro toolchain ignores added; public/ un-ignored for Astro.

Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>
Claude-Session: https://claude.ai/code/session_01D7j2UX9FNR5gmuo2EZo9rz
EOF
)"
```
Expected: commit succeeds on `feat/anonpengling-studio-hub`.

---

## Self-Review

- **Spec coverage (§3.2 Phase 0):** notes → separate repo (Task 1), history preserved (Task 1 Step 3–4), notes URL `notes.anonpengling.org` (Task 2), remove org pipeline + artifacts + `.DS_Store` ignore + `AGENTS.md` (Task 3), `public/` un-ignored for Astro (Task 3 Step 3). Covered.
- **Placeholder scan:** all commands concrete; no TBD. The only timing dependency (DNS/cert propagation) is called out with how to observe it.
- **Type consistency:** N/A (ops plan, no shared code types). Repo names/domains used identically across tasks.
- **Safety:** deletion (Task 3) is gated on a live-site check (Step 1) so notes are never removed before they exist elsewhere.
