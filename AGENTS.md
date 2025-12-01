# AGENTS.md - Instructions for AI Coding Agents

## Build Commands
- **Build site:** `./build.sh` (runs `emacs -Q --script build-site.el`)
- **Clean build:** Delete `public/` then run `./build.sh`
- **Dev server:** In Emacs: `M-x httpd-serve-directory` → `./public`

## Code Style & Conventions
- **Language:** Emacs Lisp for build system, Org-mode for content
- **Elisp style:** Use lexical-binding, package headers (Summary/Commentary/Code), `;;;` file headers
- **Naming:** `local/` prefix for custom functions (e.g., `local/org-html-add-tailwind-container`)
- **Variables:** Use `defvar` or `setq`, descriptive names with namespace prefixes
- **Indentation:** Standard Emacs Lisp (2 spaces), use `emacs-lisp-mode` auto-indent

## Architecture Notes
- Content lives in `content/` (Org files), builds to `public/` (gitignored)
- Assets in `content/assets/` are copied to `public/assets/`
- Dependencies installed to `./.packages` via MELPA/ELPA
- Tailwind CSS via CDN, dark mode follows system preference
- See CLAUDE.md for detailed architecture and build system info