# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a static site generator that converts Org-mode files to HTML and publishes to GitHub Pages. The site uses Emacs Org-mode's publishing system with Tailwind CSS for styling.

## Build System

The build process is orchestrated through Emacs Lisp:

**Build the site:**
```bash
./build.sh
```

This runs `emacs -Q --script build-site.el` which:
1. Installs dependencies (htmlize package) into `./.packages`
2. Converts all `.org` files in `content/` to HTML in `public/`
3. Copies `content/assets/` to `public/assets/`
4. Applies Tailwind CSS styling with dark mode support

**Development server (in Emacs):**
```emacs-lisp
(package-install 'simple-httpd)
M-x httpd-serve-directory RET ./public RET
```

## Architecture

### Content Pipeline
- **Source:** Org-mode files in `content/` (supports nested directories like `content/sicp/`)
- **Build script:** `build-site.el` - Emacs Lisp script using `ox-publish`
- **Output:** HTML files in `public/` (gitignored, regenerated on each build)

### Key Build Configuration (build-site.el)

The build script configures:
- **HTML head:** Tailwind CDN with typography plugin, responsive viewport meta, dark mode config
- **Dark mode:** Uses `prefers-color-scheme: media` following browser/system preference
- **Content wrapper:** Post-export filter (`local/org-html-add-tailwind-container`) injects Tailwind prose classes for responsive typography
- **Org settings:**
  - Babel enabled for shell and emacs-lisp
  - `org-confirm-babel-evaluate` set to nil (code blocks execute without confirmation)
  - InfoJS integration for navigation at `assets/scripts/org-info.js`
  - Htmlize uses CSS output type

### Styling
- Tailwind CSS loaded via CDN with typography plugin
- Dark mode follows system preference (automatic)
- Responsive max-width containers (3xl to 6xl breakpoints)
- Semantic HTML5 elements (header/main/footer for preamble/content/postamble)

## Deployment

GitHub Actions workflow (`.github/workflows/publish.yml`):
- Triggers on push to `main` branch
- Installs `emacs-nox` on Ubuntu
- Runs `./build.sh`
- Deploys `public/` folder to `gh-pages` branch via `JamesIves/github-pages-deploy-action`

## Important Notes

- The `public/` directory is completely regenerated on each build (deleted then recreated)
- Dependencies are installed to `./.packages` (gitignored)
- All org files support Babel code execution with shell and emacs-lisp languages
- The site structure mirrors the `content/` directory structure