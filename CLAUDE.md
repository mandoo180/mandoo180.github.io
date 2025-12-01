# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a static site generator that converts Org-mode files to HTML and publishes to GitHub Pages. The site uses Emacs Org-mode's publishing system with vanilla CSS for styling.

## Build System

The build process is orchestrated through Emacs Lisp:

**Build the site:**
```bash
./build.sh
```

This runs `emacs -Q --script build-site.el` which:
1. Installs dependencies (htmlize package) into `./.packages`
2. Converts all `.org` files in `content/` to HTML in `public/`
3. Copies `content/assets/` to `public/assets/` (includes CSS, JavaScript, fonts, images)
4. Applies GitHub Flavored Markdown styling with dark mode support

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
- **HTML head:** Links to vanilla CSS stylesheets, responsive viewport meta
- **Stylesheets:**
  - `assets/styles/style.css` - Main GitHub Flavored Markdown styling
  - `assets/styles/toc-sidebar.css` - Sticky floating table of contents
- **Dark mode:** Uses `prefers-color-scheme` media query following browser/system preference
- **Org settings:**
  - Babel enabled for shell and emacs-lisp
  - `org-confirm-babel-evaluate` set to nil (code blocks execute without confirmation)
  - TOC sidebar integration via JavaScript at `assets/scripts/toc-sidebar.js`
  - Htmlize uses CSS output type

### Styling
- **Vanilla CSS** - No framework dependencies, all styling self-hosted
- **GitHub-inspired design** - Clean, readable typography and spacing
- **Dark mode** - Automatic switching based on system preference
- **Responsive layout** - Mobile-first design with breakpoints at 768px, 1024px, 1280px, 1536px
- **Syntax highlighting** - GitHub-style colors for code blocks (light/dark)
- **Semantic HTML5** - header/main/footer elements for preamble/content/postamble

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