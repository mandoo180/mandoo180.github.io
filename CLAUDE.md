# CLAUDE.md

This file guides Claude Code when working in this repository.

## Overview

`anonpengling.org` — the studio hub for the anonpengling indie app studio.
An Astro static site that showcases the studio's apps (Fulang, Storyfluent,
and future apps) from a typed content collection. Dark-gallery visual,
bilingual EN/KO, deployed to GitHub Pages.

## Build System

- `npm run dev` — local dev server
- `npm run build` — static build to `dist/`
- `npm run preview` — preview the built site
- `npm run check` — `astro check` (types + content schema)
- `npm test` — Vitest unit tests for `src/lib` + `src/i18n` helpers

## Architecture

- **App data:** `src/content/apps/<slug>.md` (content collection, schema in
  `src/content/config.ts`). Add an app = add one file.
- **Logic:** pure helpers in `src/lib/app-display.ts` and `src/i18n/ui.ts`
  (unit-tested). `.astro` components stay presentational.
- **Pages:** `src/pages/index.astro` (EN, `/`), `src/pages/ko/index.astro`
  (KO, `/ko`).
- **Tokens:** `src/styles/tokens.css` (colors, spacing, type).

## Deployment

GitHub Actions (`.github/workflows/deploy.yml`) builds on push to `main`
and deploys `dist/` to GitHub Pages. Custom domain `anonpengling.org` via
`public/CNAME` + Route53. Dev notes live separately at
https://notes.anonpengling.org (repo `mandoo180/notes`).
