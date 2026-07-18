# FlashShell integration — reposition Pengling as "daily learning games"

Date: 2026-07-18 · Status: approved

## Problem

FlashShell (flashshell.anonpengling.org, a shell-learning game) lives on a
Pengling subdomain but is absent from the hub. The hub currently positions
the studio as a *language*-games studio (hero copy, meta, and a required
`learn: { language, forSpeakers }` schema field), which excludes it. More
web apps of this kind are planned, so the fix must scale.

## Decision

Broaden the brand one notch: **"daily language games" → "daily learning
games"**. FlashShell shares the studio DNA (daily rounds, game mechanics,
EN/KO bilingual); the only mismatch was subject matter. Future learning-game
web apps join the same single list — no separate "Lab" section.

## Changes

1. **Copy (`src/i18n/ui.ts`)** — hero, meta title/description broadened in
   EN and KO. Bridge line: "From Mandarin to the command line" /
   "외국어부터 커맨드라인까지". FlashShell added to meta description.
2. **Schema (`src/content.config.ts`, `src/lib/types.ts`)** — rename
   `learn.language` → `learn.subject` (shape unchanged; `forSpeakers`
   keeps its meaning). Update `AppCard.astro` meta line, both existing
   app md files, and unit tests.
3. **New entry (`src/content/apps/flashshell.md`)** — `status: live`,
   `platforms: [web]`, terminal-green accent (~#2FBF71),
   `learn: { subject: "Linux shell", forSpeakers: "EN · KO" }`.
   New icon `public/apps/flashshell/icon.svg` (dark rounded square,
   green `>_` prompt). Screenshot captured from the live site at a mobile
   viewport; if the portrait crop doesn't hold up, fall back to
   `featured: false` (compact row card, media hidden).

## Verification

`npm test`, `npm run check`, `npm run build`, visual check of `/` and
`/ko` via preview.

## Out of scope

FlashShell repo improvements (favicon/OG tags, README backlink to the hub).
