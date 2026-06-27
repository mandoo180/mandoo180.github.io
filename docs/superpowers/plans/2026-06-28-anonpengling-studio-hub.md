# anonpengling Studio Hub Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the anonpengling indie app-studio hub as an Astro static site — a dark-gallery landing page that showcases apps from a content collection, bilingual EN/KO, deployed to `anonpengling.org`.

**Architecture:** Astro static site. App data lives in a typed content collection (`src/content/apps/*.md`); a homepage per locale (`/` = EN, `/ko` = KO) reads, sorts, and renders the collection through presentational `.astro` components. All decision logic (sorting, which links to show, status/label localization) lives in pure TS helpers that are unit-tested with Vitest; `.astro` components are verified by `astro build` + preview. Design tokens are centralized CSS variables.

**Tech Stack:** Astro (latest, Node 20+), `@astrojs/sitemap`, Vitest for unit tests, vanilla CSS (no UI framework), GitHub Actions → GitHub Pages.

## Global Constraints

- Node 20+; Astro latest stable; package manager `npm`.
- `site: 'https://anonpengling.org'`; i18n `defaultLocale: 'en'`, `locales: ['en','ko']`, `routing.prefixDefaultLocale: false` → EN at `/`, KO at `/ko`.
- Visual tokens (dark gallery): page bg `#0b0e13`, card bg `#12171f`, border `#232a34`; text `#e8eaed` / `#9aa3af` / `#5f6875`; studio accent gradient `#67e8f9 → #818cf8` (solid fallback `#67e8f9`).
- App brand names render verbatim: **Fulang**, **Storyfluent** — never translated or simplified.
- No studio mascot; the aurora gradient is the studio identity.
- Respect `prefers-reduced-motion` for any gradient/glow animation.
- Mobile-first; feature tiles stack vertically under ~640px.
- This plan assumes the repo was cleaned by `2026-06-28-notes-migration-and-cleanup.md` (no Org files remain; `public/` is un-ignored). Work continues on branch `feat/anonpengling-studio-hub`.

---

### Task 1: Scaffold the Astro project + test harness

**Files:**
- Create: `package.json`, `astro.config.mjs`, `tsconfig.json`, `vitest.config.ts`, `src/env.d.ts`
- Create: `.github/workflows/deploy.yml`
- Modify: `CLAUDE.md`

**Interfaces:**
- Produces: a buildable empty Astro site; `npm run build`, `npm run preview`, `npm test`, `npm run check` scripts.

- [ ] **Step 1: Create `package.json`**

```json
{
  "name": "anonpengling-hub",
  "type": "module",
  "version": "0.1.0",
  "private": true,
  "scripts": {
    "dev": "astro dev",
    "build": "astro build",
    "preview": "astro preview",
    "check": "astro check",
    "test": "vitest run"
  },
  "dependencies": {
    "astro": "^5.0.0",
    "@astrojs/sitemap": "^3.2.0"
  },
  "devDependencies": {
    "@astrojs/check": "^0.9.0",
    "typescript": "^5.6.0",
    "vitest": "^2.1.0"
  }
}
```

- [ ] **Step 2: Create `astro.config.mjs`**

```js
import { defineConfig } from 'astro/config';
import sitemap from '@astrojs/sitemap';

export default defineConfig({
  site: 'https://anonpengling.org',
  i18n: {
    defaultLocale: 'en',
    locales: ['en', 'ko'],
    routing: { prefixDefaultLocale: false },
  },
  integrations: [sitemap()],
});
```

- [ ] **Step 3: Create `tsconfig.json`**

```json
{
  "extends": "astro/tsconfigs/strict",
  "include": [".astro/types.d.ts", "**/*"],
  "exclude": ["dist"]
}
```

- [ ] **Step 4: Create `src/env.d.ts`**

```ts
/// <reference path="../.astro/types.d.ts" />
```

- [ ] **Step 5: Create `vitest.config.ts` (pure TS units, no Astro runtime)**

```ts
import { defineConfig } from 'vitest/config';

export default defineConfig({
  test: {
    include: ['tests/**/*.test.ts'],
    environment: 'node',
  },
});
```

- [ ] **Step 6: Create the deploy workflow `.github/workflows/deploy.yml`**

```yaml
name: Deploy to GitHub Pages
on:
  push:
    branches: [main]
  workflow_dispatch:
permissions:
  contents: read
  pages: write
  id-token: write
concurrency:
  group: pages
  cancel-in-progress: false
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: 20
          cache: npm
      - run: npm ci
      - run: npm run build
      - uses: actions/upload-pages-artifact@v3
        with:
          path: dist
  deploy:
    needs: build
    runs-on: ubuntu-latest
    environment:
      name: github-pages
      url: ${{ steps.deployment.outputs.page_url }}
    steps:
      - id: deployment
        uses: actions/deploy-pages@v4
```

- [ ] **Step 7: Replace `CLAUDE.md` build/architecture sections with the Astro reality**

Rewrite `CLAUDE.md` so the "Overview", "Build System", "Architecture", and "Deployment" sections describe the Astro hub. Use this content:

```markdown
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
```

- [ ] **Step 8: Install and verify the empty project builds + tests run**

Run: `npm install && npm run build && npm test`
Expected: `npm run build` completes (an empty site builds with no pages yet is fine — if Astro errors on zero pages, proceed; Task 9 adds pages and this becomes green). `npm test` reports "no test files found" (acceptable until Task 2). Treat a clean `npm install` + successful dependency resolution as the gate here.

- [ ] **Step 9: Commit**

```bash
git add -A
git commit -m "chore: scaffold Astro project, vitest, deploy workflow"
```

---

### Task 2: Shared types + i18n strings (`useTranslations`)

**Files:**
- Create: `src/lib/types.ts`, `src/i18n/ui.ts`, `tests/i18n.test.ts`

**Interfaces:**
- Produces:
  - `type Locale = 'en' | 'ko'`, `AppStatus`, `Platform`, `LinkKey`, `interface AppData` (in `types.ts`).
  - `ui` (record keyed by locale), `useTranslations(locale: Locale): (key: UIKey) => string`, `type UIKey`, `languages` (in `ui.ts`).

- [ ] **Step 1: Write the failing test `tests/i18n.test.ts`**

```ts
import { describe, it, expect } from 'vitest';
import { useTranslations } from '../src/i18n/ui';

describe('useTranslations', () => {
  it('returns the localized string for a key', () => {
    expect(useTranslations('en')('nav.apps')).toBe('Apps');
    expect(useTranslations('ko')('nav.apps')).toBe('앱');
  });

  it('returns the hero title parts per locale', () => {
    expect(useTranslations('en')('hero.title.line2')).toBe('language games.');
    expect(useTranslations('ko')('hero.title.line2')).toBe('언어가 들리기 시작합니다.');
  });
});
```

- [ ] **Step 2: Run it and verify failure**

Run: `npm test -- i18n`
Expected: FAIL — cannot find module `../src/i18n/ui`.

- [ ] **Step 3: Create `src/lib/types.ts`**

```ts
export type Locale = 'en' | 'ko';
export type AppStatus = 'live' | 'pre-launch' | 'waitlist' | 'wip';
export type Platform = 'ios' | 'android' | 'web';
export type LinkKey = 'site' | 'appStore' | 'playStore' | 'youtube' | 'waitlist';

export interface AppData {
  name: string;
  slug: string;
  status: AppStatus;
  accent: string;
  order: number;
  featured: boolean;
  platforms: Platform[];
  learn: { language: string; forSpeakers: string };
  icon: string;
  screenshot?: string;
  links: Partial<Record<LinkKey, string>>;
  tagline: Record<Locale, string>;
  description: Record<Locale, string>;
}
```

- [ ] **Step 4: Create `src/i18n/ui.ts`**

```ts
import type { Locale } from '../lib/types';

export const languages: Record<Locale, string> = { en: 'English', ko: '한국어' };
export const defaultLocale: Locale = 'en';

export const ui = {
  en: {
    'meta.title': 'anonpengling — daily language games',
    'meta.description':
      'An independent app studio building daily language-learning games. Meet Fulang and Storyfluent.',
    'nav.apps': 'Apps',
    'nav.about': 'About',
    'nav.youtube': 'YouTube',
    'hero.eyebrow': 'Independent app studio',
    'hero.title.line1': 'We build daily',
    'hero.title.line2': 'language games.',
    'hero.subtitle':
      'One round a day, and a language starts to click. A small studio turning listening, reading, and speaking into games.',
    'hero.cta.apps': 'See the apps',
    'hero.cta.youtube': 'Watch on YouTube',
    'apps.heading': 'Apps',
    'apps.note': 'apps shipping · more on the way',
    'how.heading': 'How we work',
    'how.daily.title': 'Daily by design',
    'how.daily.body': 'Short sessions, every day. SRS and game mechanics make it a habit.',
    'how.engine.title': 'Content engine',
    'how.engine.body': 'The app plus an automated YouTube pipeline ship fresh content daily.',
    'how.small.title': 'Small & independent',
    'how.small.body': 'A one-person studio. Built fast, refined in the open.',
    'follow.heading': 'Follow the build',
    'follow.body': 'New apps and updates, plus daily content on YouTube.',
    'follow.cta': 'Watch on YouTube',
    'footer.built': 'Built with Astro',
    'app.cta.site': 'Visit',
    'app.cta.appStore': 'App Store',
    'app.cta.playStore': 'Google Play',
    'app.cta.youtube': 'YouTube',
    'app.cta.waitlist': 'Join waitlist',
    'app.learnedBy': 'for',
  },
  ko: {
    'meta.title': 'anonpengling — 매일 한 판의 언어 학습 앱',
    'meta.description':
      '언어를 게임처럼 배우는 앱을 만드는 인디 스튜디오. Fulang과 Storyfluent.',
    'nav.apps': '앱',
    'nav.about': '소개',
    'nav.youtube': 'YouTube',
    'hero.eyebrow': '인디 앱 스튜디오',
    'hero.title.line1': '매일 한 판,',
    'hero.title.line2': '언어가 들리기 시작합니다.',
    'hero.subtitle':
      '하루 한 판이면 언어가 들리기 시작합니다. 듣기·읽기·말하기를 게임으로 만드는 작은 스튜디오.',
    'hero.cta.apps': '앱 보기',
    'hero.cta.youtube': 'YouTube에서 보기',
    'apps.heading': '앱',
    'apps.note': '개 출시 · 더 준비 중',
    'how.heading': '우리가 일하는 방식',
    'how.daily.title': '매일이 기본',
    'how.daily.body': '짧게, 매일. SRS와 게임 메커니즘으로 습관이 됩니다.',
    'how.engine.title': '콘텐츠 엔진',
    'how.engine.body': '앱과 자동화된 YouTube 파이프라인이 매일 새 콘텐츠를 냅니다.',
    'how.small.title': '작고 독립적',
    'how.small.body': '1인 스튜디오. 빠르게 만들고 공개적으로 다듬습니다.',
    'follow.heading': '제작 과정 팔로우',
    'follow.body': '새 앱과 업데이트 소식, 그리고 YouTube의 매일 콘텐츠.',
    'follow.cta': 'YouTube에서 보기',
    'footer.built': 'Astro로 제작',
    'app.cta.site': '바로가기',
    'app.cta.appStore': 'App Store',
    'app.cta.playStore': 'Google Play',
    'app.cta.youtube': 'YouTube',
    'app.cta.waitlist': '대기자 등록',
    'app.learnedBy': '대상',
  },
} as const;

export type UIKey = keyof (typeof ui)['en'];

export function useTranslations(locale: Locale) {
  return function t(key: UIKey): string {
    return (ui[locale] as Record<UIKey, string>)[key] ?? ui[defaultLocale][key];
  };
}
```

- [ ] **Step 5: Run the test and verify it passes**

Run: `npm test -- i18n`
Expected: PASS (both tests green).

- [ ] **Step 6: Commit**

```bash
git add -A
git commit -m "feat(i18n): shared types and EN/KO UI strings with useTranslations"
```

---

### Task 3: App display helpers (pure logic, TDD)

**Files:**
- Create: `src/lib/app-display.ts`, `tests/app-display.test.ts`

**Interfaces:**
- Consumes: `AppData`, `Locale`, `LinkKey`, `AppStatus` from `src/lib/types.ts`.
- Produces:
  - `sortApps(apps: AppData[]): AppData[]`
  - `visibleLinks(app: AppData): { key: LinkKey; url: string }[]`
  - `statusLabel(status: AppStatus, locale: Locale): string`
  - `localized<T>(field: Partial<Record<Locale, T>>, locale: Locale): T | undefined`

- [ ] **Step 1: Write the failing test `tests/app-display.test.ts`**

```ts
import { describe, it, expect } from 'vitest';
import { sortApps, visibleLinks, statusLabel, localized } from '../src/lib/app-display';
import type { AppData } from '../src/lib/types';

const base: AppData = {
  name: 'X', slug: 'x', status: 'live', accent: '#000', order: 2, featured: true,
  platforms: ['ios'], learn: { language: 'L', forSpeakers: 'S' }, icon: '/i.png',
  links: {}, tagline: { en: 't', ko: 'ㅌ' }, description: { en: 'd', ko: 'ㄷ' },
};

describe('sortApps', () => {
  it('orders by order ascending and does not mutate input', () => {
    const a = { ...base, order: 2, name: 'A' };
    const b = { ...base, order: 1, name: 'B' };
    const input = [a, b];
    expect(sortApps(input).map((x) => x.name)).toEqual(['B', 'A']);
    expect(input.map((x) => x.name)).toEqual(['A', 'B']); // original untouched
  });
});

describe('visibleLinks', () => {
  it('returns only present links in priority order', () => {
    const app = { ...base, links: { youtube: 'https://y', waitlist: 'https://w', site: 'https://s' } };
    expect(visibleLinks(app).map((l) => l.key)).toEqual(['waitlist', 'site', 'youtube']);
  });
  it('returns empty array when there are no links', () => {
    expect(visibleLinks(base)).toEqual([]);
  });
});

describe('statusLabel', () => {
  it('localizes the status', () => {
    expect(statusLabel('pre-launch', 'en')).toBe('Pre-launch');
    expect(statusLabel('pre-launch', 'ko')).toBe('출시 예정');
    expect(statusLabel('live', 'ko')).toBe('출시됨');
  });
});

describe('localized', () => {
  it('returns the locale value when present', () => {
    expect(localized({ en: 'hi', ko: '안녕' }, 'ko')).toBe('안녕');
  });
  it('falls back to en when the locale value is missing', () => {
    expect(localized({ en: 'hi' }, 'ko')).toBe('hi');
  });
});
```

- [ ] **Step 2: Run it and verify failure**

Run: `npm test -- app-display`
Expected: FAIL — cannot find module `../src/lib/app-display`.

- [ ] **Step 3: Create `src/lib/app-display.ts`**

```ts
import type { AppData, AppStatus, Locale, LinkKey } from './types';

export function sortApps(apps: AppData[]): AppData[] {
  return [...apps].sort((a, b) => a.order - b.order);
}

const LINK_PRIORITY: LinkKey[] = ['waitlist', 'appStore', 'playStore', 'site', 'youtube'];

export function visibleLinks(app: AppData): { key: LinkKey; url: string }[] {
  return LINK_PRIORITY
    .filter((k) => Boolean(app.links[k]))
    .map((k) => ({ key: k, url: app.links[k] as string }));
}

const STATUS_LABELS: Record<Locale, Record<AppStatus, string>> = {
  en: { live: 'Live', 'pre-launch': 'Pre-launch', waitlist: 'Waitlist', wip: 'In progress' },
  ko: { live: '출시됨', 'pre-launch': '출시 예정', waitlist: '대기자 모집', wip: '개발 중' },
};

export function statusLabel(status: AppStatus, locale: Locale): string {
  return STATUS_LABELS[locale][status];
}

export function localized<T>(field: Partial<Record<Locale, T>>, locale: Locale): T | undefined {
  return field[locale] ?? field.en;
}
```

- [ ] **Step 4: Run the test and verify it passes**

Run: `npm test -- app-display`
Expected: PASS (all cases green).

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(lib): app sorting, link visibility, status/locale helpers"
```

---

### Task 4: Apps content collection + entries

**Files:**
- Create: `src/content/config.ts`, `src/content/apps/fulang.md`, `src/content/apps/storyfluent.md`

**Interfaces:**
- Consumes: nothing from other tasks (zod schema is standalone); mirrors `AppData` from Task 2.
- Produces: collection `apps`, queryable later via `getCollection('apps')`. Each entry's `data` matches `AppData` minus Astro's added `id`/`slug` wrapper (note: Astro exposes frontmatter under `entry.data`).

- [ ] **Step 1: Create `src/content/config.ts`**

```ts
import { defineCollection, z } from 'astro:content';

const apps = defineCollection({
  type: 'content',
  schema: z.object({
    name: z.string(),
    slug: z.string(),
    status: z.enum(['live', 'pre-launch', 'waitlist', 'wip']),
    accent: z.string(),
    order: z.number(),
    featured: z.boolean().default(true),
    platforms: z.array(z.enum(['ios', 'android', 'web'])),
    learn: z.object({ language: z.string(), forSpeakers: z.string() }),
    icon: z.string(),
    screenshot: z.string().optional(),
    links: z.object({
      site: z.string().url().optional(),
      appStore: z.string().url().optional(),
      playStore: z.string().url().optional(),
      youtube: z.string().url().optional(),
      waitlist: z.string().url().optional(),
    }),
    tagline: z.object({ en: z.string(), ko: z.string() }),
    description: z.object({ en: z.string(), ko: z.string() }),
  }),
});

export const collections = { apps };
```

- [ ] **Step 2: Create `src/content/apps/fulang.md`**

```markdown
---
name: Fulang
slug: fulang
status: pre-launch
accent: "#C8102E"
order: 1
featured: true
platforms: [ios, web]
learn:
  language: Taiwanese Mandarin
  forSpeakers: EN · KO
icon: /apps/fulang/icon.png
screenshot: /apps/fulang/hero.png
links:
  site: https://fulang.anonpengling.org
  youtube: https://www.youtube.com/@fulang.anonpengling
  waitlist: https://fulang.anonpengling.org
tagline:
  en: Real Taiwan Mandarin in Zhuyin, as a daily game.
  ko: 注音으로 배우는 정통 대만 만다린, 매일 게임처럼.
description:
  en: Taiwanese Mandarin in Zhuyin — from listening dialogues to stroke-by-stroke handwriting, one round a day.
  ko: 注音으로 배우는 정통 대만 만다린. 듣기 대화부터 손글씨까지, 매일 한 판의 게임으로.
---
```

- [ ] **Step 3: Create `src/content/apps/storyfluent.md`**

```markdown
---
name: Storyfluent
slug: storyfluent
status: live
accent: "#D4AF37"
order: 2
featured: true
platforms: [ios, android]
learn:
  language: English through stories
  forSpeakers: KO
icon: /apps/storyfluent/icon.png
screenshot: /apps/storyfluent/hero.png
links:
  site: https://storyfluent.anonpengling.org
  appStore: https://apps.apple.com/app/storyfluent
  youtube: https://www.youtube.com/@storyfluent_kr
tagline:
  en: Learn English by listening to classic short stories.
  ko: 고전 단편으로 듣는 영어.
description:
  en: Classic fables and short stories with word-level subtitles and shadowing — an audiobook that teaches your ears.
  ko: 고전 단편·우화로 듣는 영어. 단어별 자막과 따라 말하기(쉐도잉)로 귀가 트이는 오디오북 학습.
---
```

- [ ] **Step 4: Verify the schema validates both entries**

Run: `npm run check`
Expected: `astro check` reports 0 errors (zod accepts both entries; if a URL or enum is wrong it fails here). If `astro check` complains about missing pages/types, run `npx astro sync` first, then re-run.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "feat(content): apps collection schema + fulang & storyfluent entries"
```

> Note: real `icon`/`screenshot` PNGs are added in Task 9; until then `AppCard` (Task 6) falls back to an accent gradient placeholder, so the build is not blocked.

---

### Task 5: Design tokens + Base layout

**Files:**
- Create: `src/styles/tokens.css`, `src/layouts/Base.astro`

**Interfaces:**
- Consumes: `useTranslations`, `type Locale` (Task 2).
- Produces: `Base.astro` — props `{ locale: Locale }`, renders `<html>` head (meta/OG/title from `useTranslations`) + a `<slot/>`. Global tokens available to all components.

- [ ] **Step 1: Create `src/styles/tokens.css`**

```css
:root {
  --bg: #0b0e13;
  --card: #12171f;
  --border: #232a34;
  --text: #e8eaed;
  --text-2: #9aa3af;
  --text-3: #5f6875;
  --accent-1: #67e8f9;
  --accent-2: #818cf8;
  --accent-grad: linear-gradient(90deg, #67e8f9, #818cf8);
  --maxw: 960px;
  --radius: 16px;
  --space: 22px;
  --font: system-ui, -apple-system, "Segoe UI", Roboto, "Noto Sans KR", Pretendard, sans-serif;
}

* { box-sizing: border-box; }
html { background: var(--bg); color-scheme: dark; }
body {
  margin: 0;
  font-family: var(--font);
  color: var(--text);
  background: var(--bg);
  line-height: 1.5;
  -webkit-font-smoothing: antialiased;
}
a { color: inherit; text-decoration: none; }
.container { max-width: var(--maxw); margin: 0 auto; padding: 0 var(--space); }
.gradient-text {
  background: var(--accent-grad);
  -webkit-background-clip: text;
  background-clip: text;
  color: transparent;
}
.eyebrow {
  font-size: 12px; letter-spacing: 0.16em; text-transform: uppercase;
  color: var(--accent-1);
}
.section-label {
  font-size: 12px; letter-spacing: 0.12em; text-transform: uppercase;
  color: var(--text-3);
}
.btn {
  display: inline-block; font-size: 13px; padding: 9px 16px; border-radius: 10px;
  border: 1px solid var(--border); color: var(--text); background: transparent;
}
.btn-primary {
  border: none; color: #06121f; font-weight: 700;
  background: var(--accent-grad);
}
@media (prefers-reduced-motion: reduce) {
  * { animation: none !important; transition: none !important; }
}
```

- [ ] **Step 2: Create `src/layouts/Base.astro`**

```astro
---
import '../styles/tokens.css';
import type { Locale } from '../lib/types';
import { useTranslations } from '../i18n/ui';

interface Props { locale: Locale; }
const { locale } = Astro.props;
const t = useTranslations(locale);
const canonical = new URL(Astro.url.pathname, Astro.site).href;
---
<!doctype html>
<html lang={locale}>
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>{t('meta.title')}</title>
    <meta name="description" content={t('meta.description')} />
    <link rel="canonical" href={canonical} />
    <link rel="icon" href="/favicon.svg" type="image/svg+xml" />
    <meta property="og:type" content="website" />
    <meta property="og:title" content={t('meta.title')} />
    <meta property="og:description" content={t('meta.description')} />
    <meta property="og:image" content={new URL('/og.png', Astro.site).href} />
    <meta property="og:locale" content={locale === 'ko' ? 'ko_KR' : 'en_US'} />
    <meta name="twitter:card" content="summary_large_image" />
  </head>
  <body>
    <slot />
  </body>
</html>
```

- [ ] **Step 3: Verify it builds (type-check)**

Run: `npm run check`
Expected: 0 errors.

- [ ] **Step 4: Commit**

```bash
git add -A
git commit -m "feat(ui): design tokens and Base layout with SEO/OG meta"
```

---

### Task 6: AppCard component (feature tile + compact fallback)

**Files:**
- Create: `src/components/AppCard.astro`

**Interfaces:**
- Consumes: `AppData`, `Locale` (types); `visibleLinks`, `statusLabel`, `localized` (Task 3); `useTranslations` (Task 2).
- Produces: `AppCard.astro` — props `{ app: AppData; locale: Locale }`. Renders a feature tile when `app.featured`, else a compact row. Falls back to an accent gradient block when `app.screenshot` is absent.

- [ ] **Step 1: Create `src/components/AppCard.astro`**

```astro
---
import type { AppData, Locale } from '../lib/types';
import { visibleLinks, statusLabel, localized } from '../lib/app-display';
import { useTranslations } from '../i18n/ui';

interface Props { app: AppData; locale: Locale; }
const { app, locale } = Astro.props;
const t = useTranslations(locale);
const links = visibleLinks(app);
const tagline = localized(app.tagline, locale);
const desc = localized(app.description, locale);
const status = statusLabel(app.status, locale);
const ctaKey = (key: string) => `app.cta.${key}` as
  'app.cta.site' | 'app.cta.appStore' | 'app.cta.playStore' | 'app.cta.youtube' | 'app.cta.waitlist';
---
<article class={`card ${app.featured ? 'card--feature' : 'card--row'}`} style={`--accent:${app.accent}`}>
  <div class="card__media">
    {app.screenshot
      ? <img src={app.screenshot} alt={`${app.name} screenshot`} loading="lazy" width="74" height="140" />
      : <div class="card__phone" aria-hidden="true"></div>}
  </div>
  <div class="card__body">
    <div class="card__head">
      <img class="card__icon" src={app.icon} alt="" width="26" height="26" />
      <span class="card__name">{app.name}</span>
      <span class={`badge badge--${app.status}`}>{status}</span>
    </div>
    <p class="card__tagline">{desc ?? tagline}</p>
    <p class="card__meta">{app.learn.language} · {t('app.learnedBy')} {app.learn.forSpeakers} · {app.platforms.join(' · ')}</p>
    <div class="card__links">
      {links.map((l) => (
        <a class={l.key === 'waitlist' || l.key === 'appStore' ? 'btn btn-primary' : 'btn'}
           href={l.url} target="_blank" rel="noopener">{t(ctaKey(l.key))}</a>
      ))}
    </div>
  </div>
</article>

<style>
  .card {
    border: 1px solid var(--border);
    background: var(--card);
    border-radius: var(--radius);
    overflow: hidden;
    display: flex;
  }
  .card--row { align-items: center; }
  .card__media {
    width: 130px; flex: none;
    display: flex; align-items: center; justify-content: center; padding: 16px;
    background: radial-gradient(120% 90% at 50% 0%, color-mix(in srgb, var(--accent) 40%, transparent), var(--bg));
  }
  .card--row .card__media { width: 64px; padding: 12px; }
  .card__phone {
    width: 74px; height: 140px; border-radius: 15px;
    border: 2px solid #3a4250; background: linear-gradient(#1b2330, #0e1116);
  }
  .card--row .card__phone { width: 34px; height: 34px; border-radius: 9px; }
  .card__media img { border-radius: 14px; }
  .card__body { padding: 18px; flex: 1; min-width: 0; }
  .card__head { display: flex; align-items: center; gap: 9px; }
  .card__icon { border-radius: 7px; }
  .card__name { font-size: 16px; font-weight: 800; }
  .badge {
    font-size: 9px; border-radius: 999px; padding: 2px 8px;
    border: 1px solid var(--border); color: var(--text-2);
  }
  .badge--live { color: #7bd88f; border-color: #28503a; }
  .badge--pre-launch { color: #ff8aa0; border-color: #5a2730; }
  .card__tagline { font-size: 12.5px; color: var(--text-2); margin: 7px 0 0; }
  .card__meta { font-size: 11px; color: var(--text-3); margin: 8px 0 0; }
  .card__links { display: flex; gap: 8px; margin-top: 12px; flex-wrap: wrap; }
  .card--row .card__media { display: none; }
  @media (max-width: 640px) {
    .card--feature { flex-direction: column; }
    .card--feature .card__media { width: 100%; }
  }
</style>
```

- [ ] **Step 2: Verify type-check passes**

Run: `npm run check`
Expected: 0 errors. (Component is exercised by the build in Task 9; this step gates types only.)

- [ ] **Step 3: Commit**

```bash
git add -A
git commit -m "feat(ui): AppCard feature tile with screenshot fallback + compact row"
```

---

### Task 7: Nav + LangToggle

**Files:**
- Create: `src/components/Nav.astro`, `src/components/LangToggle.astro`

**Interfaces:**
- Consumes: `Locale` (types), `useTranslations` (Task 2).
- Produces: `Nav.astro` props `{ locale: Locale }`; `LangToggle.astro` props `{ locale: Locale }` (links `/` ⇄ `/ko`).

- [ ] **Step 1: Create `src/components/LangToggle.astro`**

```astro
---
import type { Locale } from '../lib/types';
interface Props { locale: Locale; }
const { locale } = Astro.props;
---
<div class="lang">
  <a href="/" aria-current={locale === 'en' ? 'true' : undefined} class={locale === 'en' ? 'on' : ''}>EN</a>
  <span>·</span>
  <a href="/ko" aria-current={locale === 'ko' ? 'true' : undefined} class={locale === 'ko' ? 'on' : ''}>한</a>
</div>
<style>
  .lang { display: inline-flex; gap: 6px; align-items: center; font-size: 12px;
    border: 1px solid var(--border); border-radius: 7px; padding: 3px 8px; color: var(--text-3); }
  .lang a.on { color: var(--text); font-weight: 700; }
</style>
```

- [ ] **Step 2: Create `src/components/Nav.astro`**

```astro
---
import type { Locale } from '../lib/types';
import { useTranslations } from '../i18n/ui';
import LangToggle from './LangToggle.astro';
interface Props { locale: Locale; }
const { locale } = Astro.props;
const t = useTranslations(locale);
---
<header class="nav">
  <div class="container nav__inner">
    <a class="nav__brand" href={locale === 'ko' ? '/ko' : '/'}>
      <span class="nav__mark" aria-hidden="true"></span>
      <span class="gradient-text">anonpengling</span>
    </a>
    <nav class="nav__links">
      <a href="#apps">{t('nav.apps')}</a>
      <a href="#about">{t('nav.about')}</a>
      <a href="#follow">{t('nav.youtube')}</a>
      <LangToggle locale={locale} />
    </nav>
  </div>
</header>
<style>
  .nav { border-bottom: 1px solid #1c222b; }
  .nav__inner { display: flex; justify-content: space-between; align-items: center; padding: 14px 0; }
  .nav__brand { display: flex; align-items: center; gap: 9px; font-size: 16px; font-weight: 800; }
  .nav__mark { width: 26px; height: 26px; border-radius: 8px; background: var(--accent-grad); }
  .nav__links { display: flex; gap: 16px; align-items: center; font-size: 13px; color: var(--text-2); }
</style>
```

- [ ] **Step 3: Verify type-check**

Run: `npm run check`
Expected: 0 errors.

- [ ] **Step 4: Commit**

```bash
git add -A
git commit -m "feat(ui): Nav and language toggle"
```

---

### Task 8: Hero, HowWeWork, Follow, Footer

**Files:**
- Create: `src/components/Hero.astro`, `src/components/HowWeWork.astro`, `src/components/Follow.astro`, `src/components/Footer.astro`

**Interfaces:**
- Consumes: `Locale` (types), `useTranslations` (Task 2).
- Produces: four section components, each props `{ locale: Locale }`. `Follow` links to the studio YouTube presence (no email backend per spec §3.8).

- [ ] **Step 1: Create `src/components/Hero.astro`**

```astro
---
import type { Locale } from '../lib/types';
import { useTranslations } from '../i18n/ui';
interface Props { locale: Locale; }
const { locale } = Astro.props;
const t = useTranslations(locale);
const youtube = 'https://www.youtube.com/@fulang.anonpengling';
---
<section class="hero">
  <div class="container">
    <p class="eyebrow">{t('hero.eyebrow')}</p>
    <h1 class="hero__title">{t('hero.title.line1')}<br /><span class="gradient-text">{t('hero.title.line2')}</span></h1>
    <p class="hero__sub">{t('hero.subtitle')}</p>
    <div class="hero__cta">
      <a class="btn btn-primary" href="#apps">{t('hero.cta.apps')} ↓</a>
      <a class="btn" href={youtube} target="_blank" rel="noopener">{t('hero.cta.youtube')} ↗</a>
    </div>
  </div>
</section>
<style>
  .hero { padding: 54px 0 44px; text-align: center;
    background: radial-gradient(80% 120% at 50% -10%, rgba(99,102,241,.18), transparent 60%); }
  .hero__title { font-size: clamp(30px, 6vw, 40px); font-weight: 850; line-height: 1.12;
    max-width: 600px; margin: 14px auto 0; }
  .hero__sub { font-size: 14px; color: var(--text-2); max-width: 460px; margin: 14px auto 0; }
  .hero__cta { display: flex; gap: 10px; justify-content: center; margin-top: 22px; flex-wrap: wrap; }
</style>
```

- [ ] **Step 2: Create `src/components/HowWeWork.astro`**

```astro
---
import type { Locale } from '../lib/types';
import { useTranslations } from '../i18n/ui';
interface Props { locale: Locale; }
const { locale } = Astro.props;
const t = useTranslations(locale);
const cards = [
  { title: t('how.daily.title'), body: t('how.daily.body'), color: 'var(--accent-1)' },
  { title: t('how.engine.title'), body: t('how.engine.body'), color: 'var(--accent-2)' },
  { title: t('how.small.title'), body: t('how.small.body'), color: '#7bd88f' },
];
---
<section id="about" class="how">
  <div class="container">
    <p class="section-label">{t('how.heading')}</p>
    <div class="how__grid">
      {cards.map((c) => (
        <div class="how__card">
          <div class="how__title" style={`color:${c.color}`}>{c.title}</div>
          <p class="how__body">{c.body}</p>
        </div>
      ))}
    </div>
  </div>
</section>
<style>
  .how { padding: 28px 0; border-top: 1px solid #1c222b; }
  .how__grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 12px; margin-top: 14px; }
  .how__card { border: 1px solid var(--border); border-radius: 12px; padding: 14px; }
  .how__title { font-size: 13px; font-weight: 700; }
  .how__body { font-size: 12px; color: var(--text-2); margin: 5px 0 0; }
  @media (max-width: 640px) { .how__grid { grid-template-columns: 1fr; } }
</style>
```

- [ ] **Step 3: Create `src/components/Follow.astro`**

```astro
---
import type { Locale } from '../lib/types';
import { useTranslations } from '../i18n/ui';
interface Props { locale: Locale; }
const { locale } = Astro.props;
const t = useTranslations(locale);
const youtube = 'https://www.youtube.com/@fulang.anonpengling';
---
<section id="follow" class="follow">
  <div class="container follow__inner">
    <div>
      <h2 class="follow__title">{t('follow.heading')}</h2>
      <p class="follow__body">{t('follow.body')}</p>
    </div>
    <a class="btn btn-primary" href={youtube} target="_blank" rel="noopener">{t('follow.cta')} ↗</a>
  </div>
</section>
<style>
  .follow { padding: 28px 0; border-top: 1px solid #1c222b; }
  .follow__inner { display: flex; justify-content: space-between; align-items: center; gap: 16px; flex-wrap: wrap; }
  .follow__title { font-size: 18px; font-weight: 800; margin: 0; }
  .follow__body { font-size: 12px; color: var(--text-2); margin: 4px 0 0; }
</style>
```

- [ ] **Step 4: Create `src/components/Footer.astro`**

```astro
---
import type { Locale } from '../lib/types';
import { useTranslations } from '../i18n/ui';
interface Props { locale: Locale; }
const { locale } = Astro.props;
const t = useTranslations(locale);
---
<footer class="footer">
  <div class="container footer__inner">
    <span>© 2026 anonpengling</span>
    <span>
      <a href="https://github.com/mandoo180" target="_blank" rel="noopener">GitHub</a> · {t('footer.built')}
    </span>
  </div>
</footer>
<style>
  .footer { padding: 18px 0; border-top: 1px solid #1c222b; }
  .footer__inner { display: flex; justify-content: space-between; font-size: 11px; color: var(--text-3); }
</style>
```

- [ ] **Step 5: Verify type-check**

Run: `npm run check`
Expected: 0 errors.

- [ ] **Step 6: Commit**

```bash
git add -A
git commit -m "feat(ui): Hero, HowWeWork, Follow, Footer sections"
```

---

### Task 9: Assemble homepages (EN + KO) + static assets

**Files:**
- Create: `src/pages/index.astro`, `src/pages/ko/index.astro`
- Create: `public/CNAME`, `public/favicon.svg`, `public/og.png`, `public/apps/fulang/icon.png`, `public/apps/fulang/hero.png`, `public/apps/storyfluent/icon.png`, `public/apps/storyfluent/hero.png`

**Interfaces:**
- Consumes: every component (Tasks 6–8), `getCollection`, `sortApps` (Task 3), `AppData` (Task 2).
- Produces: the two rendered homepages. App entries are mapped from `entry.data` into `AppData` for the components.

- [ ] **Step 1: Create a shared page body via `src/pages/index.astro` (EN)**

```astro
---
import { getCollection } from 'astro:content';
import type { AppData, Locale } from '../lib/types';
import { sortApps } from '../lib/app-display';
import { useTranslations } from '../i18n/ui';
import Base from '../layouts/Base.astro';
import Nav from '../components/Nav.astro';
import Hero from '../components/Hero.astro';
import AppCard from '../components/AppCard.astro';
import HowWeWork from '../components/HowWeWork.astro';
import Follow from '../components/Follow.astro';
import Footer from '../components/Footer.astro';

const locale: Locale = 'en';
const t = useTranslations(locale);
const entries = await getCollection('apps');
const apps = sortApps(entries.map((e) => e.data as AppData));
const featured = apps.filter((a) => a.featured);
const rest = apps.filter((a) => !a.featured);
const shipping = apps.filter((a) => a.status === 'live' || a.status === 'pre-launch').length;
---
<Base locale={locale}>
  <Nav locale={locale} />
  <main>
    <Hero locale={locale} />
    <section id="apps" class="apps">
      <div class="container apps__head">
        <p class="section-label">{t('apps.heading')}</p>
        <span class="apps__note">{shipping} {t('apps.note')}</span>
      </div>
      <div class="container apps__list">
        {featured.map((app) => <AppCard app={app} locale={locale} />)}
        {rest.map((app) => <AppCard app={app} locale={locale} />)}
      </div>
    </section>
    <HowWeWork locale={locale} />
    <Follow locale={locale} />
  </main>
  <Footer locale={locale} />
</Base>

<style>
  .apps { padding: 10px 0 8px; }
  .apps__head { display: flex; justify-content: space-between; align-items: baseline; }
  .apps__note { font-size: 11px; color: var(--text-3); }
  .apps__list { display: flex; flex-direction: column; gap: 14px; margin-top: 8px; }
</style>
```

- [ ] **Step 2: Create `src/pages/ko/index.astro` (KO)**

Identical to Step 1 except the locale line. Repeat the full file with this one change:

```astro
const locale: Locale = 'ko';
```

(Every other line — imports, getCollection, the markup, and the `<style>` block — is exactly as in `src/pages/index.astro`.)

- [ ] **Step 3: Create `public/CNAME`**

```
anonpengling.org
```

- [ ] **Step 4: Create `public/favicon.svg`**

```svg
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32">
  <defs><linearGradient id="g" x1="0" y1="0" x2="1" y2="1">
    <stop offset="0" stop-color="#67e8f9"/><stop offset="1" stop-color="#818cf8"/>
  </linearGradient></defs>
  <rect width="32" height="32" rx="8" fill="url(#g)"/>
</svg>
```

- [ ] **Step 5: Add placeholder raster assets**

Create 1×1 (or any) PNGs so `<img>` paths resolve; replace with real art later. Run:
```bash
mkdir -p public/apps/fulang public/apps/storyfluent
# minimal valid 1x1 transparent PNG
B64="iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg=="
for f in public/og.png public/apps/fulang/icon.png public/apps/fulang/hero.png \
         public/apps/storyfluent/icon.png public/apps/storyfluent/hero.png; do
  printf '%s' "$B64" | base64 --decode > "$f"
done
```
Expected: five PNG files created. (Note in commit body that these are placeholders pending real screenshots — see plan §"Open items".)

- [ ] **Step 6: Build and verify both locales render**

Run: `npm run build`
Expected: build succeeds; `dist/index.html` and `dist/ko/index.html` exist. Verify:
```bash
ls dist/index.html dist/ko/index.html dist/CNAME
grep -q "anonpengling" dist/index.html && grep -q "Fulang" dist/index.html && echo OK_EN
grep -q "매일 한 판" dist/ko/index.html && echo OK_KO
```
Expected: both files exist, `dist/CNAME` present, `OK_EN` and `OK_KO` printed.

- [ ] **Step 7: Spot-check in the browser**

Run: `npm run preview` and open the printed URL; confirm `/` (EN) and `/ko` (KO), the lang toggle switches, app cards show with accent gradients, and the layout is responsive (narrow the window — feature tiles stack).

- [ ] **Step 8: Commit**

```bash
git add -A
git commit -m "feat(pages): EN/KO homepages, CNAME, favicon, placeholder assets"
```

---

### Task 10: Final verification & merge readiness

**Files:**
- None (verification + optional branch finish).

**Interfaces:**
- Consumes: the whole site.

- [ ] **Step 1: Full gate — tests, types, build all green**

Run: `npm test && npm run check && npm run build`
Expected: tests pass, `astro check` 0 errors, build succeeds.

- [ ] **Step 2: Confirm the deploy workflow is the only Pages workflow**

Run: `ls .github/workflows`
Expected: only `deploy.yml` (the old `publish.yml` was removed in the migration plan).

- [ ] **Step 3: Confirm clean git state**

Run: `git status --short`
Expected: empty (all work committed).

- [ ] **Step 4: Hand off for merge + Pages domain setup**

Per `superpowers:finishing-a-development-branch`, decide how to integrate `feat/anonpengling-studio-hub` → `main`. After merge to `main`, the deploy workflow runs. Then set the apex custom domain (one-time, external):
```bash
gh api -X PUT repos/mandoo180/mandoo180.github.io/pages -f cname=anonpengling.org -F https_enforced=true
ZONE_ID=$(aws route53 list-hosted-zones-by-name --dns-name anonpengling.org. \
  --query "HostedZones[?Name=='anonpengling.org.'].Id" --output text | sed 's#/hostedzone/##')
aws route53 change-resource-record-sets --hosted-zone-id "$ZONE_ID" --change-batch '{
  "Comment":"apex -> GitHub Pages",
  "Changes":[{"Action":"UPSERT","ResourceRecordSet":{"Name":"anonpengling.org","Type":"A","TTL":300,
    "ResourceRecords":[{"Value":"185.199.108.153"},{"Value":"185.199.109.153"},
                       {"Value":"185.199.110.153"},{"Value":"185.199.111.153"}]}}]}'
```
Verify: `curl -sI https://anonpengling.org | head -1` → `HTTP/2 200` (allow time for DNS + cert).

---

## Self-Review

**1. Spec coverage:**
- §3.3 directory structure → Tasks 1,4,5,6,7,8,9 create the specified files. ✓
- §3.4 content collection schema → Task 4 (zod mirrors `AppData`). ✓
- §3.5 scalability (featured vs compact) → `AppCard` row variant (Task 6) + `featured`/`rest` split (Task 9). ✓
- §3.6 design tokens → Task 5 `tokens.css` with exact hex values. ✓
- §3.7 deployment (site, sitemap, i18n, CNAME, workflow) → Tasks 1,9; Route53 apex in Task 10. ✓
- §3.8 Follow = links, no email backend → `Follow.astro` (Task 8). ✓
- §4 data flow (getCollection → sort → featured split → AppCard) → Task 9. ✓
- §5 edge cases: screenshot fallback (Task 6), partial links via `visibleLinks` (Task 3/6), ko→en fallback via `localized` (Task 3), schema gate via `astro check` (Task 4), reduced-motion (Task 5). ✓
- §6 testing: vitest units (Tasks 2,3), build+preview (Task 9), responsive spot-check (Task 9 Step 7). ✓

**2. Placeholder scan:** No "TBD"/"add error handling"/"similar to". The KO page (Task 9 Step 2) is explicitly "repeat the file with one changed line" per the no-"similar-to" rule — the one differing line is shown. Placeholder PNGs are real generated files flagged for later replacement, not plan placeholders.

**3. Type consistency:** `Locale`/`AppData`/`AppStatus`/`Platform`/`LinkKey` defined once in `src/lib/types.ts` (Task 2) and imported everywhere. `useTranslations`/`UIKey` from `src/i18n/ui.ts`. `sortApps`/`visibleLinks`/`statusLabel`/`localized` signatures match between Task 3 definitions and Task 6/9 call sites. UI keys used in components all exist in `ui` (Task 2). ✓

## Open items (carried from spec §8)
- Real `icon`/`screenshot`/`og.png` art (placeholders generated in Task 9 Step 5).
- Display font choice (currently system stack; swap in `tokens.css --font`).
- Studio email capture (deferred; `Follow` links to YouTube per §3.8).
