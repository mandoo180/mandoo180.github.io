# anonpengling 스튜디오 허브 — 디자인 스펙

- **작성일:** 2026-06-28
- **레포:** `mandoo180.github.io` (재구성)
- **상태:** 디자인 확정, 구현 계획 대기

## 1. 목표 (Why)

`mandoo180.github.io`를 Emacs Org-mode 개발 노트 사이트에서 **anonpengling 인디 앱 스튜디오 허브**로 재구성한다. fulang·storyfluent 및 앞으로 만들 같은 류의 앱(언어를 게임처럼 배우는 앱)을 한곳에 모아 보여주는 우산 브랜드 대문이다.

**성공 기준:**
1. `anonpengling.org`에 다크 갤러리 톤의 스튜디오 홈이 떠 있다.
2. 새 앱을 **데이터 항목 하나**(content collection)만 추가하면 카드가 자동 생성된다.
3. 기존 Org-mode 개발 노트는 손실 없이 별도 레포로 이전되어 그대로 접근 가능하다.
4. EN/KO 이중 언어로 서비스된다.

**비목표 (YAGNI):**
- 블로그/저널, 통계 대시보드, 검색 — 지금은 만들지 않는다 (구조만 확장 가능하게).
- 스튜디오 전용 마스코트 일러스트 — 색(오로라 그라데이션)으로 대신하고 추후 검토.
- CMS, 로그인, 결제 — 정적 사이트 범위 밖.

## 2. 확정된 결정 (브레인스토밍 결과)

| 항목 | 결정 |
|------|------|
| 페이지 본질 | 앱 스튜디오 허브 (포트폴리오 대문) |
| 기존 노트 | **별도 레포로 이전**, 이 레포는 허브 전용으로 비움 |
| 기술 스택 | **Astro** (정적 빌드 → GitHub Pages) |
| 브랜드 | **anonpengling** 우산 브랜드 (`org.anonpengling.*` 번들, `*.anonpengling.org` 도메인) |
| 도메인 | **anonpengling.org 정점**(apex), GitHub Pages CNAME + Route53 |
| 언어 | **EN/KO 이중**, Astro i18n (`/` = EN, `/ko` = KO) |
| 비주얼 방향 | **다크 갤러리 / 프리미엄** (근검정 #0b0e13, 앱색이 카드에서 발광) |
| 스튜디오 액센트 | **오로라 그라데이션** (teal `#34d3ee` → indigo `#6366f1`), 펭귄 마스코트 없음 |
| 앱 카드 | **피처 타일** (스크린샷/폰 목업 + 텍스트 + 상태/플랫폼/링크) |
| 섹션 순서 | 내비 → 히어로 → Apps → How we work → Follow → 푸터 |

## 3. 아키텍처

### 3.1 전체 구성

두 단계로 나눈다. 같은 레포를 비우는 작업이 선행되어야 허브를 새로 올릴 수 있다.

- **Phase 0 — 정리 & 이전:** 기존 Org 노트를 새 레포로 옮기고, 이 레포를 빈 상태로 만든다.
- **Phase 1 — 스튜디오 허브:** Astro로 허브를 구축하고 anonpengling.org에 배포한다.

### 3.2 Phase 0 — 기존 콘텐츠 이전 & 레포 정리

**이전 대상 (개발 노트 자산):**
- `content/` (org 소스 전체: emacs, dotfiles, agents/claude-code, learning, neovim, examples 등)
- `build-site.el`, `build.sh`, `build.ps1`, `tangle*.sh`, `tangle-configs.el`, `split-config.py`, `retarget-skills.sh`
- `.github/workflows/publish.yml` (emacs 빌드 워크플로)
- `README.org` (노트 사이트 설명)

**이전 방식:** 새 레포(가칭 `mandoo180/notes`)를 만들고 위 파일을 옮긴다. git 히스토리 보존 여부와 최종 노트 URL(프로젝트 페이지 `mandoo180.github.io/notes` vs 서브도메인 `notes.anonpengling.org`)은 **구현 계획에서 확정**한다. 핵심 제약: 사용자 페이지(`mandoo180.github.io`)에 커스텀 도메인을 걸면 프로젝트 페이지도 `anonpengling.org/<repo>` 경로로 끌려오므로, 노트는 별도 도메인/서브도메인으로 분리하는 편이 깔끔하다.

**이 레포에서 제거/정리:**
- 위 이전 대상 파일·디렉터리 삭제
- `public/`, `.packages/`, `tmp/`, `.agent-shell/` (빌드 산출물·캐시)
- `*.bkp`, `*.backup`, `.DS_Store` 추적 정리 (`.DS_Store`는 gitignore 추가)
- `.mcp.json`, `CLAUDE.md` 는 허브용으로 갱신 (Org 빌드 설명 → Astro 빌드 설명)
- 스테이징된 `AGENTS.md` 삭제 확정

**산출물:** 노트가 분리된 새 레포 + 이 레포는 Astro 프로젝트만 남은 깨끗한 상태.

### 3.3 Phase 1 — Astro 스튜디오 허브

**디렉터리 구조:**

```
mandoo180.github.io/
├── astro.config.mjs          # site: https://anonpengling.org, i18n(en/ko), sitemap
├── package.json
├── tsconfig.json
├── public/
│   ├── CNAME                 # "anonpengling.org"
│   ├── favicon / og 이미지
│   └── apps/<slug>/...       # 앱 스크린샷·아이콘 자산
├── src/
│   ├── content/
│   │   ├── config.ts         # apps 컬렉션 zod 스키마
│   │   └── apps/
│   │       ├── fulang.md
│   │       └── storyfluent.md
│   ├── i18n/
│   │   └── ui.ts             # 사이트 chrome 문구 EN/KO 딕셔너리
│   ├── components/
│   │   ├── Nav.astro
│   │   ├── Hero.astro
│   │   ├── AppCard.astro     # 피처 타일
│   │   ├── HowWeWork.astro
│   │   ├── Follow.astro
│   │   ├── Footer.astro
│   │   └── LangToggle.astro
│   ├── layouts/
│   │   └── Base.astro        # <head> 메타·OG·테마 토큰, 공통 레이아웃
│   ├── styles/
│   │   └── tokens.css        # 색·간격·타이포 토큰 (다크 갤러리 + 오로라)
│   └── pages/
│       ├── index.astro       # EN 홈 (/)
│       └── ko/index.astro    # KO 홈 (/ko)
└── .github/workflows/deploy.yml   # Astro 빌드 → Pages 배포
```

**컴포넌트 경계 (단일 책임):**
- `AppCard.astro` — 앱 데이터 1건을 받아 피처 타일 렌더. 내부를 바꿔도 홈은 영향 없음. 입력: 앱 엔트리 + locale.
- `Nav` / `Hero` / `HowWeWork` / `Follow` / `Footer` — 각자 한 섹션. UI 문구는 `i18n/ui.ts`에서 locale로 받음.
- `Base.astro` — head·메타·OG·테마. 페이지는 콘텐츠만 신경 씀.
- 색·간격은 `tokens.css` CSS 변수로 단일화 → 디자인 변경 시 한 곳만 수정.

### 3.4 앱 content collection 스키마

새 앱 추가 = `src/content/apps/<slug>.md` 한 파일. 스키마(zod):

```ts
{
  name: string,              // "Fulang"
  slug: string,              // "fulang"
  status: 'live' | 'pre-launch' | 'waitlist' | 'wip',
  accent: string,            // "#C8102E"  카드 발광색
  order: number,             // 정렬 가중치
  featured: boolean,         // true=피처 타일, false=하단 컴팩트 리스트(확장 대비)
  platforms: ('ios'|'android'|'web')[],
  learn: { language: string, forSpeakers: string },  // "Taiwanese Mandarin" / "EN·KO"
  icon: string,              // /apps/fulang/icon.png
  screenshot: string,        // /apps/fulang/hero.png
  links: {                   // 있는 것만
    site?: string, appStore?: string, playStore?: string,
    youtube?: string, waitlist?: string
  },
  // 본문(마크다운) = locale별 tagline/description은 frontmatter에 i18n 객체로:
  tagline: { en: string, ko: string },
  description: { en: string, ko: string }
}
```

**초기 데이터:**
- **fulang** — status `pre-launch`, accent `#C8102E`, platforms `[ios, web]`, learn `대만 만다린 / EN·KO`, links: site `https://fulang.anonpengling.org`, youtube `@fulang.anonpengling`, waitlist(랜딩 폼).
- **storyfluent** — status `live`, accent `#d4af37`, platforms `[ios, android]`, learn `고전으로 영어 / KO`, links: site `https://storyfluent.anonpengling.org`, appStore, youtube `@storyfluent_kr`.

### 3.5 확장성 (앱이 늘어날 때)

피처 타일은 2~4개일 때 화려하지만 많아지면 길어진다. 대응:
- `featured: true` 앱만 상단 피처 타일로.
- `featured: false` 앱은 Apps 섹션 하단 **컴팩트 리스트 로우**로 (호버 시 발광). 컴포넌트만 분기, 데이터 모델 동일.

### 3.6 디자인 토큰

- **배경:** `#0b0e13` (페이지), `#12171f` (카드), 보더 `#232a34`
- **텍스트:** `#e8eaed` (주), `#9aa3af` (보조), `#5f6875` (희미)
- **스튜디오 액센트:** 그라데이션 `linear-gradient(90deg,#67e8f9,#818cf8)` (워드마크·CTA·링크), 단색 폴백 `#67e8f9`
- **앱색:** 카드별 `accent` 토큰으로 radial glow 배경 생성
- **타이포:** 시스템 폰트 우선(빠른 로드) + 디스플레이용 가변폰트 1종(구현 시 선정, 예: Geist/Inter). 한글 폴백 Pretendard/Noto Sans KR
- **모션:** 그라데이션·발광에 `prefers-reduced-motion` 존중

### 3.7 배포 & 인프라

- **Astro config:** `site: 'https://anonpengling.org'`, `@astrojs/sitemap`, i18n(`defaultLocale: 'en'`, `locales: ['en','ko']`, `routing: { prefixDefaultLocale: false }`).
- **GitHub Actions:** `deploy.yml` — Node 셋업 → `npm ci` → `astro build` → `actions/deploy-pages` (또는 기존처럼 `JamesIves/github-pages-deploy-action`로 `gh-pages` 브랜치). 기존 emacs 워크플로 대체.
- **커스텀 도메인:** `public/CNAME` = `anonpengling.org`. GitHub Pages 설정에서 도메인 지정 + HTTPS 강제.
- **DNS (Route53, 수동/외부 단계):** apex `anonpengling.org` A/AAAA → GitHub Pages IP(`185.199.108-111.153`), `www` CNAME → `mandoo180.github.io`. fulang/storyfluent 서브도메인 레코드는 건드리지 않음.

### 3.8 Follow(구독) 처리

정적 사이트라 이메일 수집엔 외부 엔드포인트가 필요하다. **MVP 결정:** 스튜디오 전용 리스트는 보류하고, "Follow the build"는 **YouTube 채널 링크 + 각 앱 waitlist 링크**로 연결한다. 추후 필요하면 Cloudflare Worker 또는 Formspree/Buttondown 같은 호스티드 폼으로 실제 이메일 캡처를 추가(컴포넌트만 교체). → 스펙 범위에선 폼 UI는 두되 동작은 외부 링크.

## 4. 데이터 흐름

1. 빌드 시 Astro가 `src/content/apps/*` 를 읽어 zod 검증.
2. `index.astro`(EN) / `ko/index.astro`(KO)가 앱 엔트리를 `order`로 정렬, `featured` 분기.
3. 각 엔트리 + 현재 locale을 `AppCard`에 전달 → 피처 타일/리스트 로우 렌더.
4. chrome 문구는 `i18n/ui.ts[locale]`에서 주입.
5. 정적 HTML 산출 → Pages 배포 → anonpengling.org.

## 5. 에러·엣지 케이스

- **자산 누락(스크린샷 없음):** AppCard는 스크린샷 없으면 accent 그라데이션 플레이스홀더로 폴백(레이아웃 안 깨짐).
- **링크 부분 존재:** `links`의 키만 버튼 렌더(예: storyfluent는 waitlist 없음, fulang은 appStore 없음).
- **locale 문구 누락:** ko 문구 비면 en으로 폴백.
- **스키마 위반:** zod가 빌드 실패시켜 잘못된 앱 데이터가 배포되지 않게 함.
- **reduced-motion:** 그라데이션 애니메이션 정지, 정적 색 표시.

## 6. 테스트 / 검증

- `astro build` 무오류 + `astro check`(타입) 통과.
- content collection zod 검증이 두 초기 앱에 대해 통과.
- 로컬 `astro preview`에서 EN(`/`)·KO(`/ko`) 양쪽 렌더, 언어 토글 동작.
- 반응형: 모바일(피처 타일 세로 스택)·데스크톱 확인.
- 새 앱 더미 항목 추가 시 코드 수정 없이 카드 생성됨을 확인(확장성 회귀).
- Lighthouse: 정적 사이트로 성능/접근성 양호 목표.

## 7. 구현 단계 개요 (계획에서 상세화)

0. **이전 & 정리:** 노트 → 새 레포, 이 레포 비우기, gitignore/CLAUDE.md 갱신.
1. **Astro 스캐폴드:** 프로젝트 초기화, 토큰·Base 레이아웃, i18n 설정.
2. **콘텐츠 모델:** apps 컬렉션 스키마 + fulang·storyfluent 엔트리·자산.
3. **컴포넌트:** Nav/Hero/AppCard/HowWeWork/Follow/Footer/LangToggle.
4. **페이지 조립:** EN·KO 홈, 섹션 순서대로.
5. **배포:** deploy.yml, CNAME, Pages 설정, (외부)Route53.
6. **검증:** 빌드·반응형·확장성·Lighthouse.

## 8. 미해결 / 구현 시 확정할 것

- 노트 레포의 최종 이름·URL·히스토리 보존 방식.
- 디스플레이 폰트 최종 선정.
- 스튜디오 구독 폼 실제 백엔드 도입 여부(현재는 링크로 대체).
- OG 이미지 디자인(정적 1장 vs 앱별).
