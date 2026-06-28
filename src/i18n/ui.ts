import type { Locale } from '../lib/types';

export const languages: Record<Locale, string> = { en: 'English', ko: '한국어' };
export const defaultLocale: Locale = 'en';

export const ui = {
  en: {
    'meta.title': 'Pengling — daily language games',
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
    'meta.title': 'Pengling — 매일 한 판의 언어 학습 앱',
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
