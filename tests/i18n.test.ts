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
