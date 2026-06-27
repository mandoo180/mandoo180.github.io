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
