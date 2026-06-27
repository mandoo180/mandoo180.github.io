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
