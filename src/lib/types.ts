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
