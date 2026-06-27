import { defineCollection, z } from 'astro:content';
import { glob } from 'astro/loaders';

const apps = defineCollection({
  loader: glob({ pattern: '**/*.md', base: './src/content/apps' }),
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
