// 'export type { X } from "mod"' desugars to imports, so the re-exported
// name is visible to import rules
// MATCH:
export type { Foo } from './types';
