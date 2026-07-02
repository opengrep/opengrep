// Lambda VarDef as a top-level export.  This must show up in
// project_funcs_by_name AND in the per-file visibility set so that
// importers in app.ts can resolve `handler` by name.
export const handler = (input: string): string => {
  // ruleid: lambda-vardef-resolution
  return db.query(input);
};

declare const db: { query(q: string): string };
