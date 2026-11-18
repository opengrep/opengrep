import { render } from '@testing-library/react';

const customRender = (ui: string) => {
  // ruleid: export-alias-of-imported-name
  sink(ui);
  return {
    ...render(ui),
  };
};

export function describe(text: string): void {
  // ok: export-alias-of-imported-name
  sink(text);
}

export * from '@testing-library/react';
export { customRender as render };
