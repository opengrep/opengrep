import { getData as fetchData } from './lib';

export function run(): void {
  fetchData(source());
}
