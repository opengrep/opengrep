import { sendClean, sendDirty } from './senders';

export function main(): void {
  sendClean(source());
  sendDirty(source());
}
