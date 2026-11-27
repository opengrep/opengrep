namespace App;

use App\Util\Store;

function source(): int {
  return 1;
}

function main(): void {
  $store = new Store();
  $store->put(source());
  Store::relay(source());
}
