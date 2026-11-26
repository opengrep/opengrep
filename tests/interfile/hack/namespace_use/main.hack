namespace App;

use function App\Util\{quiet, relay};
use type App\Util\Box;

function source(): int {
  return 1;
}

function main(): void {
  relay(source());
  quiet(2);
  $box = new Box();
  $box->put(source());
  $box->keep(3);
}
