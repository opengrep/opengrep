namespace App;

use function App\Util\relay, const App\Util\quiet;

function source(): int {
  return 1;
}

function main(): void {
  relay(source());
  quiet(source());
}
