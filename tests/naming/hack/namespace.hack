<?hh
namespace App;

function helper(): int { return 1; }
function go(): int { return helper(); }

namespace Other;

function helper(): int { return 2; }
function go2(): int { return helper(); }
