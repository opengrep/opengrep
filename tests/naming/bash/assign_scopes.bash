x=1
f() {
  local y=2
  y=3
  z=4
  x=5
  echo "$x $y $z"
}
f
echo "$z"
