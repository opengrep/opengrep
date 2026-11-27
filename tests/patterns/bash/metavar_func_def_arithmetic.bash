# ERROR:
f() {
  (( n++ ))
  echo "$n"
}

(( n++ ))
