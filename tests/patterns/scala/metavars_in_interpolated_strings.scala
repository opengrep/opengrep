
//ERROR: match
println(s"select $some_userdata")

//ERROR: match
println(s"select ${some_userdata}")

//ERROR: match
println(s"select ${some_userdata} from table")

//ERROR: match
println(s"select ${some_userdata} from $table")

//OK:
println(s"$select ${some_userdata} from $table")
