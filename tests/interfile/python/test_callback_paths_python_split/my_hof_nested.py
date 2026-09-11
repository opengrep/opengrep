def my_hof_nested(boss, mode):
    if mode == "team_a":
        team = boss["team_a"]
    else:
        team = boss["team_b"]
    if mode == "leader":
        cb = team["leader"]
    else:
        cb = team["sub"]
    return cb(boss["data"])

