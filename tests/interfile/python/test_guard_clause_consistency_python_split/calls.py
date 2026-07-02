def calls():
    f_dead(unknown(), source())
    f_dead_len(unknown(), source())
    f_live(unknown(), source())
    f_dead_str(unknown(), source())
    f_escape_refutes(unknown(), source())
    g_escape_eval("zzz", source())
