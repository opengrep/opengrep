from f_dead import f_dead
from f_dead_len import f_dead_len
from f_dead_str import f_dead_str
from f_escape_refutes import f_escape_refutes
from f_live import f_live
from g_escape_eval import g_escape_eval
from source import source
from unknown import unknown

def calls():
    f_dead(unknown(), source())
    f_dead_len(unknown(), source())
    f_live(unknown(), source())
    f_dead_str(unknown(), source())
    f_escape_refutes(unknown(), source())
    g_escape_eval("zzz", source())
