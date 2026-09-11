from custom_map_builtin import custom_map_builtin
from process_custom_map_builtin import process_custom_map_builtin

def test_custom_map_builtin_named():
    arr = [source()]
    custom_map_builtin(arr, process_custom_map_builtin)

