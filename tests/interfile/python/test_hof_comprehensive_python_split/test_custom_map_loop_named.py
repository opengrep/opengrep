from custom_map_loop import custom_map_loop
from process_custom_map_loop import process_custom_map_loop

def test_custom_map_loop_named():
    arr = [source()]
    custom_map_loop(arr, process_custom_map_loop)

