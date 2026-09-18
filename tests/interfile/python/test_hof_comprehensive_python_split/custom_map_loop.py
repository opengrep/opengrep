def custom_map_loop(arr, callback):
    result = []
    for item in arr:
        result.append(callback(item))
    return result

