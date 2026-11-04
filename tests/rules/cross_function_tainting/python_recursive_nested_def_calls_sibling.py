# A function with a nested function that calls itself and a sibling nested
# function, both reading a variable of the enclosing function.


def read_all(request, state):
    path = request.args["p"]
    missing = []
    # ruleid: test-recursive-nested-def
    with sink(path) as file:

        def load(target):
            file.seek(1)
            return file.read(target)

        def walk(target, source, key_path):
            if isinstance(source, int):
                return load(target)
            elif isinstance(source, dict):
                for key in list(target.keys()):
                    current = f"{key_path}.{key}" if key_path else key
                    if key in source:
                        target[key] = walk(target[key], source[key], current)
                    else:
                        missing.append(current)
                return target
            elif isinstance(source, list):
                result = []
                for i, (a, b) in enumerate(zip(target, source)):
                    current = f"{key_path}[{i}]" if key_path else f"[{i}]"
                    result.append(walk(a, b, current))
                return result
            return source

        out = walk(state, {}, "")
    # ok: test-recursive-nested-def
    sink(missing)
    return out
