# Test: Python collection models for taint propagation

def test_list_append(tainted):
    lst = []
    lst.append(tainted)
    # ruleid: python-collection-taint
    sink(lst)

def test_list_pop(tainted):
    lst = [tainted]
    value = lst.pop()
    # ruleid: python-collection-taint
    sink(value)

def test_dict_get(tainted):
    d = {"key": tainted}
    value = d.get("key")
    # ruleid: python-collection-taint
    sink(value)

def test_set_add(tainted):
    s = set()
    s.add(tainted)
    # ruleid: python-collection-taint
    sink(s)

def test_list_extend(tainted):
    lst = []
    lst.extend([tainted])
    # ruleid: python-collection-taint
    sink(lst)

def sink(data):
    print(data)
