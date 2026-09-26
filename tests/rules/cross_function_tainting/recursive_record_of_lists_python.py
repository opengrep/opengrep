import os
import requests


def f(element):
    ret = {}
    if element.text:
        ret["text"] = element.text
    if element.tail:
        ret["tail"] = element.tail
    for child in element:
        ret[child.tag] = [ret[child.tag]]
        ret[child.tag].append(f(child))
    return ret


def main():
    # ruleid: recursive_record_of_lists_python
    requests.get(f(os.environ.get("X")))
