try:
    from lxml import etree
except ImportError:
    class etree:
        pass

#ERROR:
etree.parse(x)
json.parse(x)
