def main():
    # Only label A flows here; the sink requires A and B, so NO finding.
    cb = lambda x: sink(source_a())
    cb(1)
