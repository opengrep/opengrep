from mid_b import B
from mid_c import C

class D(B, C):
    pass                  # C3 MRO: [D, B, C, A] -> D().m resolves to C.m
