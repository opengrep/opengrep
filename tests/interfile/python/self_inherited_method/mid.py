from base import Base


# Intermediate class: forces a 3-hop MRO walk (Child -> Mid -> Base) to reach
# the inherited methods. Redefines nothing.
class Mid(Base):
    pass
