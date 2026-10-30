# A propagator into the parameter of a lambda taints the parameter in the
# lambda's body.
def main():
    x = source()
    # ruleid: python_propagator_into_lambda_parameter
    each(x, lambda y: sink(y))
    c = "safe"
    # ok: python_propagator_into_lambda_parameter
    other(c, lambda z: sink(z))
