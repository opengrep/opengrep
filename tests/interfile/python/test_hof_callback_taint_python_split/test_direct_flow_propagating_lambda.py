from app_with_direct_flow import app_with_direct_flow

def test_direct_flow_propagating_lambda():
    # ruleid: test-hof-callback-taint
    return sink(app_with_direct_flow(lambda x: x, source()))

