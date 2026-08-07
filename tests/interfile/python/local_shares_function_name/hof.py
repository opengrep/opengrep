# The HOF that makes a callback candidate consequential: its signature
# carries the call-my-argument obligation replayed at each call site.
def run(cb, data):
    return cb(data)
