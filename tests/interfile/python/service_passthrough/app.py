from service import CreateService

def vulnerable():
    tainted = source()
    result = CreateService("user", tainted).execute("group")
    # ruleid: test-service-passthrough
    sink(result)
