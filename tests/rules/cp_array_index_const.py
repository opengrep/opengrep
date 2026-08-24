AUTH_HEADER = "authorization"
CONTENT_TYPE = "content-type"


def set_header(headers):
    # ruleid: cp-array-index-const-write
    headers[AUTH_HEADER] = "token"


def get_header(headers):
    # ruleid: cp-array-index-const-read
    return headers[CONTENT_TYPE]
