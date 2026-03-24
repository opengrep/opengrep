import helpers


def sink(value):
    print(value)


def main():
    payload = helpers.build_payload()
    sink(payload)


if __name__ == "__main__":
    main()
