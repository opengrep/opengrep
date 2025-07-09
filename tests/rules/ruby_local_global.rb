def opts(data) = Array(data).join(" ") # process options

def test_calls4(user_input)
    opts = [user_input, "-c"] # local var
    # ruleid: test
    system("ls #{opts}")
end
