package main

func app_callback_only(f func(string) string, x string) string {
	return f(x)
}
