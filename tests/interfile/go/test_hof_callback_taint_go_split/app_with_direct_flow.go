package main

func app_with_direct_flow(f func(string) string, x string) string {
	return f(x) + x
}
