package svc

func Run() string {
	var s Service
	return Service.Read(s)
}
