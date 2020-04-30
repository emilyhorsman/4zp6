module github.com/emilyhorsman/4zp6/software-prototype

go 1.14

require (
	github.com/emilyhorsman/4zp6/backend/controller v0.0.0-00010101000000-000000000000
	github.com/emilyhorsman/4zp6/protocol/go v0.0.0-00010101000000-000000000000
	github.com/golang/protobuf v1.4.0 // indirect
	github.com/sirupsen/logrus v1.4.2
	golang.org/x/net v0.0.0-20200425230154-ff2c4b7c35a0 // indirect
	google.golang.org/protobuf v1.21.0
)

replace github.com/emilyhorsman/4zp6/backend/controller => ../backend/controller

replace github.com/emilyhorsman/4zp6/protocol/go => ../protocol/go
