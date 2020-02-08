module github.com/emilyhorsman/4zp6/mock

go 1.13

require (
	github.com/emilyhorsman/4zp6/backend/controller v0.0.0
	github.com/emilyhorsman/4zp6/protocol/go v0.0.0
	github.com/go-kit/kit v0.9.0
	github.com/go-logfmt/logfmt v0.5.0 // indirect
	github.com/go-stack/stack v1.8.0 // indirect
	github.com/gogo/protobuf v1.3.1
	github.com/golang/protobuf v1.3.3 // indirect
	github.com/joho/godotenv v1.3.0
	github.com/sirupsen/logrus v1.4.2
)

replace github.com/emilyhorsman/4zp6/backend/controller v0.0.0 => ../backend/controller

replace github.com/emilyhorsman/4zp6/protocol/go v0.0.0 => ../protocol/go
