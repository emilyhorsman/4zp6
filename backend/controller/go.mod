module github.com/emilyhorsman/4zp6/backend/controller

go 1.13

require (
	github.com/eclipse/paho.mqtt.golang v1.2.0
	github.com/emilyhorsman/4zp6/protocol/go v0.0.0-00010101000000-000000000000
	github.com/gorilla/mux v1.7.4
	github.com/joho/godotenv v1.3.0
	github.com/kr/pretty v0.2.0 // indirect
	github.com/lib/pq v1.3.0
	github.com/satori/go.uuid v1.2.0
	github.com/sirupsen/logrus v1.4.2
	github.com/streadway/amqp v0.0.0-20200108173154-1c71cc93ed71
	github.com/stretchr/testify v1.4.0 // indirect
	golang.org/x/net v0.0.0-20200202094626-16171245cfb2 // indirect
	golang.org/x/sys v0.0.0-20190502145724-3ef323f4f1fd // indirect
	google.golang.org/protobuf v1.21.0
	gopkg.in/check.v1 v1.0.0-20180628173108-788fd7840127 // indirect
)

replace github.com/emilyhorsman/4zp6/protocol/go => ../../protocol/go
