all: go cpp

go:
	protoc --go_out=go/ Telemetry.proto

cpp:
	protoc -o Telemetry.pb Telemetry.proto
	nanopb-0.3.9.5-macosx-x86/generator-bin/nanopb_generator --output-dir=cc/ Telemetry.pb
	rm Telemetry.pb
