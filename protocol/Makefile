all: go cpp

go:
	protoc --go_out=go/ telemetry.proto

cpp:
	protoc -o telemetry.pb telemetry.proto
	../binary/nanopb-0.3.9.5-macosx-x86/generator-bin/nanopb_generator --output-dir=cc/ telemetry.pb
	rm telemetry.pb

.PHONY: go cpp
