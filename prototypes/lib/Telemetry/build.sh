#!/bin/sh

# Generate Arduino Protobuf files

# This process only has to be done when the "Telemetry.proto" file has changed.

# You will have to download + unzip "protoc" and "nanopb_generator" for your
# specific operating system (use latest version):
#   - protoc: https://github.com/protocolbuffers/protobuf/releases
#   - nanopb: https://jpa.kapsi.fi/nanopb/download/

# Once downloaded, open a terminal and run the commands (ensuring that you point
# to "protoc" and "nanopb_generator" files in the folder). It will most likely
# be located in a "bin" or "generator-bin" folder.

protoc -o Telemetry.pb Telemetry.proto
nanopb-0.3.9.3-macosx-x86/generator-bin/nanopb_generator Telemetry.pb
rm Telemetry.pb
