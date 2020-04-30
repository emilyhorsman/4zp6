# Telemetry

## Features

* A common runtime for register-paradigm I2C devices. 
  Telemetry offers an Arduino-compatible library which generalizes application logic for peripherals (i.e., no more hardware libraries such as [this](https://github.com/adafruit/Adafruit_SHT31), [this](https://github.com/adafruit/Adafruit_AMG88xx), or [this](https://github.com/adafruit/Adafruit_LIS3DH)).
* Telemetry can run an access point on a microcontroller to write configuration to non-volatile memory.
  This configuration will then be used to connect to an existing WiFi network and a Telemetry server.
* Declarative configurations for new peripherals.
* Write application logic for peripheral data (e.g., interpreting temperature) with no dependencies, in any language, and without firmware updates on your fleet of microcontrollers.
* Plug-and-play data logging, consumption API, and visualization on a web dashboard.

## Building the project

* Firmware: Refer to [`prototypes/README.md`](./prototypes/)
* Sample Dashboard: Refer to [`dashboard/README.md`](./dashboard/)
