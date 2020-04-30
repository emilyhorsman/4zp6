package db

import (
	"github.com/emilyhorsman/4zp6/backend/controller/state"
	telemetry "github.com/emilyhorsman/4zp6/protocol/go"
)

const (
	upsertRegistration = `INSERT INTO Registration(uuid,firmware,ipv4,ipv6)
	VALUES ($1,$2,$3,$4)
	ON CONFLICT (uuid) DO UPDATE
	SET uuid=EXCLUDED.uuid,
		firmware=EXCLUDED.firmware,
		ipv4=EXCLUDED.ipv4,
		ipv6=EXCLUDED.ipv6;`

	deletePeripheral = `DELETE FROM Peripheral
	WHERE controller = $1;`

	insertPeripheral = `INSERT INTO Peripheral(controller,busId,busAddr,callResp)
	VALUES ($1,$2,$3,$4);`

	selectRegistration = `SELECT uuid from Registration;`

	selectAllRegistration = `SELECT uuid,firmware,ipv4,ipv6 from Registration;`

	selectFilterPeripheral = `SELECT busId,busAddr,callResp
	FROM Peripheral
	WHERE controller = $1;`
)

// RegistrationPeripherals is the registration information combined with
// connected Peripherals.
type RegistrationPeripherals struct {
	UUID        string       `json:"uuid"`        // UUID is the microcontroller unique identifier
	Firmware    int          `json:"firmware"`    // Firmware is the microcontroller firmware number
	IPv4        string       `json:"ipv4"`        // IPv4 is the IPv4 address
	IPv6        string       `json:"ipv6"`        // IPv6 is the IPv6 address
	Peripherals []Peripheral `json:"peripherals"` // Peripheral is the list of connected peripherals
}

// Peripheral is a peripheral connected to the microcontroller.
type Peripheral struct {
	BusID    int    `json:"busId"`    // BusID is the bus identifier of the peripheral
	BusAddr  int    `json:"busAddr"`  //BusAddr is the bus address of the peripehral
	CallResp []byte `json:"callResp"` // CallResp is the general call response
}

// UpsertRegistration will insert the registration telemetry message into the
// "registration" and "peripheral" tables.
func UpsertRegistration(s *state.State, msg *telemetry.Telemetry) error {
	// insert into registration
	r := msg.Registration
	stmt, err := s.SQL.Prepare(upsertRegistration)
	if err != nil {
		return err
	}
	_, err = stmt.Exec(r.Uuid, r.Version, r.Ipv4, r.Ipv6)
	if err != nil {
		return err
	}

	// delete existing peripehrals
	stmt, err = s.SQL.Prepare(deletePeripheral)
	if err != nil {
		return err
	}
	_, err = stmt.Exec(r.Uuid)
	if err != nil {
		return err
	}

	// add new peripherals
	stmt, err = s.SQL.Prepare(insertPeripheral)
	if err != nil {
		return err
	}
	for _, p := range r.Peripherals {
		_, err = stmt.Exec(r.Uuid, p.BusId, p.BusAddr, p.GeneralCallResp)
	}
	return nil
}

// GetRegistered returns the map of microcontrollers UUID that are in the
// registration table.
func GetRegistered(s *state.State) (map[string]bool, error) {
	out := make(map[string]bool)
	rows, err := s.SQL.Query(selectRegistration)
	if err != nil {
		return out, err
	}
	defer rows.Close()
	for rows.Next() {
		var uuid string
		rows.Scan(&uuid)
		out[uuid] = true
	}
	return out, nil
}

// GetRegistrationPeripherals will join the "Register" and "Peripheral" tables,
// returning the complete registration information. It will return an error if
// the
func GetRegistrationPeripherals(s *state.State) ([]RegistrationPeripherals, error) {
	var out []RegistrationPeripherals

	// query for all rows for Registration
	stmt, err := s.SQL.Prepare(selectAllRegistration)
	if err != nil {
		return out, err
	}
	microcontrollers, err := stmt.Query()
	if err != nil {
		return out, err
	}
	defer microcontrollers.Close()

	// iterate over microcontroller
	for microcontrollers.Next() {
		// scan data into database
		var reg RegistrationPeripherals
		err = microcontrollers.Scan(&reg.UUID, &reg.Firmware, &reg.IPv4, &reg.IPv6)

		// query peripherals for each microcontroller
		stmt, err := s.SQL.Prepare(selectFilterPeripheral)
		if err != nil {
			return out, err
		}
		peripherals, err := stmt.Query(reg.UUID)
		if err != nil {
			return out, err
		}
		defer peripherals.Close()

		// iterate over peripherals
		for peripherals.Next() {
			var per Peripheral
			err = peripherals.Scan(&per.BusID, &per.BusAddr, &per.CallResp)

			// append peripheral to microcontroller list
			reg.Peripherals = append(reg.Peripherals, per)
		}
		// append registration peripheral to output list
		out = append(out, reg)
	}

	return out, err
}
