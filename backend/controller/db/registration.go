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
)

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
	for rows.Next() {
		var uuid string
		rows.Scan(&uuid)
		out[uuid] = true
	}
	return out, nil
}
