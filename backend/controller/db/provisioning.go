package db

import (
	"github.com/emilyhorsman/4zp6/backend/controller/state"
	telemetry "github.com/emilyhorsman/4zp6/protocol/go"
)

const (
	selectProvisioning = `SELECT busAddr,name,definitionId,regIdLength,regId,regBlockLength,bytesPerReg,readPeriod
	FROM Provisioning
	LEFT JOIN Processor using(busAddr)
	LEFT JOIN ReadDefinition using(definitionId)
	WHERE provisioning.busAddr = $1;`
)

// QueryProvisioning will query the "Processor", "ReadDefintion" and
// "Provisioning" tables using the provided bus address, returning a telemetry
// Protobuf instance and an indicator if a provisioning record was found. An
// error will be returned if the query cannot be completed.
func QueryProvisioning(s *state.State, busAddr uint32) (*telemetry.Provisioning, bool, error) {
	// query provisioning profiles for bus address
	stmt, err := s.SQL.Prepare(selectProvisioning)
	if err != nil {
		return nil, false, err
	}
	rows, err := stmt.Query(busAddr)
	if err != nil {
		return nil, false, err
	}
	defer rows.Close()

	var out telemetry.Provisioning
	var definitions []*telemetry.Provisioning_ReadDef

	// iterate over each row (read definition with name and bus address)
	for rows.Next() {
		// create new definition, scan results into definition
		var def telemetry.Provisioning_ReadDef
		err = rows.Scan(&out.BusAddr, &out.Name,
			&def.DefinitionId, &def.RegisterIdLength, &def.RegisterId,
			&def.RegisterBlockLength, &def.NumBytesPerRegister, &def.ReadPeriod)
		if err != nil {
			return nil, false, err
		}
		// append definition to list of definitions
		definitions = append(definitions, &def)
	}
	// attach list of read definitions to provisioning profile
	out.ReadDefinitions = definitions

	if out.Name == "" && len(out.ReadDefinitions) == 0 {
		// don't actually have any read defs but no error
		return &out, false, nil
	}

	// successful responds, definitions found
	return &out, true, nil
}
