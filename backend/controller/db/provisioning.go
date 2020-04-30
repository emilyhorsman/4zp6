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

	selectAllProvisioning = `SELECT busAddr,name,definitionId,regIdLength,regId,regBlockLength,bytesPerReg,readPeriod
	FROM Provisioning
	LEFT JOIN Processor using(busAddr)
	LEFT JOIN ReadDefinition using(definitionId);`
)

// Provisioning represnts a joined entry from "Provisioning", "Processor", and
// "ReadDefinition" tables.
type Provisioning struct {
	BusAddr        int    `json:"busAddr"`        // BusAddr is the bus address of the peripehral
	Name           string `json:"name"`           // Name is common name of peripheral controller
	DefinitionID   int    `json:"definitionId"`   // DefinitionID is a definition UUID
	RegIDLength    int    `json:"regIdLength"`    // RegIDLength is register ID length
	RegID          int    `json:"regId"`          // RegID is register ID
	RegBlockLength int    `json:"regBlockLength"` // RegBlockLenght is register block length
	BytesPerReg    int    `json:"bytesPerReg"`    // BytesPerReg is bytes per register
	ReadPeriod     int    `json:"readPeriod"`     // ReadPeriod is the bus address reading period
}

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

// GetProvisioning returns a list of active provisionings. An erorr will be
// returned if the query cannot be completed.
func GetProvisioning(s *state.State) ([]Provisioning, error) {
	var out []Provisioning

	// query all provisioning definitions
	stmt, err := s.SQL.Prepare(selectAllProvisioning)
	if err != nil {
		return out, err
	}
	rows, err := stmt.Query()
	if err != nil {
		return out, err
	}
	defer rows.Close()

	// iterate over each row (read definition with name and bus address)
	for rows.Next() {
		// create new provisioning and read definition, scan results
		var prov Provisioning
		err = rows.Scan(&prov.BusAddr, &prov.Name, &prov.DefinitionID, &prov.RegIDLength,
			&prov.RegID, &prov.RegBlockLength, &prov.BytesPerReg, &prov.ReadPeriod)
		if err != nil {
			return out, err
		}
		// append to output
		out = append(out, prov)
	}
	return out, nil
}
