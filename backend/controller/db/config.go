package db

import (
	"github.com/emilyhorsman/4zp6/backend/controller/state"
)

const (
	upsertProcessor = `INSERT INTO Processor(busAddr,name)
	VALUES ($1,$2)
	ON CONFLICT (busAddr) DO UPDATE
	SET busAddr=EXCLUDED.busAddr,
		name=EXCLUDED.name;`

	upsertReadDefinition = `INSERT INTO ReadDefinition(definitionId,regIdLength,regId,regBlockLength,bytesPerReg,readPeriod)
	VALUES ($1,$2,$3,$4,$5,$6)
	ON CONFLICT (definitionId) DO UPDATE
	SET definitionId=EXCLUDED.definitionId,
		regIdLength=EXCLUDED.regIdLength,
		regId=EXCLUDED.regId,
		regBlockLength=EXCLUDED.regBlockLength,
		bytesPerReg=EXCLUDED.bytesPerReg,
		readPeriod=EXCLUDED.readPeriod;`

	upsertProvisioning = `INSERT INTO Provisioning(busAddr,definitionId)
	VALUES ($1,$2)
	ON CONFLICT (busAddr,definitionId) DO UPDATE
	SET busAddr=EXCLUDED.busAddr,
		definitionId=EXCLUDED.definitionId;`
)

// JSONConfig is the configuration from the peripheral controller.
type JSONConfig struct {
	BusAddr         int              `json:"busAddr"`
	Name            string           `json:"name"`
	ReadDefinitions []ReadDefinition `json:"readDefinitions"`
}

// ReadDefinition is a definition in the controller.
type ReadDefinition struct {
	DefinitionID        int `json:"definitionId"`
	RegisterIDLength    int `json:"registerIdLength"`
	RegisterID          int `json:"registerId"`
	RegisterBlockLength int `json:"registerBlockLength"`
	NumBytesPerRegister int `json:"numBytesPerRegister"`
	ReadPeriod          int `json:"readPeriod"`
}

// UpsertConfig accepts the JSON configuration from the peripheral controller.
// will update the "Processor", "ReadDefinition" and "Provisioning" tables with
// the provided data. It will return an error if the tables cannot be updated.
func UpsertConfig(s *state.State, config JSONConfig) error {
	// upsert processor information
	stmt, err := s.SQL.Prepare(upsertProcessor)
	if err != nil {
		return err
	}
	_, err = stmt.Exec(config.BusAddr, config.Name)
	if err != nil {
		return err
	}
	// upsert definitions
	for _, def := range config.ReadDefinitions {
		// upsert definition
		stmt, err = s.SQL.Prepare(upsertReadDefinition)
		if err != nil {
			return err
		}
		_, err = stmt.Exec(def.DefinitionID,
			def.RegisterIDLength,
			def.RegisterID,
			def.RegisterBlockLength,
			def.NumBytesPerRegister,
			def.ReadPeriod)
		if err != nil {
			return err
		}
		// upsert provisioning
		stmt, err = s.SQL.Prepare(upsertProvisioning)
		if err != nil {
			return err
		}
		_, err = stmt.Exec(config.BusAddr, def.DefinitionID)
		if err != nil {
			return err
		}
	}
	return nil
}
