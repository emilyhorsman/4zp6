package db

import "github.com/emilyhorsman/4zp6/backend/controller/state"

const (
	registrationSchema = `CREATE TABLE IF NOT EXISTS Registration(
		uuid 		text primary key 	not null,
		firmware 	int 				not null,
		ipv4 		text 				not null,
		ipv6 		text 				not null
	);`

	peripheralSchema = `CREATE TABLE IF NOT EXISTS Peripheral(
		controller 	text 	not null REFERENCES Registration(uuid) ON DELETE CASCADE,
		busID 		int 	not null,
		busAddr 	int 	not null,
		callResp 	bytea,
		PRIMARY KEY(controller, busID, busAddr)
	);`

	processorSchema = `CREATE TABLE IF NOT EXISTS Processor(
		busAddr int primary key not null,
		name 	text 			not null
	);`

	readDefinitionSchema = `CREATE TABLE IF NOT EXISTS ReadDefinition(
		definitionId 	int primary key not null,
		regIdLength 	int 			not null,
		regId 			int 			not null,
		regBlockLength 	int 			not null,
		bytesPerReg 	int 			not null,
		readPeriod 		int 			not null
	);`

	provisioningSchema = `CREATE TABLE IF NOT EXISTS Provisioning(
		busAddr 		int not null REFERENCES Processor(busAddr) ON DELETE CASCADE,
		definitionId 	int not null REFERENCES ReadDefinition(definitionId) ON DELETE CASCADE,
		PRIMARY KEY(busAddr, definitionId)
	);`

	dataSchema = `CREATE TABLE IF NOT EXISTS Data(
		pollUuid 	text 		not null,
		uuid 		text 		not null REFERENCES Registration(uuid) ON DELETE CASCADE,
		busAddr 	int 		not null REFERENCES Processor(busAddr) ON DELETE CASCADE,
		time 		timestamptz not null,
		data 		jsonb 		not null,
		PRIMARY KEY(pollUuid)
	);`
)

// Init initializes the Postgres database. If the tables do not already exist,
// the tables will be created. An error will be returned if the database cannot
// be queried or if the tables cannot be created.
func Init(s *state.State) error {
	// create Registration relation if not already exists
	_, err := s.SQL.Query(registrationSchema)
	if err != nil {
		return err
	}
	// create Peripheral relation if not already exists
	_, err = s.SQL.Query(peripheralSchema)
	if err != nil {
		return err
	}
	// create Processor relation if not already exists
	_, err = s.SQL.Query(processorSchema)
	if err != nil {
		return err
	}
	// create ReadDefinition relation if not already exists
	_, err = s.SQL.Query(readDefinitionSchema)
	if err != nil {
		return err
	}
	// create Provisioning relation if not already exists
	_, err = s.SQL.Query(provisioningSchema)
	if err != nil {
		return err
	}
	// create Data relation if not already exists
	_, err = s.SQL.Query(dataSchema)
	if err != nil {
		return err
	}
	return nil
}
