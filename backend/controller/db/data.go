package db

import (
	"time"

	"github.com/emilyhorsman/4zp6/backend/controller/state"
)

const (
	insertData = `INSERT INTO Data(uuid,busAddr,time,data)
	VALUES ($1,$2,$3,$4);`
)

// IndexData is called when the peripheral controller publishes processed data
// (payload). It will attempt to index the data in the "Data" table.
func IndexData(s *state.State, uuid string, busAddr int, ts time.Time, payload []byte) error {
	// insert data
	stmt, err := s.SQL.Prepare(insertData)
	if err != nil {
		return err
	}
	_, err = stmt.Exec(uuid, busAddr, ts.Format(time.RFC3339), payload)
	if err != nil {
		return err
	}
	return nil
}
