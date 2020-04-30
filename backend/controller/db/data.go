package db

import (
	"database/sql"
	"encoding/json"
	"time"

	"github.com/emilyhorsman/4zp6/backend/controller/state"
	u "github.com/satori/go.uuid"
)

const (
	insertData = `INSERT INTO Data(pollUuid,uuid,busAddr,time,data)
	VALUES ($1,$2,$3,$4,$5);`

	selectAllData = `SELECT uuid,busAddr,time,data
	FROM data
	WHERE uuid=$1 AND busAddr=$2;`

	selectStartData = `SELECT uuid,busAddr,time,data
	FROM data
	WHERE uuid=$1 AND busAddr=$2 AND time>=$3;`

	selectStopData = `SELECT uuid,busAddr,time,data
	FROM data
	WHERE uuid=$1 AND busAddr=$2 AND time<=$3;`

	selectStartStopData = `SELECT uuid,busAddr,time,data
	FROM data
	WHERE uuid=$1 AND busAddr=$2 AND time between $3 and $4;`
)

// IndexData is called when the peripheral controller publishes processed data
// (payload). It will attempt to index the data in the "Data" table.
func IndexData(s *state.State, uuid string, busAddr int, ts time.Time, payload []byte) error {
	// generate poll UUID
	pollUUID := u.NewV4()

	// insert data
	stmt, err := s.SQL.Prepare(insertData)
	if err != nil {
		return err
	}
	_, err = stmt.Exec(pollUUID.String(), uuid, busAddr, ts.Format(time.RFC3339), payload)
	if err != nil {
		return err
	}
	return nil
}

// QueryData accepts a microcontroller UUID, bus address, and option start and
// stop times. It will return all data points matching the parameters. An error
// will be returned if the query cannot be completed.
func QueryData(s *state.State, uuid string, busAddr int, start *time.Time, stop *time.Time) ([]state.WebsocketFrame, error) {
	var out []state.WebsocketFrame

	// generic rows (populated selectively below)
	var rows *sql.Rows

	if start != nil && stop != nil {
		// query between start and stop times
		stmt, err := s.SQL.Prepare(selectStartStopData)
		if err != nil {
			return out, err
		}
		// query with all parameters
		r, err := stmt.Query(uuid, busAddr, start, stop)
		if err != nil {
			return out, err
		}
		rows = r
	} else if start != nil && stop == nil {
		// query data after start time
		stmt, err := s.SQL.Prepare(selectStartData)
		if err != nil {
			return out, err
		}
		// query with all but stop
		r, err := stmt.Query(uuid, busAddr, start)
		if err != nil {
			return out, err
		}
		rows = r
	} else if start == nil && stop != nil {
		// query data before stop time
		stmt, err := s.SQL.Prepare(selectStopData)
		if err != nil {
			return out, err
		}
		// query with all but start
		r, err := stmt.Query(uuid, busAddr, stop)
		if err != nil {
			return out, err
		}
		rows = r
	} else {
		// query all data
		stmt, err := s.SQL.Prepare(selectAllData)
		if err != nil {
			return out, err
		}
		// query ignoring time
		r, err := stmt.Query(uuid, busAddr)
		if err != nil {
			return out, err
		}
		rows = r
	}
	for rows.Next() {
		// create measurement, scan data into measurement
		var m state.WebsocketFrame
		var tempData []byte
		err := rows.Scan(&m.UUID, &m.BusAddr, &m.Timestamp, &tempData)
		if err != nil {
			return out, err
		}
		// convert the payload in temp data to generic interface
		var buff interface{}
		err = json.Unmarshal(tempData, &buff)
		if err != nil {
			s.Log.Errorln(err)
		}
		// attach generic interface to measurement
		m.Data = buff
		// append to output
		out = append(out, m)
	}
	return out, nil
}
