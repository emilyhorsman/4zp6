package api

import (
	"encoding/json"
	"net/http"
	"strconv"
	"time"

	"github.com/emilyhorsman/4zp6/backend/controller/db"
	"github.com/emilyhorsman/4zp6/backend/controller/state"
)

// dataHandler is "/api/data". It is responsible for querying data. It accepts a
// mandatory "uuid" and "busAddr" parameter. It will query the provided UUID and
// bus address for data. An optional "start" and "stop" time can be provided for
// time filtering. The time parameters must be specified in RFC3339. By default
// "start" is the earliest data entry and "stop" is the time the request was
// performed.
func dataHandler(s *state.State, w http.ResponseWriter, r *http.Request) {
	params := r.URL.Query()

	// get required parameters
	uuid := params.Get("uuid")
	busStr := params.Get("busAddr")
	if uuid == "" || busStr == "" {
		w.WriteHeader(http.StatusBadRequest)
		w.Write([]byte("Bad request: request must include url query parameters 'uuid' and 'busAddr'"))
		return
	}

	// convert bus address to integer
	busAddr, err := strconv.Atoi(busStr)
	if err != nil {
		w.WriteHeader(http.StatusBadRequest)
		w.Write([]byte("Bad request: bus address must be integer"))
		return
	}

	// default null pointers, populated if time exists
	var startTime, stopTime *time.Time

	// get optional parameters
	start := params.Get("start")
	if start != "" {
		t, err := time.Parse(time.RFC3339, start)
		if err != nil {
			w.WriteHeader(http.StatusBadRequest)
			w.Write([]byte("Bad request: start time provided must be in RFC3339 format"))
			return
		}
		startTime = &t
	}
	stop := params.Get("stop")
	if stop != "" {
		t, err := time.Parse(time.RFC3339, stop)
		if err != nil {
			w.WriteHeader(http.StatusBadRequest)
			w.Write([]byte("Bad request: stop time provided must be in RFC3339 format"))
			return
		}
		stopTime = &t
	}

	// query data in database
	data, err := db.QueryData(s, uuid, busAddr, startTime, stopTime)
	if err != nil {
		s.Log.Errorln(err)
		w.WriteHeader(http.StatusInternalServerError)
		w.Write([]byte("internal server error"))
		return
	}
	// convert data to JSON
	buff, err := json.Marshal(data)
	if err != nil {
		s.Log.Errorln(err)
		w.WriteHeader(http.StatusInternalServerError)
		w.Write([]byte("internal server error"))
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.Write(buff)
}
