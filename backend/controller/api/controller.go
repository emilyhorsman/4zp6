package api

import (
	"encoding/json"
	"net/http"

	"github.com/emilyhorsman/4zp6/backend/controller/db"
	"github.com/emilyhorsman/4zp6/backend/controller/state"
)

// controllerHandler is "/api/microcontroller". It returns to the browser a list
// of registered microcontrollers and their connected peripherals.
func controllerHandler(s *state.State, w http.ResponseWriter, r *http.Request) {
	// query all microcontrollers and connected peripherals
	list, err := db.GetRegistrationPeripherals(s)
	if err != nil {
		s.Log.Errorln(err)
		w.WriteHeader(http.StatusInternalServerError)
		w.Write([]byte("internal server error"))
		return
	}
	// convert data to JSON
	buff, err := json.Marshal(list)
	if err != nil {
		s.Log.Errorln(err)
		w.WriteHeader(http.StatusInternalServerError)
		w.Write([]byte("internal server error"))
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.Write(buff)
}
