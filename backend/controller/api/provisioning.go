package api

import (
	"encoding/json"
	"net/http"

	"github.com/emilyhorsman/4zp6/backend/controller/db"
	"github.com/emilyhorsman/4zp6/backend/controller/state"
)

// provisioningHandler is "/api/provisioning". It returns a list of provisioning
// configurations and read definitions.
func provisioningHandler(s *state.State, w http.ResponseWriter, r *http.Request) {
	// query all provisionings
	list, err := db.GetProvisioning(s)
	if err != nil {
		panic(err)
	}
	// convert data to JSON
	buff, err := json.Marshal(list)
	if err != nil {
		w.WriteHeader(http.StatusInternalServerError)
		w.Write([]byte("internal server error"))
		return
	}
	w.Header().Set("Content-Type", "application/json")
	w.Write(buff)
}
