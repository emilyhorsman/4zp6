package api

import (
	"context"
	"fmt"
	"net/http"
	_ "net/http/pprof" // performance profiling
	"time"

	"github.com/emilyhorsman/4zp6/backend/controller/ctxlog"
	"github.com/emilyhorsman/4zp6/backend/controller/state"
	"github.com/gorilla/mux"
	u "github.com/satori/go.uuid"
	log "github.com/sirupsen/logrus"
)

// Start will start the HTTP API server.
func Start(s *state.State) error {
	// create main request router
	router := mux.NewRouter()
	api := router.PathPrefix("/api").Subrouter()
	router.StrictSlash(true)

	// UUID generator
	uuid := u.NewV4()

	// register performance profiling
	router.PathPrefix("/debug/pprof/").Handler(http.DefaultServeMux)

	// register 404 handler
	router.NotFoundHandler = http.HandlerFunc(
		func(w http.ResponseWriter, r *http.Request) {
			notFoundHandler(s, w, r)
		})

	// log all working requests on debug level
	router.Use(func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// provide context to every request
			ctx := context.Background()

			// update context with fields
			fields := s.Log.WithFields(log.Fields{
				"request": uuid.String(),
				"addr":    r.RemoteAddr,
				"method":  r.Method,
				"uri":     r.Host + r.RequestURI,
			})
			ctx = ctxlog.WithFields(ctx, fields)

			// inject context into request
			next.ServeHTTP(w, r.WithContext(ctx))
		})
	})

	// register all routes
	registerRoutes(s, api)

	server := &http.Server{
		Addr:         ":6060",
		Handler:      router,
		ReadTimeout:  5 * time.Second,
		WriteTimeout: 10 * time.Second,
		IdleTimeout:  120 * time.Second,
	}

	go func() {
		for {
			data := <-s.Data
			s.Log.Println("{api}", string(data))
		}
	}()

	s.Log.Info("[api] controller now listening on :6060")
	return server.ListenAndServe()
}

// registerRoutes registers all of the HTTP routes.
func registerRoutes(s *state.State, r *mux.Router) {
	r.HandleFunc("/", indexHandler)
	r.HandleFunc("/microcontrollers", controllerHandler)
	r.HandleFunc("/provisioning", provisioningHandler)
	r.HandleFunc("/data", dataHandler)
}

// notFoundHandler returns the HTTP 404 response.
func notFoundHandler(s *state.State, w http.ResponseWriter, r *http.Request) {
	// log and return 404 error
	s.Log.WithFields(log.Fields{
		"addr":   r.RemoteAddr,
		"method": r.Method,
		"uri":    r.Host + r.RequestURI,
	}).Warn("[request] http 404 not found")
	w.WriteHeader(http.StatusNotFound)
	fmt.Fprintf(w, "404 Not Found\n")
}
