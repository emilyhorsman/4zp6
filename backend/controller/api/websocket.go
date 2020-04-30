package api

import (
	"net/http"

	"github.com/emilyhorsman/4zp6/backend/controller/state"
	"github.com/gorilla/websocket"
)

var (
	// websocket client tracking
	clients = make(map[*websocket.Conn]bool)

	// upgrade HTTP reqeusts to wbesockets
	upgrader = websocket.Upgrader{
		CheckOrigin: func(r *http.Request) bool {
			return true
		},
	}
)

// websocketHandler is "/api/ws". It is responsible for upgrading HTTP
// connections to websockets. It registers websocket clients into the client
// global variable map.
func websocketHandler(s *state.State, w http.ResponseWriter, r *http.Request) {
	ws, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		s.Log.Errorln(err)
		w.WriteHeader(http.StatusInternalServerError)
		w.Write([]byte("failed to establish websocket connection"))
		return
	}
	// register client
	clients[ws] = true
}

// websocketEventLoop is responsible for emitting data events to all connected
// websock subscribers. If a client is no longer reachable, it will remove the
// client from the list.
func websocketEventLoop(s *state.State) {
	for {
		data := <-s.Data
		for client := range clients {
			// send data in separate go routine
			go func(s *state.State, clients map[*websocket.Conn]bool, client *websocket.Conn, data []byte) {
				err := client.WriteMessage(websocket.TextMessage, data)
				if err != nil {
					s.Log.Warnln("[websocket] error sending message to client", err)
					delete(clients, client)
				}
			}(s, clients, client, data)
		}
	}
}
