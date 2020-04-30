import React, { useState, useEffect } from 'react';
import './App.css';
import throttle from './Throttle';

function App() {
    const [status, setStatus] = useState({});

    useEffect(() => {
        const socket = new WebSocket("wss://telemetry.0xt.ca/ws");
        socket.onmessage = throttle(
            function(event) {
                const data = JSON.parse(event.data);
                setStatus({
                    ...status,
                    [data.uuid]: {data: data.data, timestamp: data.timestamp}
                });
            },
            500
        );
    });

    return (
        <div className="App">
            {Object.keys(status).map(uuid => {
                const {temp, humidity, denominator} = status[uuid].data;
                return (
                    <p>
                        {uuid}:
                        {' '}
                        {temp / denominator}°C,
                        {' '}
                        {humidity / denominator}% Humidity
                    </p>
                );
            })}
        </div>
    );
}

export default App;
