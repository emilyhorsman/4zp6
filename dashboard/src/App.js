import React, { useRef, useState, useEffect } from 'react';
import './App.css';
import throttle from './Throttle';
import {VictoryLine, VictoryChart} from 'victory';

function Peripheral({uuid, data}) {
    const {temp, humidity, denominator} = data[data.length - 1].data;
    const series = data.map(({timestamp, data}) => ({x: timestamp, y: data.temp / data.denominator}));
    return (
        <>
            <p>
                {uuid}:
                {' '}
                {temp / denominator}°C,
                {' '}
                {humidity / denominator}% Humidity
            </p>
            <VictoryChart>
                <VictoryLine data={series} />
            </VictoryChart>
        </>
    );
}

function App() {
    const [status, setStatus] = useState({});
    const socket = useRef(null);

    useEffect(() => {
        socket.current = new WebSocket("wss://telemetry.0xt.ca/ws");
        socket.current.onmessage = throttle(
            function(event) {
                const data = JSON.parse(event.data);
                const prev = status[data.uuid] || [];
                setStatus({
                    ...status,
                    [data.uuid]: prev.concat([{data: data.data, timestamp: data.timestamp}])
                });
            },
            500
        );

        return () => {
            socket.current.close();
        }
    });

    return (
        <div className="App">
            {Object.keys(status).map(uuid => <Peripheral key={uuid} uuid={uuid} data={status[uuid]} />)}
        </div>
    );
}

export default App;
