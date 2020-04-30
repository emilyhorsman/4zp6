import React, { useRef, useState, useEffect } from 'react';
import './App.css';
import throttle from './Throttle';
import {VictoryAxis, VictoryLine, VictoryChart} from 'victory';

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
            <div style={{maxWidth: '50%', margin: '0 auto'}}>
                <VictoryChart>
                    <VictoryLine
                        data={series}
                        scale={{x: "time", y: "linear"}}
                        label="Temperature (°C)"
                    />
                    <VictoryAxis
                        tickValues={[data[0].timestamp, data[data.length - 1].timestamp]}
                        tickFormat={['Start', 'End']}
                        label="Time"
                    />
                    <VictoryAxis dependentAxis />
                </VictoryChart>
            </div>
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
                    [data.uuid]: prev.concat([{data: data.data, timestamp: Date.parse(data.timestamp)}])
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
