# Telemetry Backend

## Environment Configuration
To deploy the backend controller and all necessary services, both [Docker](https://docs.docker.com/get-docker/) and [Docker Compose](https://docs.docker.com/compose/install/) are required.
Before deploying the stack, the Docker daemon must be running.

## Deployment
Once the environment is running, from this `backend` directory, please run the following:
```sh
docker-compose up -d --build
```
This command will download all required Docker images for Telemetry to function.
The following services will be downloaded, configured, and launched:
* RabbitMQ: AMQP broker
* VerneMQ: MQTT broker
* Postgres: Relational database
* Caddy: Webserver proxy

The command will also trigger the build process for Telemetry.
When the build process is complete, Telemetry will also configure itself and launch.

## Configuration
Inside the `config/` directory there is a `secret.env` file.
Please replace `username` and `password` with secure credentials for the services to use.

## Monitoring
The status of running Docker containers may be checked by running the following:
```sh
docker ps -a
```
Additionally, logs regarding a particular service may be viewed by running the following:
```sh
docker logs containerName
```
The name of the container can be determined by running the status command above.

## Cleanup
To stop and delete all containers, please run the following:
```sh
docker-compose down
```
This will preserve all persistent volumes created (Caddy and Postgres).
To remove persistent volumes, please reference Docker documentation.
