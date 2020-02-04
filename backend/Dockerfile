# Alpine linux with timezone and CA data
FROM golang:alpine as goalpine

RUN adduser -D -g '' gopher

# certificates + timezone data
RUN apk update
RUN apk --no-cache add ca-certificates tzdata git


# build image 
FROM goalpine as build

COPY ./.git /git
WORKDIR /git

COPY backend/controller /4zp6
WORKDIR /4zp6
RUN CGO_ENABLED=0 GOOS=linux GOARCH=amd64 go build -a -mod=vendor -installsuffix cgo -ldflags="-w -s" -o /4zp6/entrypoint main.go


# final image
FROM alpine

RUN adduser -D -g '' gopher
RUN mkdir /storage
RUN chown gopher:gopher /storage
COPY --from=build /etc/ssl/certs/ca-certificates.crt /etc/ssl/certs/
COPY --from=build /usr/share/zoneinfo /usr/share/zoneinfo
COPY --from=build /etc/passwd /etc/passwd
COPY --from=build /4zp6 /
USER gopher

ENTRYPOINT ["/entrypoint"]
