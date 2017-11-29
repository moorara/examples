# Go Toys
This repo includes code examples written in [Go](https://golang.org).

## echo-mgo-vue
A simple single-page application (SPA) using
[Echo](https://echo.labstack.com), [mgo](https://labix.org/mgo), and [Vue](https://vuejs.org).

```
docker-compose up -d
go run main.go
```

## gorilla-websocket-vue
A simple single-page real-time chat application using
[Gorilla WebSocket](http://www.gorillatoolkit.org/pkg/websocket).
<sup>[tutorial](https://scotch.io/bar-talk/build-a-realtime-chat-server-with-go-and-websockets)</sup>

```
go run main.go
```

## rabbitmq-postgres
A simple distributed monitoring application using
[RabbitMQ](http://www.rabbitmq.com) and [PostgreSQL](https://www.postgresql.org).
<sup>[course](https://app.pluralsight.com/library/courses/go-build-distributed-applications)</sup>

```
make up
```

You can access RabbitMQ management console at [http://localhost:8080](http://localhost:8080).
Username and password are both `guest`.

You can also see the basic monitoring web application at [http://localhost:3000](http://localhost:3000).
