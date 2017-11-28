package main

import (
	"log"

	"github.com/labstack/echo"
	"github.com/labstack/echo/middleware"
	"github.com/moorara/toys/go/echo-mgo-vue/db"
	"github.com/moorara/toys/go/echo-mgo-vue/service"
	mgo "gopkg.in/mgo.v2"
)

const port = ":8000"

func main() {
	// DB connection
	mongoDB := db.NewMongoDB("localhost:27017", mgo.Monotonic, true)
	if err := mongoDB.Connect(); err != nil {
		panic(err)
	}
	defer mongoDB.Disconnect()

	// Instantiations
	e := echo.New()
	e.HideBanner = true
	taskService := service.NewTaskService(mongoDB.Session)

	// Middlewares
	e.Use(middleware.Logger())
	e.Use(middleware.Recover())

	// Routes
	e.File("/", "public/index.html")
	e.GET("/tasks", taskService.GetTasks)
	e.POST("/tasks", taskService.CreateTask)
	e.GET("/tasks/:id", taskService.GetTask)
	e.PUT("/tasks/:id", taskService.UpdateTask)
	e.DELETE("/tasks/:id", taskService.DeleteTask)

	// Start server
	log.Printf("Starting servcer on port %s\n", port)
	e.Logger.Fatal(e.Start(port))
}
