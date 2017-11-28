package main

import (
	"github.com/labstack/echo"
	"github.com/labstack/echo/middleware"
	"github.com/moorara/go-examples/echo-mgo-vue/db"
	"github.com/moorara/go-examples/echo-mgo-vue/service"
	"gopkg.in/mgo.v2"
)

func main() {
	// DB connection
	mongoDB := db.NewMongoDB("localhost:27017", mgo.Monotonic, true)
	if err := mongoDB.Connect(); err != nil {
		panic(err)
	}
	defer mongoDB.Disconnect()

	// Instantiations
	e := echo.New()
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
	e.Logger.Fatal(e.Start(":8000"))
}
