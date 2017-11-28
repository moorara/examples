package service

import (
	"errors"
	"net/http"
	"time"

	"github.com/labstack/echo"
	"github.com/moorara/toys/go/echo-mgo-vue/model"
	"gopkg.in/mgo.v2"
	"gopkg.in/mgo.v2/bson"
)

const (
	DATABASE   = "todo"
	COLLECTION = "tasks"
)

type Res map[string]interface{}

type TaskService struct {
	Session *mgo.Session
}

func NewTaskService(session *mgo.Session) *TaskService {
	return &TaskService{
		Session: session,
	}
}

func ToObjectId(id string) (bson.ObjectId, error) {
	if bson.IsObjectIdHex(id) == false {
		return "", errors.New("invalid id")
	}
	return bson.ObjectIdHex(id), nil
}

func (service *TaskService) GetTasks(c echo.Context) error {
	var tasks []model.Task
	err := service.Session.DB(DATABASE).C(COLLECTION).Find(bson.M{}).All(&tasks)
	if err != nil {
		return err
	}

	return c.JSON(http.StatusOK, tasks)
}

func (service *TaskService) CreateTask(c echo.Context) error {
	var body model.Task
	err := c.Bind(&body)
	if err != nil {
		return nil
	}

	task := model.NewTask(body.Name)
	err = service.Session.DB(DATABASE).C(COLLECTION).Insert(task)
	if err != nil {
		return err
	}

	return c.JSON(http.StatusCreated, task)
}

func (service *TaskService) GetTask(c echo.Context) error {
	id, err := ToObjectId(c.Param("id"))
	if err != nil {
		return c.JSON(http.StatusBadRequest, Res{"message": err.Error()})
	}

	var task model.Task
	err = service.Session.DB(DATABASE).C(COLLECTION).FindId(id).One(&task)
	if err != nil {
		if err == mgo.ErrNotFound {
			return c.NoContent(http.StatusNotFound)
		}
		return err
	}

	return c.JSON(http.StatusOK, task)
}

func (service *TaskService) UpdateTask(c echo.Context) error {
	var body model.Task
	err := c.Bind(&body)
	if err != nil {
		return nil
	}

	id, err := ToObjectId(c.Param("id"))
	if err != nil {
		return c.JSON(http.StatusBadRequest, Res{"message": err.Error()})
	}

	var task model.Task
	err = service.Session.DB(DATABASE).C(COLLECTION).FindId(id).One(&task)
	if err != nil {
		if err == mgo.ErrNotFound {
			return c.NoContent(http.StatusNotFound)
		}
		return err
	}

	task.Name = body.Name
	task.LastUpdated = time.Now()

	_, err = service.Session.DB(DATABASE).C(COLLECTION).UpsertId(id, &task)
	if err != nil {
		return err
	}

	return c.JSON(http.StatusCreated, task)
}

func (service *TaskService) DeleteTask(c echo.Context) error {
	id, err := ToObjectId(c.Param("id"))
	if err != nil {
		return c.JSON(http.StatusBadRequest, Res{"message": err.Error()})
	}

	err = service.Session.DB(DATABASE).C(COLLECTION).RemoveId(id)
	if err != nil {
		if err == mgo.ErrNotFound {
			return c.NoContent(http.StatusNotFound)
		}
		return err
	}

	return c.NoContent(http.StatusOK)
}
