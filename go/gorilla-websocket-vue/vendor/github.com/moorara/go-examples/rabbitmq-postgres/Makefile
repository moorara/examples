sensor_name ?= $(shell cat /dev/random | LC_CTYPE=C tr -dc "[:alpha:]" | head -c 8)

up:
	docker-compose up -d

down:
	docker-compose down

sensor:
	@ go run sensor/sensor.go --name $(sensor_name)

coordinator:
	@ go run coordinator/exec/main.go

datamanager:
	@ go run data/exec/main.go

webapp:
	@ cd web && go run main.go

.PHONY: up down sensor coordinator datamanager webapp
