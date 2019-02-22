sensor_name ?= $(shell cat /dev/random | LC_CTYPE=C tr -dc "[:alpha:]" | head -c 8)


up:
	docker-compose up -d rabbitmq postgres
	sleep 5
	docker-compose up -d
	docker-compose scale coordinator=6 datamanager=2

down:
	docker-compose down


.PHONY: up down
