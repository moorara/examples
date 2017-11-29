sensor_name ?= $(shell cat /dev/random | LC_CTYPE=C tr -dc "[:alpha:]" | head -c 8)


down:
	docker-compose down

up:
	docker-compose up -d rabbitmq postgres
	sleep 5
	docker-compose up -d
	docker-compose scale coordinator=6 datamanager=2


.PHONY: down up
