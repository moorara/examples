clean:
	@ rm -rf keys

keys:
	@ ./generate-keys.sh

up:
	@ docker-compose up -d

down:
	@ docker-compose down


.PHONY: clean keys
.PHONY: up down
