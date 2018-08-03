pub-sub:
	@ docker-compose up -d publisher
	@ docker-compose up -d --scale subscriber=4 subscriber

req-rep:
	@ docker-compose up -d requestor
	@ docker-compose up -d --scale replier=4 replier

queueing:
	@ docker-compose up -d producer
	@ docker-compose up -d consumer

down:
	@ docker-compose down


.PHONY: pub-sub req-rep queueing
