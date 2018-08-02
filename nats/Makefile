pub-sub:
	@ docker-compose up -d publisher
	@ docker-compose up -d subscriber
	@ docker-compose scale subscriber=4

req-rep:
	@ docker-compose up -d requestor
	@ docker-compose up -d replier

queueing:
	@ docker-compose up -d producer
	@ docker-compose up -d consumer

down:
	@ docker-compose down


.PHONY: pub-sub req-rep queueing
