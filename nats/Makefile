pub-sub:
	@ docker-compose run publisher
	@ docker-compose run subscriber

req-rep:
	@ docker-compose run requestor
	@ docker-compose run replier

queueing:
	@ docker-compose run producer
	@ docker-compose run consumer

down:
	@ docker-compose down


.PHONY: pub-sub req-rep queueing
