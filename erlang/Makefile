path := $(shell pwd)

erlang:
	@ docker run -it \
	    --volume "$(path)/tutorials:/tutorials" \
	    erlang \
	    bash

.PHONY: erlang
