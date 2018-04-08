path := $(shell pwd)

erlang:
	@ docker run -it \
	    --volume "$(path)/tutorials:/tutorial" \
	    erlang \
	    bash

.PHONY: erlang
