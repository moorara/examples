path := $(shell pwd)

clean:
	@ find tutorials -name "Cargo.lock" | xargs rm -f
	@ find tutorials -name "target" | xargs rm -rf

rust:
	@ docker run -it \
	    --volume "$(path)/tutorials:/tutorials" \
	    rust \
	    bash

.PHONY: clean rust
