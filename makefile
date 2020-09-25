ID = 25sep2020
IMAGE_NAME = covidrisk_$(ID)

default:
	@echo targets:  build bash

build:
	docker build -t $(IMAGE_NAME) -f Dockerfile .


bash:
	docker run -it \
           -p 3838:9012 \
           --entrypoint /bin/bash \
           $(IMAGE_NAME)
