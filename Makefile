
.PHONY: docker

docker: Dockerfile build.sbt src/* project/*
	docker build -t szczerbi/f2d_build .

firrtl2digitaljs.jar: docker docker_build.sh
	sh docker_build.sh
