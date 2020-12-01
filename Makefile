
.PHONY: docker

external:
	mkdir external

external/digitaljs_peek_poke_tester: external
	cd external && git clone https://github.com/JakubSzczerbinski/digitaljs_peek_poke_tester.git
	cd external/digitaljs_peek_poke_tester && npm install

test: external/digitaljs_peek_poke_tester
	sbt test

docker: Dockerfile build.sbt src/* project/*
	docker build -t szczerbi/f2d_build .

firrtl2digitaljs.jar: docker docker_build.sh
	sh docker_build.sh
