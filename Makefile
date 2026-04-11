.PHONY: build clean fetch serve docker-build docker-run

build:
	rsync -r static/ output/

clean:
	rm -rf output

docker-build:
	set -e && \
	sudo docker build --file=Containerfile . && \
	docker image tag $$(docker images --format "{{.ID}}" --no-trunc | head -n 1) prod-planet

docker-deploy:
	set -e && \
	docker run --rm --interactive prod-planet>/tmp/planet.tar && \
	sudo -u prod-planet tar xvf /tmp/planet.tar

fetch:
	gosh planet.scm

serve: build
	gosh serve.scm 8080 output