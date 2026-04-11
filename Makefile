.PHONY: build clean fetch serve

build:
	rsync -r static/ output/

clean:
	rm -rf output

fetch:
	gosh planet.scm

serve: build
	gosh serve.scm 8080 output