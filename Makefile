all: build

build:
	stack build --executable-profiling

run:
	stack exec nanc test.c
