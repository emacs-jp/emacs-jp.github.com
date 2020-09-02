## Makefile

all:

DOCKERLOCK := .docker.lock
PORT ?= 4000

##############################

.PHONY: all
all: up

$(DOCKERLOCK):
	PORT=${PORT} docker-compose up -d
	touch $@

.PHONY: up serve
up: $(DOCKERLOCK)
serve: $(DOCKERLOCK)

.PHONY: log
log: $(DOCKERLOCK)
	docker-compose logs -f

# -T option from https://github.com/docker/compose/issues/7306
.PHONY: build
build: $(DOCKERLOCK)
	docker-compose exec -T main jekyll build --trace

.PHONY: down
down:
	docker-compose down
	rm -rf $(DOCKERLOCK)
