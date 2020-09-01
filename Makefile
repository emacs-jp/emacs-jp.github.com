## Makefile

all:

DOCKERLOCK := .docker.lock

##############################

.PHONY: all
all: up

$(DOCKERLOCK): up
	touch $@

.PHONY: up
up:
	docker-compose up -d

# -T option https://github.com/docker/compose/issues/7306
.PHONY: build
build: $(DOCKERLOCK)
	docker-compose exec -T main jekyll build --trace

.PHONY: down
down:
	docker-compose down
	rm -rf $(DOCKERLOCK)
