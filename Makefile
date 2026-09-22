.DEFAULT_GOAL := up

PORT ?= 4000
export PORT

.PHONY: all pull up serve log build versions down

all: up

pull:
	docker compose pull

up serve:
	docker compose up -d

log:
	docker compose logs -f

build:
	docker compose run --rm --no-deps -T main /pages.sh build

versions:
	docker compose run --rm --no-deps -T --entrypoint ruby main \
		-rgithub-pages -rjson -e 'puts RUBY_DESCRIPTION; puts "JSON #{JSON::VERSION}"; Gem.loaded_specs.sort.each { |name, spec| puts "#{name} #{spec.version}" }'

down:
	docker compose down
