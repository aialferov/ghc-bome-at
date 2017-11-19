USER = aialferov
PROJECT = ghc-bome-at
VERSION = latest

PORT = 8088

TARGET_PROJECT = ghc-bome
TARGET_PORT = 8080

REBAR = ./rebar3

PREFIX = usr/local

BIN_DIR = bin
BIN_PATH = $(DEST_DIR)/$(PREFIX)/$(BIN_DIR)
BIN_PATH_IN = $(shell $(REBAR) path --bin)

SUITES_DIR = $(shell $(REBAR) path --app $(PROJECT) --ebin)
PRIV_DIR = priv
LOG_DIR = _build/logs

BUILD_DIR = _build
BUILD_DIR_IMAGE = $(BUILD_DIR)/image

all:
	$(REBAR) compile
	$(REBAR) unlock

at: all
	$(REBAR) ct \
		--config $(PRIV_DIR)/$(PROJECT).conf \
		--dir $(SUITES_DIR) \
		--logdir $(LOG_DIR)

run: all
	$(BIN_PATH_IN)/$(PROJECT)

install:
	mkdir -p $(BIN_PATH)
	install -p $(BIN_PATH_IN)/$(PROJECT) $(BIN_PATH)

uninstall:
	rm -f $(BIN_PATH)/$(PROJECT)
	rmdir -p $(BIN_PATH) 2> /dev/null || true

shell:
	$(REBAR) shell
	$(REBAR) unlock

clean:
	$(REBAR) clean -a
	$(REBAR) unlock

distclean: clean
	rm -rf $(BUILD_DIR)

ifeq ($(shell uname), Darwin)
    AT_DISPLAY_CMD = open
endif
ifeq ($(shell uname), Linux)
    AT_DISPLAY_CMD = xdg-open
endif

at-display:
	$(AT_DISPLAY_CMD) $(LOG_DIR)/index.html

docker-build: all
	$(MAKE) install DEST_DIR=$(BUILD_DIR_IMAGE) PREFIX=
	install -p -m 644 Dockerfile $(BUILD_DIR_IMAGE)
	docker build $(BUILD_DIR_IMAGE) -t $(USER)/$(PROJECT):$(VERSION)

docker-push:
	docker push $(USER)/$(PROJECT):$(VERSION)

docker-run:
	docker run \
		--link $(TARGET_PROJECT) \
		--env HOST=$(TARGET_PROJECT) --env PORT=$(TARGET_PORT) \
		--rm -it -p $(PORT):$(PORT) \
		$(USER)/$(PROJECT):$(VERSION)

docker-clean:
	docker images -qf dangling=true | xargs docker rmi
