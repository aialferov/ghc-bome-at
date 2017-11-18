USER = aialferov
PROJECT = ghc-bome-at
VERSION = latest

REBAR = ./rebar3

BIN_PATH_IN = $(shell $(REBAR) path --bin)

SUITES_DIR = suites
PRIV_DIR = priv
LOG_DIR = _build/logs

BUILD_DIR = _build
BUILD_DIR_IMAGE = $(BUILD_DIR)/image

all:
	$(REBAR) compile
	$(REBAR) unlock

at:
	$(REBAR) ct \
		--config $(PRIV_DIR)/$(PROJECT).conf \
		--dir $(SUITES_DIR) \
		--logdir $(LOG_DIR)

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
	mkdir -p $(BUILD_DIR_IMAGE)
	mkdir -p $(BUILD_DIR_IMAGE)/bin
	mkdir -p $(BUILD_DIR_IMAGE)/etc
	mkdir -p $(BUILD_DIR_IMAGE)/usr/share/ghc-bome-at
	install -p -m 644 Dockerfile $(BUILD_DIR_IMAGE)
	install -p $(BIN_PATH_IN)/$(PROJECT) $(BUILD_DIR_IMAGE)/bin
	install -p -m 644 $(PRIV_DIR)/$(PROJECT).conf $(BUILD_DIR_IMAGE)/etc
	install -p -m 644 $(SUITES_DIR)/* $(BUILD_DIR_IMAGE)/usr/share/ghc-bome-at
	docker build $(BUILD_DIR_IMAGE) -t $(USER)/$(PROJECT):$(VERSION)

docker-push:
	docker push $(USER)/$(PROJECT):$(VERSION)

docker-run:
	docker run --rm --link ghc-bome \
		--env HOST=ghc-bome --env PORT=8080 \
		$(USER)/$(PROJECT):$(VERSION)

docker-clean:
	docker images -qf dangling=true | xargs docker rmi
