REBAR = ./rebar3

BUILD_DIR = _build
CT_LOG_INDEX = _build/test/logs/index.html

all:
	@echo "Usage: make at|at-display"

at:
	$(REBAR) ct

clean:
	$(REBAR) clean -a
	$(REBAR) unlock

distclean: clean
	rm -rf $(BUILD_DIR)

ifeq ($(shell uname), Darwin)
    CT_DISPLAY_CMD = open
endif
ifeq ($(shell uname), Linux)
    CT_DISPLAY_CMD = xdg-open
endif

at-display:
	$(CT_DISPLAY_CMD) $(CT_LOG_INDEX)
