PROJECT = cortex_remote_write
PROJECT_DESCRIPTION = Prometheus remote_write for Cortex, as a library!

DEPS = \
	hackney \
	prometheus \
	snappy

dep_elvis_mk = git https://github.com/inaka/elvis.mk.git
dep_hackney = git https://github.com/benoitc/hackney 1.16.0
dep_snappy = git https://github.com/skunkwerks/snappy-erlang-nif.git
dep_version.mk = git https://github.com/manifest/version.mk.git


BUILD_DEPS = elvis_mk gpb version.mk
TEST_DEPS = meck
TEST_DIR = tests
DIALYZER_DIRS = --src src tests

DEP_PLUGINS = elvis_mk version.mk

SHELL_OPTS = -eval 'application:ensure_all_started(cortex_remote_write).' -config sys

include erlang.mk
