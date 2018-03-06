all: compile test

compile: gerlang.so ergo.beam

clean:
	rm -rf ergo.beam erl_crash.dump gerlang.h gerlang.so examples/sample_plugin.so

ergo.beam: ergo.erl
	erlc -Werror -Wall ergo.erl

gerlang.so: gerlang.go
	# There must be a better way to do this! Run once to generate gerlang.h, and then again to generate
	# gerlang.so with the C code included. I could replace the first call by a direct call of
	# cgo, but doesn't feel like the correct solution.
	CGO_CFLAGS_ALLOW=.* CGO_LDFLAGS_ALLOW=.* go build -buildmode=c-shared -o gerlang.so gerlang.go
	CGO_CFLAGS_ALLOW=.* CGO_LDFLAGS_ALLOW=.* go build -buildmode=c-shared -o gerlang.so

test: examples/sample_plugin.so
	#erl -eval 'ergo:call("","",[1,2,3]), init:stop()' -noinput -start_epmd false
	cd examples && go test

examples/sample_plugin.so:
	cd examples && go build -buildmode=plugin -o sample_plugin.so sample_plugin.go
