all:
	go install ./runtime
	go install ./erlgenerator

clean:
	rm -f runtime.h

runtime:

erlgenerator:

gerlang.so: gerlang.go
	# There must be a better way to do this! Run once to generate gerlang.h, and then again to generate
	# gerlang.so with the C code included. I could replace the first call by a direct call of
	# cgo, but doesn't feel like the correct solution.
	CGO_CFLAGS_ALLOW=.* CGO_LDFLAGS_ALLOW=.* go build -buildmode=c-shared -o gerlang.so gerlang.go
	CGO_CFLAGS_ALLOW=.* CGO_LDFLAGS_ALLOW=.* go build -buildmode=c-shared -o gerlang.so

test:
	echo TODO