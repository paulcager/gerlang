CFLAGS=-Iinclude

all: nif/ergo.so nif/ergo.beam

clean:
	rm -rf nif/ergo.so nif/gerlang.h nif/libgerlang.a nif/libgerlang.h nif/ergo.beam _obj

nif/ergo.beam: nif/ergo.erl
	erlc -o nif/ nif/ergo.erl

nif/ergo.so: nif/libgerlang.a nif/ergo_nif.c nif/gerlang.h
	cd nif && $(CC) $(CFLAGS) -o ergo.so -fpic -shared ergo_nif.c libgerlang.a

nif/libgerlang.a nif/gerlang.h: gerlang.go
	go build -buildmode=c-archive -o nif/libgerlang.a


#nif/gerlang.h: gerlang.go
#	go tool cgo --exportheader nif/gerlang.h gerlang.go