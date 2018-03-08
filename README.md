# gerlang
Calling Go functions from Erlang

Reminder for myself:

    CGO_LDFLAGS_ALLOW="-shared" go build -v  -buildmode=c-shared -o gerlang.so github.com/paulcager/gerlang