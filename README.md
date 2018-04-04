# gerlang
Calling Go functions from Erlang

Reminder for myself:

    CGO_LDFLAGS_ALLOW="-shared" go build -v  -buildmode=c-shared -o gerlang.so github.com/paulcager/gerlang

gerlang:call(PluginLib, FuncName, ParmList).

Drops down into NIF that then calls