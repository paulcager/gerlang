package gerlang

import (
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func generateTest() (string, error) {
	tmp, err := ioutil.TempDir(".", "generated")
	if err != nil {
		return "", err
	}

	exePath := filepath.Join(tmp, "erlgenerator")
	output, err := exec.Command("go", "build", "-o", exePath, "./erlgenerator").CombinedOutput()
	if err != nil {
		os.RemoveAll(tmp)
		return "", fmt.Errorf("failed to build: %s\n%s", err, output)
	}

	output, err = exec.Command(exePath, "-out", tmp, "-verbose", "github.com/paulcager/gerlang/testing").CombinedOutput()
	if err != nil {
		os.RemoveAll(tmp)
		return "", fmt.Errorf("failed to run: %s\n%s", err, output)
	}

	return tmp, nil
}

func runErl(dir string, s string) ([]byte, error) {
	old, _ := os.Getwd()
	defer os.Chdir(old)
	os.Chdir(dir)
	s = `Res = ` + s + `, io:format("~p", [Res]), init:stop()`
	fmt.Printf("Running %#q ...", s)

	b, err := exec.Command("erl", "-eval", s, "-noinput", "-start_epmd", "false").CombinedOutput()
	fmt.Printf(" got %#q\n", b)
	return b, err
}

func runErlMult(dir string, s string) ([]byte, error) {
	old, _ := os.Getwd()
	defer os.Chdir(old)
	os.Chdir(dir)
	s = `Res = ` + s + `, io:format("~t", [Res]), init:stop()`
	fmt.Printf("Running %#q ...", s)

	b, err := exec.Command("erl", "-eval", s, "-noinput", "-start_epmd", "false").CombinedOutput()
	fmt.Printf(" got %#q\n", b)
	return b, err
}

func must(err error) {
	if err != nil {
		panic(err)
	}
}

func TestGenerateBasic(t *testing.T) {
	tmp, err := generateTest()
	require.NoError(t, err)
	defer os.RemoveAll(tmp)
	fmt.Println(tmp)

	b, err := runErl(tmp, `ergo:testing_testBasic(-99, -1, 102, true, 12.0e5, 125, atom, "")`)
	require.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, `{"-99 -1 102 true 1.2e+06 125 atom []"}`, string(b))

	// integers != 0 are also treated as true; integers are accepted as floats.
	b, err = runErl(tmp, `ergo:testing_testBasic(-99, -1, 102, 1, 12, 125, "string", [1, 2])`)
	require.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, `{"-99 -1 102 true 12 125 string [1 2]"}`, string(b))

	// Binaries are accepted as strings and []bytes
	b, err = runErl(tmp, `ergo:testing_testBasic(-99, -1, 102, 1, 12, 125, <<"bin", $X>>, <<1, 2>>)`)
	require.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, `{"-99 -1 102 true 12 125 binX [1 2]"}`, string(b))

	// Now some bad conversions
	b, err = runErl(tmp, `ergo:testing_testBasic(-99, -1, 102, xxxxx, 12.0e5, 125, a, "")`)
	assert.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, "{error,\"Cannot translate `xxxxx` - expected a bool\"}", string(b))
	b, err = runErl(tmp, `ergo:testing_testBasic(-99, -1.0, 102, true, 12.0e5, 125, a, "")`)
	assert.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, "{error,\"Cannot translate `-1.000000e+00` - expected an int\"}", string(b))
	b, err = runErl(tmp, `ergo:testing_testBasic("-99", -1, 102, true, 12.0e5, 125, a, "")`)
	assert.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, "{error,\"Cannot translate `\\\"-99\\\"` - expected an int\"}", string(b))
	b, err = runErl(tmp, `ergo:testing_testBasic(-99, -1, 102, true, 12.0e5, 125, 1, "")`)
	assert.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, "{error,\"Cannot translate `1` - expected a string\"}", string(b))

	// TODO - the following should return an error, but doesn't because we don't check for truncation.
	//b, err = runErl(tmp, `ergo:testing_testBasic(-99, -1, 102, true, 12.0e5, 125, a, [1, 2, 257])`)
	//require.Error(t, err, "Command output: %s", b)
}

func TestGenerateStruct(t *testing.T) {
	tmp, err := generateTest()
	require.NoError(t, err)
	defer os.RemoveAll(tmp)
	fmt.Println(tmp)

	b, err := runErl(tmp, `ergo:testing_testStruct(#{"S" => "Str", "I64" => 22, "Sub" => { "AA" }})`)
	require.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, `{{"Str",22,0,{<<"AA">>}}}`, string(b))

	b, err = runErl(tmp, `ergo:testing_testStruct({"Hi", 1,2,{[7, 8]}})`)
	require.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, `{{"Hi",1,2,{<<7,8>>}}}`, string(b))
}

func TestGenerateMap(t *testing.T) {
	tmp, err := generateTest()
	require.NoError(t, err)
	defer os.RemoveAll(tmp)
	fmt.Println(tmp)

	b, err := runErl(tmp, `ergo:testing_testMap(#{a => 1})`)
	require.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, `{#{"a" => 1,"new" => 22}}`, string(b))

	b, err = runErl(tmp, `ergo:testing_testMap(#{a => "a"})`)
	require.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, `{#{"a" => "a","new" => 22}}`, string(b))

	b, err = runErl(tmp, `ergo:testing_testMap(#{a => {1, [c,d]}})`)
	require.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, `{#{"a" => [1,["c","d"]],"new" => 22}}`, string(b))

	// Can also specify a map as a proplist, http://erlang.org/doc/man/proplists.html.
	b, err = runErl(tmp, `ergo:testing_testMap([ {aaa, "A"}, bbb, {will_ignore}, {ccc, 3} ])`)
	require.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, `{#{"aaa" => "A","bbb" => true,"ccc" => 3,"new" => 22}}`, string(b))
	b, err = runErl(tmp, `ergo:testing_testMap([])`)
	require.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, `{#{"new" => 22}}`, string(b))
}

func TestReturnMultiple(t *testing.T) {
	tmp, err := generateTest()
	require.NoError(t, err)
	defer os.RemoveAll(tmp)
	fmt.Println(tmp)

	b, err := runErl(tmp, `ergo:testing_testReturnMultiple(true)`)
	require.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, `{error,"returnedError"}`, string(b))
}
