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
	s = `{ Res } = ` + s + `, io:format("~s", [Res]), init:stop()`
	fmt.Printf("Running %#q\n", s)

	return exec.Command("erl", "-eval", s, "-noinput", "-start_epmd", "false").CombinedOutput()
}

func must(err error) {
	if err != nil {
		panic(err)
	}
}

func TestGenerateStruct(t *testing.T) {
	tmp, err := generateTest()
	require.NoError(t, err)
	//defer os.RemoveAll(tmp)
	fmt.Println(tmp)

	b, err := runErl(tmp, `ergo:testing_testStruct({"Hi", 1,2,{"GG"}})`)
	require.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, "?", string(b))
}

func TestGenerateMap(t *testing.T) {
	tmp, err := generateTest()
	require.NoError(t, err)
	//defer os.RemoveAll(tmp)
	fmt.Println(tmp)

	b, err := runErl(tmp, `ergo:testing_testMap(#{a => 1})`)
	require.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, "map[a:1]", string(b))

	b, err = runErl(tmp, `ergo:testing_testMap(#{a => "a"})`)
	require.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, "map[a:a]", string(b))

	b, err = runErl(tmp, `ergo:testing_testMap(#{a => {1, [c,d]}})`)
	require.NoError(t, err, "Command output: %s", b)
	assert.Equal(t, "map[a:[1 [c d]]]", string(b))
}
