package gerlang

import (
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"testing"

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

func TestGenerateStruct(t *testing.T) {
	tmp, err := generateTest()
	require.NoError(t, err)
	//defer os.RemoveAll(tmp)
	fmt.Println(tmp)

}
