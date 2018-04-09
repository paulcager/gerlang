package main

import (
	"fmt"
	"go/types"
	"io"
	"os"

	"unicode/utf8"

	"unicode"

	"path/filepath"

	"bytes"

	"io/ioutil"

	"golang.org/x/tools/go/loader"
	"golang.org/x/tools/imports"
)

const outDir = "ergo/out" // TODO cmd-line arg

var (
	conf    = loader.Config{}
	verbose = true
)

type FuncDecl struct {
	name string
	*types.Signature
	pkg *loader.PackageInfo
}

func GoToErl(pkg, name string) string {
	ch, size := utf8.DecodeRune([]byte(name))
	return fmt.Sprintf("%s_%c%s", pkg, unicode.ToLower(ch), name[size:])
	// TODO - do a better renaming, e.g. conf.FromArgs becomes conf_from_args
}

func GoToGlue(pkg, name string) string {
	ch, size := utf8.DecodeRune([]byte(pkg))
	return fmt.Sprintf("%c%s_%s", unicode.ToUpper(ch), pkg[size:], name)
}

func (f *FuncDecl) PrintErl(w io.Writer) {
	// TODO - include types such as `when is_list(A)` and ideally typespecs.
	name := GoToErl(f.pkg.Pkg.Name(), f.name)
	fmt.Fprintf(w, "%s(", name)
	// TODO - do a better renaming, e.g. conf.FromArgs becomes conf_from_args
	for i := 0; i < f.Params().Len(); i++ {
		v := f.Params().At(i)
		paramName := "_" + v.Name()
		fmt.Fprint(w, paramName)
		if i < f.Params().Len()-1 {
			fmt.Fprint(w, ", ")
		}
	}
	fmt.Fprint(w, ") ->\n    exit(no_nif).\n\n")
}

func (f *FuncDecl) PrintGlue(w io.Writer) {
	glueName := GoToGlue(f.pkg.Pkg.Name(), f.name)
	results := f.Results()
	params := f.Params()
	fmt.Fprintf(w, "//export %s\n", glueName)
	fmt.Fprintf(w, "func %s(env *C.ErlNifEnv, argc C.int, __argv []C.ERL_NIF_TERM) C.ERL_NIF_TERM {\n", glueName)
	defer fmt.Fprintf(w, "}\n")
	if params.Len() > 0 {
		fmt.Fprintf(w, "\tvar __err error\n")
	}
	for i := 0; i < params.Len(); i++ {
		param := params.At(i)
		fmt.Fprintf(w, "\tvar %s %s\n", param.Name(), param.Type().String())
	}
	for i := 0; i < params.Len(); i++ {
		param := params.At(i)
		fmt.Fprintf(w, "\tif __err = runtime.Convert(env, __argv[%d], &%s); __err != nil { return runtime.MakeError(env, __err.Error()) }\n", i, param.Name())
	}

	fmt.Fprintf(w, "\t")
	if f.Results().Len() > 0 {
		fmt.Fprintf(w, "_results := make([]interface{}, %d)\n\t", f.Results().Len())
		for i := 0; i < f.Results().Len(); i++ {
			if i > 0 {
				fmt.Fprint(w, ", ")
			}
			fmt.Fprintf(w, "_results[%d]", i)
		}
		fmt.Fprintf(w, " = ")
	}
	fmt.Fprintf(w, "%s.%s(", f.pkg.Pkg.Name(), f.name)
	for i := 0; i < params.Len(); i++ {
		param := params.At(i)
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprintf(w, "%s", param.Name())
	}
	if f.Variadic() {
		fmt.Fprintf(w, "...")
	}
	fmt.Fprintf(w, ")\n")

	if results.Len() == 0 {
		fmt.Fprintf(w, "\treturn runtime.MakeAtom(env, \"ok\")\n")
		return
	}

	if isErrorType(results.At(results.Len() - 1)) {
		// If an error was returned, return {'error', err.Error()}.
		// Otherwise (the error was nil), exclude the nil from the return values.
		fmt.Fprintf(w, "\tif err, ok := _results[%d].(error); ok && err != nil { return runtime.MakeError(env, err.Error()) } else { _results = _results[:len(_results)-2] }\n", results.Len()-1)
	}
	fmt.Fprintf(w, "\treturn runtime.MakeReturnValue(env, _results)")
}

func isErrorType(v *types.Var) bool {
	return v.Type().String() == "error" // TODO - doesn't cover cases where returns MyStructThatImplementsError
}

func main() {
	pkgNames := os.Args[1:] // TODO - have proper cmd-line processing.
	pkgNames = []string{"fmt", "os"}
	conf.FromArgs(pkgNames, false)
	prog, err := conf.Load()
	if err != nil {
		fail("Cannot process packages %v: %s", pkgNames, err)
	}

	os.MkdirAll(outDir, 0777)
	erlOut, err := os.Create(filepath.Join(outDir, "ergo.erl"))
	if err != nil {
		fail("Cannot open ergo.erl: %s", err)
	}
	defer erlOut.Close()

	glueOut := new(bytes.Buffer)

	fmt.Fprintf(erlOut, "%s", `-module(ergo).
-compile(export_all).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./ergo", 0).

`)

	fmt.Fprintf(glueOut, "%s", `package main

import "C"

`)

	for _, name := range pkgNames {
		pkg := prog.Imported[name]
		fmt.Println("// Processing package", name)
		pkgScope := pkg.Pkg.Scope()
		items := pkgScope.Names()
		for _, name := range items {
			obj := pkgScope.Lookup(name)
			if obj == nil || !obj.Exported() {
				continue
			}
			switch t := obj.Type().(type) {
			case *types.Signature:
				if verbose {
					fmt.Println("//    Processing", name, conf.Fset.Position(obj.Pos()), " -- ", t)
				}
				decl := &FuncDecl{
					name:      name,
					pkg:       pkg,
					Signature: t,
				}
				decl.PrintErl(erlOut)
				decl.PrintGlue(glueOut)
			default:
				// For now don't process any other definition (such as a var).
			}
		}
	}

	glueCode := glueOut.Bytes()
	if b, err := imports.Process("ergo.go", glueCode, nil); err == nil {
		glueCode = b
	}

	ioutil.WriteFile(filepath.Join(outDir, "ergo.go"), glueCode, 0555)
}

func fail(format string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, format, args...)
	os.Exit(2)
}
