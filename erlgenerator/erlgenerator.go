package main

import (
	"fmt"
	"go/types"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"unicode"
	"unicode/utf8"

	"bytes"

	"golang.org/x/tools/go/loader"
	"golang.org/x/tools/imports"
)

//go:generate bash -c "(echo package main; echo; echo 'const ConvertStr = `'; sed 1s/runtime/main/ ../runtime/convert.go; echo '`') >convert.go"
//go:generate bash -c "(echo package main; echo; echo 'const MakeTermStr = `'; sed 1s/runtime/main/ ../runtime/maketerm.go; echo '`') >maketerm.go"

const outDir = "out" // TODO cmd-line arg

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
	fmt.Fprintf(w, "func %s(env *C.ErlNifEnv, argc C.int, __argv *C.ERL_NIF_TERM) C.ERL_NIF_TERM {\n", glueName)
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
		fmt.Fprintf(w, "\tif __err = Convert(env, *__argv, &%s); __err != nil { return MakeError(env, __err.Error()) }\n", param.Name())
		fmt.Fprintf(w, "__argv = (*C.ERL_NIF_TERM)(unsafe.Pointer(8 + uintptr(unsafe.Pointer(__argv))))\n")
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
		fmt.Fprintf(w, "\treturn MakeAtom(env, \"ok\")\n")
		return
	}

	if isErrorType(results.At(results.Len() - 1)) {
		// If an error was returned, return {'error', err.Error()}.
		// Otherwise (the error was nil), exclude the nil from the return values.
		fmt.Fprintf(w, "\tif err, ok := _results[%d].(error); ok && err != nil { return MakeError(env, err.Error()) } else { _results = _results[:len(_results)-2] }\n", results.Len()-1)
	}
	fmt.Fprintf(w, "\treturn MakeReturnValue(env, _results)")
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

	var decls []*FuncDecl
	for _, name := range pkgNames {
		pkg := prog.Imported[name]
		fmt.Println("Processing package", name)
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
					fmt.Println("  Processing", name, conf.Fset.Position(obj.Pos()), " -- ", t)
				}
				decl := &FuncDecl{
					name:      name,
					pkg:       pkg,
					Signature: t,
				}
				decls = append(decls, decl)
			default:
				// For now don't process any other definition (such as a var).
			}
		}
	}

	os.MkdirAll(outDir, 0755)
	generateErl(decls)
	generateGlue(decls)
	generateC(decls)
}

func generateErl(decls []*FuncDecl) {
	erlOut, err := os.Create(filepath.Join(outDir, "ergo.erl"))
	if err != nil {
		fail("Cannot write erl file: %s", err)
	}
	defer erlOut.Close()

	erlOut.Write([]byte(ErlHeaderStr))
	for _, decl := range decls {
		decl.PrintErl(erlOut)
	}
}

func generateGlue(decls []*FuncDecl) {
	ioutil.WriteFile(filepath.Join(outDir, "convert.go"), []byte(ConvertStr), 0644)
	ioutil.WriteFile(filepath.Join(outDir, "maketerm.go"), []byte(MakeTermStr), 0644)

	out := new(bytes.Buffer)
	out.WriteString(`package main

// #include "erl_nif.h"
import "C"

func main() {}
`)

	for _, decl := range decls {
		decl.PrintGlue(out)
	}

	glueCode := out.Bytes()
	if b, err := imports.Process("ergo.go", glueCode, nil); err == nil {
		glueCode = b
	}

	glueOut, err := os.Create(filepath.Join(outDir, "ergo.go"))
	if err != nil {
		fail("Cannot write go file: %s", err)
	}
	defer glueOut.Close()
	ioutil.WriteFile(filepath.Join(outDir, "ergo.go"), glueCode, 0555)
}

func generateC(decls []*FuncDecl) {
	// NB: _cgo_export.h will contain definitions of each function without the `const` specified
	// for the arg parameter. This would in turn cause ERL_NIF_INIT to fail. Therefore we generate a
	// C file with the correct declarations.

	out, err := os.Create(filepath.Join(outDir, "ergo_nif_init.c"))
	if err != nil {
		fail("Cannot write C file: %s", err)
	}
	defer out.Close()

	out.Write([]byte("#include \"erl_nif.h\"\n\n"))
	for _, decl := range decls {
		glueName := GoToGlue(decl.pkg.Pkg.Name(), decl.name)
		fmt.Fprintf(out, "extern ERL_NIF_TERM %s(ErlNifEnv* p0, int p1, const ERL_NIF_TERM* p2);\n", glueName)
	}

	fmt.Fprintf(out, "\nstatic ErlNifFunc nif_funcs[] = {\n")
	for _, decl := range decls {
		glueName := GoToGlue(decl.pkg.Pkg.Name(), decl.name)
		erlName := GoToErl(decl.pkg.Pkg.Name(), decl.name)
		fmt.Fprintf(out, "\t{\"%s\", %d, %s},\n", erlName, decl.Params().Len(), glueName)
	}
	fmt.Fprintf(out, "};\n\nERL_NIF_INIT(ergo, nif_funcs, NULL, NULL, NULL, NULL);\n")
}

func fail(format string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, format, args...)
	os.Exit(2)
}
