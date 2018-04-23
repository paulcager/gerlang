package main

import (
	"bytes"
	"fmt"
	"go/types"
	"io"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"unicode"
	"unicode/utf8"

	"go/token"

	"golang.org/x/tools/go/loader"
	"golang.org/x/tools/imports"
)

//go:generate bash -c "(echo package main; echo; echo 'const ConvertStr = `'; sed 1s/runtime/main/ ../runtime/convert.go; echo '`') >convert.go"
//go:generate bash -c "(echo package main; echo; echo 'const MakeTermStr = `'; sed 1s/runtime/main/ ../runtime/maketerm.go; echo '`') >maketerm.go"

var (
	conf = loader.Config{}
)

type FuncDecl struct {
	pkg      *loader.PackageInfo
	position string
	name     string
	//*types.Signature
	receiver *types.Var
	params   []*types.Var
	results  []*types.Var
	variadic bool
}

// TODO - sort out the awful replication of the receiver test.

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
	recv := f.receiver
	name := GoToErl(f.pkg.Pkg.Name(), f.name)
	if f.receiver != nil {
		t := recv.Type()
		ptr, ok := t.(*types.Pointer)
		for ok {
			t = ptr.Elem()
			ptr, ok = t.(*types.Pointer)
		}

		name = GoToErl(f.pkg.Pkg.Name(), t.(*types.Named).Obj().Name()+"_"+f.name)
	}

	fmt.Fprintf(w, "%s(", name)

	params := f.params
	if f.receiver != nil {
		params = append([]*types.Var{f.receiver}, params...)
	}

	for i, v := range params {
		paramName := "_" + v.Name()
		fmt.Fprint(w, paramName)
		if i < len(params)-1 {
			fmt.Fprint(w, ", ")
		}
	}
	fmt.Fprint(w, ") ->\n    exit(no_nif).\n\n")
}

func (f *FuncDecl) PrintGlue(w io.Writer) {
	params := f.params
	results := f.results
	recv := f.receiver
	glueName := GoToGlue(f.pkg.Pkg.Name(), f.name)
	if recv != nil {
		t := recv.Type()
		ptr, ok := t.(*types.Pointer)
		for ok {
			t = ptr.Elem()
			ptr, ok = t.(*types.Pointer)
		}

		glueName = GoToGlue(f.pkg.Pkg.Name(), t.(*types.Named).Obj().Name()+"_"+f.name)
	}

	fmt.Fprintf(w, "//export %s\n", glueName)
	fmt.Fprintf(w, "func %s(env *C.ErlNifEnv, argc C.int, __argv *C.ERL_NIF_TERM) C.ERL_NIF_TERM {\n", glueName)
	fmt.Fprintf(w, "// %s\n", f.position)
	defer fmt.Fprintf(w, "}\n")
	if len(params) > 0 || recv != nil {
		fmt.Fprintf(w, "\tvar __err error\n")
	}
	if recv != nil {
		typ := types.TypeString(recv.Type(), typeQualifier)
		fmt.Fprintf(w, "\tvar %s %s\n", recv.Name(), typ)
	}
	for i := range params {
		param := params[i]
		typ := types.TypeString(param.Type(), typeQualifier)
		fmt.Fprintf(w, "\tvar %s %s\n", param.Name(), typ)
	}
	if recv != nil {
		fmt.Fprintf(w, "\tif __err = Convert(env, *__argv, &%s); __err != nil { return MakeError(env, __err.Error()) }\n", recv.Name())
	}
	for i := range params {
		param := params[i]
		fmt.Fprintf(w, "\tif __err = Convert(env, *__argv, &%s); __err != nil { return MakeError(env, __err.Error()) }\n", param.Name())
		fmt.Fprintf(w, "__argv = (*C.ERL_NIF_TERM)(unsafe.Pointer(8 + uintptr(unsafe.Pointer(__argv))))\n")
	}

	fmt.Fprintf(w, "\t")
	if len(results) > 0 {
		fmt.Fprintf(w, "_results := make([]interface{}, %d)\n\t", len(results))
		for i := range results {
			if i > 0 {
				fmt.Fprint(w, ", ")
			}
			fmt.Fprintf(w, "_results[%d]", i)
		}
		fmt.Fprintf(w, " = ")
	}
	if recv == nil {
		fmt.Fprintf(w, "%s.%s(", f.pkg.Pkg.Name(), f.name)
	} else {
		fmt.Fprintf(w, "%s.%s(", recv.Name(), f.name)
	}
	for i := range params {
		param := params[i]
		if i > 0 {
			fmt.Fprint(w, ", ")
		}
		fmt.Fprintf(w, "%s", param.Name())
	}
	if f.variadic {
		fmt.Fprintf(w, "...")
	}
	fmt.Fprintf(w, ")\n")

	if len(results) == 0 {
		fmt.Fprintf(w, "\treturn MakeAtom(env, \"ok\")\n")
		return
	}

	if isErrorType(results[len(results)-1]) {
		// If an error was returned, return {'error', err.Error()}.
		// Otherwise (the error was nil), exclude the nil from the return values.
		fmt.Fprintf(w, "\tif err, ok := _results[%d].(error); ok && err != nil { return MakeError(env, err.Error()) } else { _results = _results[:len(_results)-2] }\n", len(results)-1)
	}
	fmt.Fprintf(w, "\treturn MakeReturnValue(env, _results)")
}

func typeQualifier(p *types.Package) string {
	return p.Name()
}

func isErrorType(v *types.Var) bool {
	return v.Type().String() == "error" // TODO - doesn't cover cases where returns MyStructThatImplementsError
}

func main() {
	//t := time.Now()
	//tv := reflect.ValueOf(&t)
	//fmt.Println(tv.Elem().Field(0).Uint())
	//fmt.Println(tv.Elem().Field(0).CanSet())
	//fmt.Println(tv.Elem().Field(0).Addr())
	//tv.Elem().Field(0).SetUint(99)
	//os.Exit(2)

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

			switch obj := obj.(type) {
			case *types.Func:
				t := obj.Type().(*types.Signature)
				if verbose {
					fmt.Println("  Processing", name, conf.Fset.Position(obj.Pos()), " -- ", t)
				}
				decl := createDecl(conf.Fset.Position(obj.Pos()), name, pkg, t)
				decls = append(decls, decl)

			case *types.TypeName:
				t := obj.Type().(*types.Named)
				if verbose {
					fmt.Println("  Processing", name, conf.Fset.Position(obj.Pos()), " -- ", t)
				}
				for i := 0; i < t.NumMethods(); i++ {
					meth := t.Method(i)
					if meth.Exported() {
						decl := createDecl(conf.Fset.Position(meth.Pos()), meth.Name(), pkg, meth.Type().(*types.Signature))
						decls = append(decls, decl)
					}
				}

			default:
				// For now don't process any other definition (such as a var).
			}
		}
	}

	// TODO - should I just check `file erl`?
	nifDir, _ := filepath.Abs("nif")

	os.MkdirAll(outDir, 0755)
	generateErl(decls)
	generateGlue(decls)
	generateC(decls)

	err = os.Chdir(outDir)
	if err != nil {
		fail("Could not chdir %q: %s", outDir, err)
	}

	if verbose {
		fmt.Printf("Nif directory is %s\n", nifDir)
	}

	os.Setenv("CGO_LDFLAGS_ALLOW", ".*")
	os.Setenv("C_INCLUDE_PATH", nifDir+"/include")
	os.Setenv("LIBRARY_PATH", nifDir+"/lib")

	output, err := exec.Command("go", "build", "-o", "ergo.so", "-buildmode", "c-shared").CombinedOutput()
	if err != nil {
		fail("Could not compile generated package %q: %s\n%s", outDir, err, output)
	}

	output, err = exec.Command("erlc", "ergo.erl").CombinedOutput()
	if err != nil {
		fail("Could not compile ergo.erl: %s\n%s", err, output)
	}
}

func createDecl(pos token.Position, name string, pkg *loader.PackageInfo, sig *types.Signature) *FuncDecl {
	var params, results []*types.Var

	for i := 0; i < sig.Params().Len(); i++ {
		params = append(params, sig.Params().At(i))
	}
	for i := 0; i < sig.Results().Len(); i++ {
		results = append(results, sig.Results().At(i))
	}

	return &FuncDecl{
		position: pos.String(),
		name:     name,
		pkg:      pkg,
		variadic: sig.Variadic(),
		params:   params,
		results:  results,
		receiver: sig.Recv(),
	}
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
	} else {
		fmt.Fprintf(os.Stderr, "Imports failed [%T]: %s", err, err)
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
		recv := decl.receiver
		glueName := GoToGlue(decl.pkg.Pkg.Name(), decl.name)
		if recv != nil {
			t := recv.Type()
			ptr, ok := t.(*types.Pointer)
			for ok {
				t = ptr.Elem()
				ptr, ok = t.(*types.Pointer)
			}

			glueName = GoToGlue(decl.pkg.Pkg.Name(), t.(*types.Named).Obj().Name()+"_"+decl.name)
		}
		fmt.Fprintf(out, "extern ERL_NIF_TERM %s(ErlNifEnv* p0, int p1, const ERL_NIF_TERM* p2);\n", glueName)
	}

	fmt.Fprintf(out, "\nstatic ErlNifFunc nif_funcs[] = {\n")

	// TODO - this is very crappy.
	for _, decl := range decls {
		recv := decl.receiver
		glueName := GoToGlue(decl.pkg.Pkg.Name(), decl.name)
		numParams := len(decl.params)
		if recv != nil {
			numParams++
			t := recv.Type()
			ptr, ok := t.(*types.Pointer)
			for ok {
				t = ptr.Elem()
				ptr, ok = t.(*types.Pointer)
			}

			glueName = GoToGlue(decl.pkg.Pkg.Name(), t.(*types.Named).Obj().Name()+"_"+decl.name)
		}
		erlName := GoToErl(decl.pkg.Pkg.Name(), decl.name)
		if decl.receiver != nil {
			t := recv.Type()
			ptr, ok := t.(*types.Pointer)
			for ok {
				t = ptr.Elem()
				ptr, ok = t.(*types.Pointer)
			}

			erlName = GoToErl(decl.pkg.Pkg.Name(), t.(*types.Named).Obj().Name()+"_"+decl.name)
		}

		fmt.Fprintf(out, "\t{\"%s\", %d, %s},\n", erlName, numParams, glueName)
	}
	fmt.Fprintf(out, "};\n\nERL_NIF_INIT(ergo, nif_funcs, NULL, NULL, NULL, NULL);\n")
}

func fail(format string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, format, args...)
	os.Exit(2)
}
