package main

import "flag"

var (
	verbose  = true
	outDir   string
	pkgNames []string
)

func init() {
	flag.StringVar(&outDir, "out", "out", "directory in which to create the generated files")
	flag.BoolVar(&verbose, "verbose", true, "display debug information") // TODO set default to false

	flag.Parse()
	pkgNames = flag.Args()
	if len(pkgNames) == 0 {
		pkgNames = []string{"."}
	}
}
