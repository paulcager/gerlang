package runtime

func ExampleObjectToTerm() {
	//	runErl(`ergo:call("examples/sample_plugin.so","SimpleParams",[22, <<"Hello">>])`)
	//
	//	// Output:
	//	// a=22, b=Hello
	//
	//	runErl(`ergo:call("examples/sample_plugin.so","SimpleParams",[1,{a,b,[]},3])`)
	//
	//	// Output:
	//	// Running `ergo:call("","",[1,2,3])`
	//	// err is <nil>
	//	// &int(1)
	//	// err is <nil>
	//	// &int(2)
	//	// err is <nil>
	//	// &int(3)
	//}
	//
	//func runErl(s string) {
	//	fmt.Printf("Running %#q\n", s)
	//	cmd := exec.Command("erl", "-eval", s+`, init:stop()`, "-noinput", "-start_epmd", "false")
	//	cmd.Stdout = os.Stdout
	//	cmd.Stderr = os.Stderr
	//	if err := cmd.Run(); err != nil {
	//		fmt.Println("Error", err)
	//	}
	//}
	//
	//func must(err error) {
	//	if err != nil {
	//		panic(err)
	//	}
}
