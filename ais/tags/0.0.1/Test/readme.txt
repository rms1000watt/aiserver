**************************
AIS Regression Suite
**************************

Purpose: Regression Tests for All Major System Functions

Organization of the Regression Suite
Regression tests are named Test_*.sl.
Each regression test is designed to be an independant test.
Regression tests implement the regression test interface by using
common utility lambdas loaded by the RegTest.sl script.

Suites of tests can be launched using the following scripts:
RunQuick.sl 			-- Runs "Quick" suite
RunTiny.sl 				-- Runs "Tiny" suite
RunCore.sl				-- Runs "Core" suite
RunCheck.sl				-- Runs "Check" suite
RunTiming.sl			-- Runs "Timing" suite
RunBenchMark.sl			-- Runs "BenchMark" suite

Utility Scritps
RegTest.sl		-- Conditional calls RegTestLoad.ls.
RegTestLoad.sl	-- Creates the regTest lambda and a few other useful lambdas 
					with global scope. It also loads up the Libraires and 
					locks globals the first time it is run. This script will
					only be compiled one time.

Regression Test Interface
The regression test interface is a set of coding rules that
ensure each regression test can be run in an automated fashon
and produce consistent and comprehensive output. 

For convienence and consistency, a collection
of utility lambdas to make coding a test script simple is 
provided in the RegTest.sl script. This script is
run at the beginning of each test to load these utilities
in the environment. RegTest.sl may be run more than once as it
only modifies the environment the first time it is run.

**********************
Coding a Test file
**********************
1. Each regression test file must be named Test_*.sl.
2. Each regression test must run (runScript "RegTest.sl")
	before any other code in the script. This creates 
	the regTest lambda as needed and allows the test script to 
	be run independantly.
	
4. Running RegTest.sl installs a few global lambdas that should
	be used in test scripts to report test progress and results. 
	These include:
	(testStart nameoftest)
	(testit cmd result)
	(testError errorstring)
	(testEnd)
	See existing test scripts for examples of how these are used.
5. Data files used by a test should have names prefixed with that 
	test's name. For instance: 

**********************
Registering a new test
**********************
When a new test script is written you need to register it in the
runTests lambda in the RegTestLoad.sl script file. Simply add
the test script to the aRunMatrix and specify which of the 
test suites should include the test script. aRunMatrix is pretty
much self explanitory.

**********************
Running a Test Suite
**********************
You can run a test suite file like RunTiny.sl directly.
When run directly the Run*.sl files will execute each test
with JitOn and then with JitOff.

You may also Launch RunStartupOnly.sl and then run suites
using the (runSuite suite jitmode iterations) lambda. runSuite is
defined in the RegTestLoad.sl file.
(runSuite suite [jitmode] [interations])
Arguments:
	suite	a string containing the name of the suite. ex: "All"
	jitmode	an optional string. One of "On", "Off", "Both"
			"Both" is the default.
	iterations Number of times to run suite. default is 1.

**********************
Running a Test
**********************
You can run a test file like Test_Benchmark.sl directly.
When run directly the Test_*.sl files will execute with JitOn.
You may also Launch RunStartupOnly.sl and then run tests
using the (runTest testfile jitmode iterations) lambda. runTest is 
defined in the RegTestLoad.sl file.
(runTest testfile [jitmode] [iterations])
Arguments
	testfile	the name of the testfile. ex: "Test_Complex.sl"
	jitmode		an option string. One of "On", "Off", "Both"
				"Both" is the default.
	iterations	Number of times to run test. Default is 1.

Notes:
RegTestLoad.sl is called conditionaly by RegTest.sl to avoid
the compiliation of the lambdas in RegTestLoad.sl unless this 
compilation is necessary. An unwanted compiliation would replace
the previously compiled lambdas and cause possible loss of important 
state information. RegTest.sl ensures that RegTestLoads.sl is
compiled only once.

The regression suite tests the system against the current versions
of Libraries located in the Libraries folder. 
The cabinets are set to auto load newer library source files when they
have found to be changed.

The libraries are always available for any test. This ensures the
tests will cohabitate with the standard libraries. The libraries
are considered core assets of the environment.

** There should be no run order dependencies among test scripts.

** Test scripts should clean up temporary files before and after they run.

** Test scripts should put temporary files in the temp fodler. This avoids
these temporary files being checked into the source tree by mistake.

** Test scripts should expect that the temp folder may be cleared out by
other scripts -- no persistent data should be stored in the temp folder.

** Test scripts should never define a lambda arleady defined in a library!


