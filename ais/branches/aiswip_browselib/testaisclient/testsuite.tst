# ais/testaisclient/testsuite.tst
# TestSuite - Runs all of the tests in the test suite

# CHANGE HISTORY
# Version	Date		Who		Change
# 1.0104	 9/8/2006	tlw		Note on tab to separate body from request for POST.
# 1.0065	 6/25/2005	tlw		Change _eval to _ais|eval|exp
# 1.0057	 3/11/2005	tlw		Add in App client tests
# 1.0056	 3/ 2/2005	tlw		Change request format to H0: or X1:, etc.
# 1.0053	12/18/2004	tlw		Add multiple responses, timeout, variables, documentation.
# 1.0039	 7/12/2004	tlw		Add all directives to testxmlamp.txt.

# CONNECTIONS
# The test system can simulate requests to AIS from one or more simultaneous web clients using any one
# of the 3 supported protocols, APP, HTTP, or XML.  Currently, the tester allows one client using the APP
# protocol, and up to two clients using either the HTTP and/or XML protocols.  Thus, the test suite can
# initialize five clients.  The clients remain dormant until a request is submitted by a client to the
# server.  Requests in the test suite specify which client they wish to simulate by the second character
# in the request. For example, a request to be submitted by the second HTTP client begins with H1:.  Each
# client can talk to the same server or to different servers.  The server is specified by the "URL"
# included in each test in the test suite (the actual form of the request submitted to the server is not
# quite the same as the test format as noted below).

# COOKIES
# The test suite supports cookies for HTTP clients.  The cookie returned from AIS is saved and included in
# all subsequent request headers.  All HTTP clients connected to the same server share the same cookie, much
# like multiple browsers open on the same client machine do in real life.  Note that multiple connections
# to the same client share the same session for each context.  HTTP shares a set of conflicting and
# contradictory specifications that box us into this unwieldy situation.

# CONTEXTS
# A single server may support more than one context (an instance of one application running on the server).
# A request may specify the context to be used.  If no context is specified, the request is submitted to
# the default context for this protocol.  A single connection (or a set of HTTP connections sharing the same
# cookie) may submit requests to two or more contexts.  Thus, one connection can support multiple simultaneous
# sessions to different contexts.  Further, each separate XML or AIS connection establishes a separate
# session to the same or different contexts.  But, multiple HTTP connections sharing the same cookie all
# share a single session.  Thus, it is not possible for an HTTP client to open two browsers each conducting
# a separate session to the same context (but they could open separate sessions to different clients).

# TESTS
# Tests specify a protocol, a connection (i.e., a client) followed by a request to be submitted to the
# server.  Since the tester supports up to two clients per protocol, the connection is either 0 or 1.
# The protocol is A for the APP (AIS proprietary) protocol, H for the HTTP GET protocol, P for the HTTP
# POST protocol, and X for the XML protocol. Use a TAB to separate the body of the POST from the request
# line. The format for each (followed by an example) is shown below:
#	 1.	An://host/req
#		A0://localhost/_ais|logon|user|tmay|passwd|tim
#	 2.	Hn://host/!contextname/path
#		H0://localhost/default.html
#	 3.	Pn://host/!contextname/amp.dll	body
#		P1://localhost/amp.dll	xml=<amp target="_ais" act="noop" />
#	 4.	X0://host/ampmsg
#		X0://localhost/_ais|logon|user|tmay|passwd|tim
# Where:
#	 1.	An for a request submitted by the APP client, n = connection number (must be 0)
#	 2.	Hn for an Http GET request, n = connection number (0 or 1)
#	 3. Pn for an HTTP POST request, n = connection number (0 or 1), body is body of request.
#	 4. Note that a tab separates the body from the request line.
#	 5.	X0 for an XML request
#	 6.	Host is the DNS name or IP address of AIS host.
#	 7. path is path relative to wwwroot of context.
#	 8. body is the body of the post request.
#	 9. ampmsg is name-value pairs separated by vertical bars. Ampmsg is converted into XML doc.

# NOTES
#	 1.	The vertical bar (|) matches a DEL (rubout) character
#	 2. An entry of the form |{nnn}... will repeat field nnn times.
#		For example, to expand the value of param1 to AbcAbcAbcAbc
#			testLambda|ampAct|param1|{4}Abc		
#	 3.	One or more expected responses must follow the request with no intervening blank lines
#		or comments in between. See requests listed below for examples.

# DEFINITIONS
# To define a variable that applies to all tests use:
#	#&var=value
# then "value" will be substituted for $var$ appearing any request or expected response.
# For example:
#	#&host=localhost
#	HO://$host$/default.html  becomes HO://localhost/default.html

# DIRECTIVES
#	 1. Include. To include another test file named testXXX.txt, add entry of the form:
#			#include testXXX.txt
#	 2. Stop. To terminate tests at the current line in the current file, add the entry:
#			#stop

# BUILT-IN COMMANDS
#	 1. Pause. To pause a sequence of tests, add the following "test". Press NextTest to
#		resume testing.
#			pause
#			Paused.
#	 2.	Timeout.  A default timeout of 1000 msec. occurs if the server does not return a
#		response in the time alloted.  The timeout can be reset at any point in the test
#		sequence by entering a "test" of the form:
#			timeout=msec
#			timeout reset
#		where msec is the number of msec. to wait.  If msec is 0, the timer is disabled.

# VARIABLES
# A variable definition may precede any expected response. It is of the form
#		&varname=RegExp
# where RegExp is a regular expression such as:
#		connectid\|(+d)
# RegExp is applied to the response from the current test to extract a value to be
# assigned to the varname.  RegExp must include one set of parentheses to identify
# the value to be assigned.  In the above example the number following connectid|
# is captured. Note that the vertical bar must be escaped; otherwise, it is treated as an
# OR operator in a regular expression.
# After a var is defined, an expected response or a request may include a variable
# reference of the form $varname$. Every instance of $varname$ in the request or
# the expected response is expanded to the value assigned to this variable. See the requests
# below for examples.

# RESPONSES
#	Here are all the possible responses to a test:
#	 1.	ABORT: Cancel current test
#		If NextTest button is pressed while test is pending, a dialog asks the user if user
#		wishes to continue waiting.  If no, this message appears and a new test is launched.
#	 2.	FAIL: Press NextTest to continue
#		One or more of the responses to a test did not match the expected response.  See
#		the section of Scenarios below for suggestions on handling this case.
#	 3.	Pass
#		All the expected responses to a test matched the returned responses.  If RunAll is set,
#		the next test is automatically launched; else, press NextTest to continue testing.
#	 4.	Paused: Press NextTest to continue - Encountered a pause request in test sequence. Press
#		NextTest button to continue.
#	 5.	Timeout: Press NextTest to continue
#		The timer fired before a expected response was returned.  You may need to fix the server,
#		modify the test, or extend the timeout period to avoid a timeout.
#	 6.	Timeout period updated
#		Encountered a timeout request in test sequence. Press NextTest to continue.
#	 7.	Unexpected Response
#		A response returned when no response was expected.  This can happen if the test suite
#		is missing an expected response or if a previous test was aborted and the delayed
#		response finally shows up.  If the tester has moved on to a new test before the
#		extra respose is returned, this response can cause the new test to fail. Either
#		increase the timeout setting or modify the test suite to avoid this problem.

# SCENARIOS
#	Start AIS in the server. Start testaisclient.  Open the test files in a text editor.
#	Run all the tests by checking the RunAll check-box and press NextTest. If a test fails,
#	the tester will stop.  Here are some things that you can do at this point.
#	 1.	Set Breakpoint. Add a pause directive just before the test that fails.  Restart
#		the tests. When the tester pauses, set a breakpoint in the server code and trace
#		the operation.
#	 2. Rerun Test.  Edit the request shown in the "Manual Test:" edit box and/or set
#		breakpoints in the code and press Submit to rerun the test. Or, just press LastTest
#		to rerun the test.
#	 3. Modify timeout.  Modify the timeout period by entering a timeout request in the
#		ManualTest edit box.  Enter "timeout=0" to disable the timer.
#	 4. Modify AIS code.  Stop the debugger, edit the code and recompile. Restart AIS.
#		In the tester, press Restart and then Clear to reconnect to AIS.
#	 5.	Modify Test Suite.  Edit request/expected responses in the text editor. Press Restart
#		and then Clear to restart with modified code.
#	 6. Check Discrepencies.  Check the showResults check-box.  Run the tests. Review each
#		response to make sure that a error did not sneak by.  Modify test to catch error in
#		the future.  Modify source code to fix error.
 	

# -------------------------------------------------------------------------------------------------
# Definitions
#&host=192.168.0.12
#&httpport=8084
#&appport=8081
#&xmlport=8080
#&logsysmsg=69
#&warnlvl=5

# Get a cookie
# &title matches 'Default Page'
H0://$host$:$httpport$/default.htm
&title=<TITLE>([^<]+)</
<HTML><HEAD><TITLE>$title$</TITLE></HEAD><BODY><H3>InvestByLambda.com - Default Page</H3>

# Open a second connection
H1://$host$:$httpport$/index.htm
<HTML><HEAD><TITLE>Index Page</TITLE></HEAD><BODY><H3>InvestByLambda.com - Index Page</H3>

P1://$host$:$httpport$/amp.dll	_ais
result|

# Decrease timeout from 1000 msec.
timeout=250
Decrease timeout for local server.

# --------------------------------------------- APP -----------------------------------------------
#include testappamp.tst

# -------------------------------------------- HTTP -----------------------------------------------
#include testhttpamp.tst
#include testhttpLambda.tst
###include testhttpxmlamp.tst

# -------------------------------------------- XML ------------------------------------------------
#include testxmlamp.tst
#include testxmlLambda.tst

# ---------------------------------------- DIRECTIVES ---------------------------------------------
#stop   - All lines below the stop directive are ignored
include - See above for include directive

# ------------------------------------ BUILT-IN COMMANDS ------------------------------------------
pause
Paused. Press NextTest to continue

timeout=0
Disable timer
# end

