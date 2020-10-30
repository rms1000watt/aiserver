# ais/testaisclient/testhttpamp.txt

# Tests of APP-port built-in commands
# CHANGE HISTORY
# Version	Date		Who		Change
# 1.0104	9/8/2006	tlw		Add httpsid0 and httsid1 to distinguish sessions on H0 from P1.
# 1.0039	7/12/2004	tlw		Comment out the logon.

# NOTES
#	 1.	See testsuite.tst for more info on constructing tests.
# -------------------------------------------------------------------------------------------------

# logon - Comment out to test the autologon feature
H0://$host$:$httpport$/amp.dll?_ais=logon&user=tmay&passwd=tim
&httpconnectid=connectid\|(\d+)
&httpusrid=usrid\|(\d+)
connectid|$httpconnectid$|usrid|$httpusrid$
# -------------------------------------------------------------------------------------------------

# OpenSession
H0://$host$:$httpport$/amp.dll?_ais=opensession&context=TestAis&userid=$httpusrid$
&httpsid0=(\d+)
$httpsid0$
# -------------------------------------------------------------------------------------------------

# probe
H0://$host$:$httpport$/amp.dll?
result|
# -------------------------------------------------------------------------------------------------

# probe
H0://$host$:$httpport$/amp.dll?_ais
result|
# -------------------------------------------------------------------------------------------------

# noop
H0://$host$:$httpport$/amp.dll?_ais=noop
sessionid=$httpsid0$
# -------------------------------------------------------------------------------------------------

# (noop)
H0://$host$:$httpport$/amp.dll?_eval=%28noop%29
true
# -------------------------------------------------------------------------------------------------

# eval
# Uncomment to start a long-running task
#H0://$host$:$httpport$/amp.dll?_eval=%28sleepy+120%29
#true
# -------------------------------------------------------------------------------------------------

# eval
H0://$host$:$httpport$/amp.dll?_ais=eval&exp=%28writeln+{Hello from query string}%29
true
# -------------------------------------------------------------------------------------------------

# AisLisp  (+ 1 1)
H0://$host$:$httpport$/amp.dll?_ais=eval&exp=%28%2B+1+1%29
2
# -------------------------------------------------------------------------------------------------

# Probe.
P1://$host$:$httpport$/amp.dll	_ais
result|

# eval Url-encoded post.
P1://$host$:$httpport$/amp.dll	_eval=%28writeln+{Hello+from+query+string}%29
true
# -------------------------------------------------------------------------------------------------

# eval Plain text post.
P1://$host$:$httpport$/amp.dll	_ais=eval&exp=(writeln {Hello from query string})
true
# -------------------------------------------------------------------------------------------------

# xml - Url-encoded query.    <amp target="_ais" act="noop"/>
H0://$host$:$httpport$/amp.dll?xml=%3Camp+target%3D%22_ais%22+act%3D%22noop%22%2F%3E
<amp act="noop" status="0" target="_ais" xtype="return"><result>sessionid=$httpsid0$</result>
# -------------------------------------------------------------------------------------------------

# OpenSession
P1://$host$:$httpport$/amp.dll?	_ais=opensession&context=TestAis&userid=$httpusrid$
&httpsid1=(\d+)
$httpsid1$

# -------------------------------------------------------------------------------------------------
# xml - Plain text Post. Content-Type text/xml.  <amp target="_ais" act="noop"/>
P1://$host$:$httpport$/amp.dll	<amp target="_ais" act="noop"/>
<amp act="noop" status="0" target="_ais" xtype="return"><result>sessionid=$httpsid1$</result>
# -------------------------------------------------------------------------------------------------

# xml - Plain text Post.
P1://$host$:$httpport$/amp.dll	xml=<amp target="_ais" act="noop"/>
<amp act="noop" status="0" target="_ais" xtype="return"><result>sessionid=$httpsid1$</result>
# -------------------------------------------------------------------------------------------------

# OpenConsoleLog
P1://$host$:$httpport$/amp.dll	<amp target="_ais" act="openconsolelog"><clear>true</clear><redirect>true</redirect><sessionid>0</sessionid><size>1000</size></amp>
<amp act="openconsolelog" status="0" target="_ais" xtype="return"><result>true
# -------------------------------------------------------------------------------------------------

# Console Output
P1://$host$:$httpport$/amp.dll	<amp target="_ais" act="eval"><exp>(display {Cons})</exp></amp>
amp act="eval" status="0" target="_ais" xtype="return"><result>true</result>
# -------------------------------------------------------------------------------------------------

# GetConsoleLog
P1://$host$:$httpport$/amp.dll	<amp target="_ais" act="getconsolelog"><clear>true</clear><sessionid>0</sessionid><wait>true</wait></amp>
<amp act="getconsolelog" status="0" target="_ais" xtype="return"><Display reqid=
# -------------------------------------------------------------------------------------------------

# GetConsoleLog
P1://$host$:$httpport$/amp.dll	<amp target="_ais" act="getconsolelog"/>
<amp act="getconsolelog" status="0" target="_ais" xtype="return">
# -------------------------------------------------------------------------------------------------

# Console Output
P1://$host$:$httpport$/amp.dll	<amp target="_ais" act="eval"><exp>(display {More console output})</exp></amp>
<amp act="eval" status="0" target="_ais" xtype="return"><result>true</result>
# -------------------------------------------------------------------------------------------------

# GetConsoleLog
P1://$host$:$httpport$/amp.dll	<amp target="_ais" act="getconsolelog"/>
<amp act="getconsolelog" status="0" target="_ais" xtype="return"><Display reqid=
# -------------------------------------------------------------------------------------------------

# EnableConsoleLog - close the log
P1://$host$:$httpport$/amp.dll	<amp target="_ais" act="enableconsolelog"><enable>close</enable><sessionid>0</sessionid></amp>
<amp act="enableconsolelog" status="0" target="_ais" xtype="return"><result>true
# -------------------------------------------------------------------------------------------------

# Other built-in AMP functions are tested in testxmlamp.txt
# -------------------------------------------------------------------------------------------------


