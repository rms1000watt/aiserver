# ais/testaisclient/testxmlhttpamp.txt

# Tests using XML documents via a HTTP port
# NOTES
#	 1.	See testsuite.tst for more info on constructing tests.

# CHANGE HISTORY (put latest entry at top)
# Version	Date		Who		Change
# 1.0104	9/8/2006	tlw		Fix error in protocol for (noop).
# 1.0062	5/17/2005	tlw		Test Http port using XML
# -------------------------------------------------------------------------------------------------

# probe
H0://$host$:$httpport$/amp.dll?
result|
# -------------------------------------------------------------------------------------------------

# (noop)
H0://$host$:$httpport$/amp.dll?_eval=%28noop%29
&httpsessionid=(\d+)
sessionid=$httpsessionid$
# -------------------------------------------------------------------------------------------------

# Probe. Empty post.
P1://$host$:$httpport$/amp.dll	
result|

# eval Url-encoded post.   _eval=(writeln {Hello from query string})   
P1://$host$:$httpport$/amp.dll	_eval=%28writeln+{Hello+from+query+string}%29
true
# -------------------------------------------------------------------------------------------------

# eval Plain text post.   _eval=(writeln {Hello from query string})   
P1://$host$:$httpport$/amp.dll	_eval=(writeln {Hello from query string})
true
# -------------------------------------------------------------------------------------------------

# xml - Url-encoded query.    <amp target="_ais" act="noop"/>
H0://$host$:$httpport$/amp.dll?xml=%3Camp+target%3D%22_ais%22+act%3D%22noop%22%2F%3E
<amp act="noop" status="0" target="_ais" xtype="return"><result>sessionid=$httpsessionid$</result>
# -------------------------------------------------------------------------------------------------

# xml - Plain text Post. Content-Type text/xml.  <amp target="_ais" act="noop"/>
P1://$host$:$httpport$/amp.dll	<amp target="_ais" act="noop"/>
<amp act="noop" status="0" target="_ais" xtype="return"><result>sessionid=$httpsessionid$</result>
# -------------------------------------------------------------------------------------------------

# xml - Plain text Post.  xml=<amp target="_ais" act="noop"/>
P1://$host$:$httpport$/amp.dll	xml=<amp target="_ais" act="noop"/>
<amp act="noop" status="0" target="_ais" xtype="return"><result>sessionid=$httpsessionid$</result>
# -------------------------------------------------------------------------------------------------


# All built-in AMP functions are tested in testxmlamp.txt
# -------------------------------------------------------------------------------------------------

