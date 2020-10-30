#!/bin/bash
# Author: Franklin Chua <franklin.chua@gmail.com>
# This script creates a source tarball from the SVN repository.
# Usage: $0 [x.y.z]

# svn export https://aiserver.svn.sourceforge.net/svnroot/aiserver/ais/trunk

if [ $# -gt 1 ]
then
  AIS_VERSION="ais-$1"
else
  AIS_VERSION="ais-0.0.1"
fi

# In actual practice, there should be a snapshot of the trunk under 'tags'.
# For example:
#     tags/ais-0.0.1
#     tags/ais-0.0.2
#     tags/ais-0.0.3
#
# We can get a copy of that snapshot we the following command:
# svn export https://aiserver.svn.sourceforge.net/svnroot/aiserver/ais/tags/$AIS_VERSION
#
# For now, we will use this command:
# svn export https://aiserver.svn.sourceforge.net/svnroot/aiserver/ais/trunk $AIS_VERSION

rm -rf $AIS_VERSION

svn export https://aiserver.svn.sourceforge.net/svnroot/aiserver/ais/trunk $AIS_VERSION

tar cvfz "$AIS_VERSION.tar.gz" "$AIS_VERSION"

rm -rf $AIS_VERSION

exit 0

