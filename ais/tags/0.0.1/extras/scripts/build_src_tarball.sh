#!/bin/bash
# Author: Franklin Chua <franklin.chua@gmail.com>
# This script creates a source tarball from the SVN repository.
# Usage: $0 [x.y.z]

# svn export https://aiserver.svn.sourceforge.net/svnroot/aiserver/ais/trunk

if [ $# -gt 1 ]
then
  BASE_VERSION="$1"
else
  BASE_VERSION="head"
fi

AIS_VERSION="ais-$BASE_VERSION"

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

if [ $BASE_VERSION = "head" ]
then
  svn export https://aiserver.svn.sourceforge.net/svnroot/aiserver/ais/trunk $AIS_VERSION
else
  svn export https://aiserver.svn.sourceforge.net/svnroot/aiserver/ais/tags/$BASE_VERSION $AIS_VERSION
fi

rm -rf $AIS_VERSION/docs/onlinedocs

svn export https://aiserver.svn.sourceforge.net/svnroot/aiserver/aisdocs/trunk/onlinedocs $AIS_VERSION/docs/onlinedocs

tar cvfz "$AIS_VERSION.tar.gz" "$AIS_VERSION"

rm -rf $AIS_VERSION

exit 0

