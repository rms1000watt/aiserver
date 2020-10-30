#!/bin/bash
# Author: Franklin Chua <franklin.chua@gmail.com>
# This script prepares the AIS files for Build Service (RPM)
# Usage: $0 [x.y.z]

# Additional Requirements:
#   The %changelog entries in ais.spec and ais-static.spec should be updated.

# osc co home:aiserver:rpm:ais
# osc co home:aiserver:rpm:ais-static

if [ $# -gt 0 ]
then
  VERSION="$1"
else
  VERSION="0.0.1"
fi

# prepare osc directories
echo "Retrieving Build Service Directories..."
if [ -d "home:aiserver:rpm/ais" ]
then
  osc up "home:aiserver:rpm/ais"
else
  osc co home:aiserver:rpm ais
fi

if [ -d "home:aiserver:rpm/ais-static" ]
then
  osc up "home:aiserver:rpm/ais-static"
else
  osc co home:aiserver:rpm ais-static
fi

echo "Preparing RPM Specification files..."
cat ../buildservice/rpm/ais.spec | sed -e "s/x.x.x/$VERSION/g" > "home:aiserver:rpm/ais/ais.spec"
cat ../buildservice/rpm/ais-static.spec | sed -e "s/x.x.x/$VERSION/g" > "home:aiserver:rpm/ais-static/ais.spec"

echo "Copying Source Tarball..."
cp -f "ais-$VERSION.tar.gz" "home:aiserver:rpm/ais"
cp -f "ais-$VERSION.tar.gz" "home:aiserver:rpm/ais-static"

echo "Done."

echo "The following directories have been created/updated:"
echo "  home:aiserver:rpm/ais"
echo "  home:aiserver:rpm/ais-static"
echo ""
echo "You may now commit the modified files using the following command:"
echo "  osc ci"
echo ""
echo "If you have not added the files, please do so by running the following command:"
echo "  osc add *"

exit 0

