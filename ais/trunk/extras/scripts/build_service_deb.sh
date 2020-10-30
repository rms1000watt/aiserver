#!/bin/bash
# Author: Franklin Chua <franklin.chua@gmail.com>
# This script prepares the AIS files for Build Service (DEB)
# Usage: $0 [x.y.z]

# Additional Requirements:
#   The ais.changes and ais-static.changes should be updated.

# osc co home:aiserver:deb:ais
# osc co home:aiserver:deb:ais-static

if [ $# -gt 0 ]
then
  VERSION="$1"
else
  VERSION="0.0.1"
fi

# prepare osc directories
echo "Retrieving Build Service Directories..."
if [ -d "home:aiserver:deb/ais" ]
then
  osc up "home:aiserver:deb/ais"
else
  osc co home:aiserver:deb ais
fi

if [ -d "home:aiserver:deb/ais-static" ]
then
  osc up "home:aiserver:deb/ais-static"
else
  osc co home:aiserver:deb ais-static
fi

echo "Preparing DEB Specification files..."
cat ../buildservice/deb/ais/ais.dsc | sed -e "s/x.x.x/$VERSION/g" > "home:aiserver:deb/ais/ais.dsc"
cat ../buildservice/deb/ais/ais.changes | sed -e "s/x.x.x/$VERSION/g" > "home:aiserver:deb/ais/ais.changes"
cat ../buildservice/deb/ais-static/ais-static.dsc | sed -e "s/x.x.x/$VERSION/g" > "home:aiserver:deb/ais-static/ais-static.dsc"
cat ../buildservice/deb/ais-static/ais-static.changes | sed -e "s/x.x.x/$VERSION/g" > "home:aiserver:deb/ais-static/ais-static.changes"
cp ../buildservice/deb/ais/debian.* "home:aiserver:deb/ais"
cp ../buildservice/deb/ais-static/debian.* "home:aiserver:deb/ais-static"

echo "Copying Source Tarball..."
cp -f "ais-$VERSION.tar.gz" "home:aiserver:deb/ais"
cp -f "ais-$VERSION.tar.gz" "home:aiserver:deb/ais-static"

echo "Done."

echo "The following directories have been created/updated:"
echo "  home:aiserver:deb/ais"
echo "  home:aiserver:deb/ais-static"
echo ""
echo "You may now commit the modified files using the following command:"
echo "  osc ci"
echo ""
echo "If you have not added the files, please do so by running the following command:"
echo "  osc add *"

exit 0

