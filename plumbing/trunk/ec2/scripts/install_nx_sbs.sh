#!/bin/bash
# This script will install NoMachine NX Small Business Server.
# Any existing NX installation will be removed.
# This script can be called from start.sh

echo "Removing previous installation of NX..."
dpkg --purge nxserver nxnode nxclient

echo "Removing NX install directory..."
rm -rf /usr/NX

if [ ! -d /ebs/admin/nx_sbs ]
then
    echo "/ebs/admin/nx_sbs does not exist. Please create the said directory and put the NX install packages in it."
    exit 1
fi

echo "Installing NX Small Business Server..."
dpkg -i /ebs/admin/nx_sbs/nx*.deb

cd /usr/NX/etc

echo "Renaming NX Node license file..."
if [ -f node.lic ]
then
    mv node.lic node.lic.eval
fi

echo "Renaming NX Server license file..."
if [ -f server.lic ]
then
    mv server.lic server.lic.eval
fi

echo "Installing FIMCO license files..."
tar xvfz /ebs/admin/nx_sbs/key.tar.gz

echo "Setting permissions..."
chmod 0400 node.lic server.lic
chown nx:root node.lic server.lic

echo "Installation complete!"
exit 0

