#!/bin/bash
# This script will install NoMachine NX Free Server.
# Any existing NX installation will be removed.
# This script can be called from start.sh

echo "Removing previous installation of NX..."
dpkg --purge nxserver nxnode nxclient

echo "Removing NX install directory..."
rm -rf /usr/NX

if [ ! -d /ebs/admin/nx_free ]
then
    echo "/ebs/admin/nx_free does not exist. Please create the said directory and put the NX install packages in it."
    exit 1
fi

echo "Installing NX Free Server..."
dpkg -i /ebs/admin/nx_free/nx*.deb

echo "Installation complete!"
exit 0

