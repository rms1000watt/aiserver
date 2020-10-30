#!/bin/bash
# This is script creates a backup of FIMCO configuration files.
# This script should be run whenever there's a modification in
# FIMCO configuration files in the running instance.

if [ -f /ebs/admin/config_fimco.tar.gz ]
then
	echo "Renaming last backup..."
	mv -f /ebs/admin/config_fimco.tar.gz /ebs/admin/config_fimco.old.tar.gz
fi

echo "Saving FIMCO configuration..."
tar cvfz /ebs/admin/config_fimco.tar.gz `cat /ebs/admin/config_list.txt`

exit 0

