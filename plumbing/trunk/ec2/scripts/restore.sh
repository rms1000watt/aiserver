#!/bin/bash
# This script restores the instace to it's original configuration.
# This script removes FIMCO related configuration.
# This is normally used when bundling the AMI.
# The script need not be run before terminating the instance.
# The following files are used by this script:
#   /ebs/admin/config_orig.tar.gz - contains the original configuration files
#   /ebs/admin/install_nx_free.sh - NX Free Server install script
#   /ebs/admin/nx_free/*.*        - NX Free Server files
#
# This script should be saved in /ebs/admin and copied to /mnt by init.sh

INSTANCE_ID=`curl http://169.254.169.254/latest/meta-data/instance-id 2> /dev/null`

echo "Stopping MySQL Server..."
/etc/init.d/mysql stop

echo "Installing NX Free Server..."
/ebs/admin/install_nx_free.sh

echo "Reverting to original configuration..."
tar -x -C / -v -z -f /ebs/admin/config_orig.tar.gz

echo "Starting MySQL Server..."
/etc/init.d/mysql start

echo "Removing cron job..."
rm -f /etc/cron.daily/update_motd.sh

export EC2_CERT=/mnt/cert_fimco.pem
export EC2_PRIVATE_KEY=/mnt/pk-fimco.pem

mount | cut -d' ' -f3 | grep -w "^/ebs$"
if [ $? -eq 0 ]
then
	echo "Unmounting /ebs..."
    umount /ebs
fi

mount | cut -d' ' -f3 | grep -w "^/home$"
if [ $? -eq 0 ]
then
	echo "Unmounting /home..."
    umount /home
fi

VOLUMES=`ec2-describe-volumes | grep $INSTANCE_ID | cut -f2`

for VOLUME in $VOLUMES
do
	echo "Detaching volume $VOLUME..."
	ec2-detach-volume $VOLUME
done

rm -f $EC2_CERT
rm -f $EC2_PRIVATE_KEY

echo "Restoration complete!"
exit 0

