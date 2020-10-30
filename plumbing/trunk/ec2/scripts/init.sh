#!/bin/bash
# This script initializes the instance with FIMCO's configuration.
# This script is designed to be run once, that is, immediately after 
# the instance is started.
# The following files are used by this script:
#   /ebs/admin/config_list.txt     - contains a list of files to be backed-up
#   /ebs/admin/config_fimco.tar.gz - contains FIMCO configuration files
#   /ebs/admin/install_nx_sbs.sh   - NX Small Business Server install script
#   /ebs/admin/nx_sbs/*.*          - NX Small Business Server files
#   /ebs/admin/update_motd.sh      - MOTD update script
#   /ebs/admin/restore.sh          - restoration script
#
# This script should be saved in /ebs/admin

echo "Saving original configuration..."
tar cvfz /ebs/admin/config_orig.tar.gz `cat /ebs/admin/config_list.txt`

echo "Stopping MySQL Server..."
/etc/init.d/mysql stop

echo "Applying FIMCO configuration..."
tar -x -C / -v -z -f /ebs/admin/config_fimco.tar.gz

echo "Starting MySQL Server..."
/etc/init.d/mysql start

echo "Installing NX Small Business Server..."
/ebs/admin/install_nx_sbs.sh

echo "Installing cron job to fix /etc/motd..."
# the /etc/motd file is updated every time the system is rebooted
# to make the changes permanent we will install a cron job
# which will take care of adding the contents we want
cp /ebs/admin/update_motd.sh /etc/cron.daily
chmod +x /etc/cron.daily/update_motd.sh
/etc/cron.daily/update_motd.sh

echo "Copying restoration script to /mnt..."
cp /ebs/admin/restore.sh /mnt/

echo "Initialization complete!"
exit 0

