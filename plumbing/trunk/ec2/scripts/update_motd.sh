#!/bin/bash
# This script updates /etc/motd to reflect the versions
# of the tools installed in the system
#
# This script should be saved in /ebs/admin
# and copied to /etc/cron.daily in by init.sh

NEW=/var/run/motd.new
REAL=/var/run/motd
SKEL=/etc/motd.tail

source /etc/profile

SVN_VERSION=`svn --version | head -1 | cut -d' ' -f3,4`
QT_VERSION=`qmake --version | tail -1 | cut -d' ' -f4`
MYSQL_VERSION=`mysql --version | cut -d',' -f1 | cut -d' ' -f4-6`

# quick and dirty way to extract version of eclipse
ECLIPSE_DIR=`whereis eclipse | cut -d' ' -f2`
ECLIPSE_VERSION=`cat $ECLIPSE_DIR/readme/readme_eclipse.html | grep "<title>" | cut -d' ' -f5 | cut -d'<' -f1`

EC2_API_VERSION=`ec2-version`
EC2_AMI_VERSION=`ec2-ami-tools-version | head -n1`

uname -snrvm > $NEW
[ -f $SKEL ] && cat $SKEL >> $NEW

echo "****************************************************************************" >> $NEW
echo "To access AIS Analytic Information Server documentation, please visit:" >> $NEW
echo "http://aiserver.wiki.sourceforge.net/" >> $NEW
echo "" >> $NEW
echo "The AIS home page can be found at:" >> $NEW
echo "http://aiserver.sourceforge.net/" >> $NEW
echo "" >> $NEW
echo "The AIS project page can be found at:" >> $NEW
echo "http://sourceforge.net/projects/aiserver/" >> $NEW
echo "" >> $NEW
echo "VERSIONS" >> $NEW
echo "Subversion:    " $SVN_VERSION >> $NEW
echo "Trolltech Qt:  " $QT_VERSION >> $NEW
echo "MySQL:         " $MYSQL_VERSION >> $NEW
echo "Eclipse:       " $ECLIPSE_VERSION >> $NEW
echo "EC2 API Tools: " $EC2_API_VERSION >> $NEW
echo "EC2 AMI Tools: " $EC2_AMI_VERSION >> $NEW
echo "****************************************************************************" >> $NEW

mv -f $NEW $REAL

