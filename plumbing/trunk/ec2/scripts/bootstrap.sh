#!/bin/bash
# EC2 instance bootstrap script
# This script will automated initialization of the EC2 instance.
# The script should be passed as a parameter to ec2-run-instances.
#
#   ec2-run-instances [AMI] -k [KEYPAIR] -f bootstrap.sh -t [MACHINE_TYPE] -z [AVAILABILITY_ZONE]
#
#   ec2-run-instances ami-9759bffe -k gsg-keypair -f bootstrap.sh -t m1.large -z us-east-1a
#
# The EBS_VOLUME variable must be set to the correct EBS Volume ID to be associated with
# the instance to be run.

EBS_VOLUME="vol-b1e20bd8"
#EBS_VOLUME="vol-191cf070"
INSTANCE_ID=`curl http://169.254.169.254/latest/meta-data/instance-id 2> /dev/null`

export JAVA_HOME=/usr
export EC2_HOME=/usr/local/ec2/apitools
export EC2_PRIVATE_KEY=/mnt/pk_fimco.pem
export EC2_CERT=/mnt/cert_fimco.pem
PATH=/usr/local/ec2/apitools/bin:$PATH

if [ -z $EBS_VOLUME ]
then
    echo "EBS_VOLUME is not defined"	
    exit 1
fi

# create the certificate
LINE_BEG=`cat $0 | grep -n "BEGIN CERTIFICATE" | tail -1 | cut -d':' -f1`
LINE_END=`cat $0 | grep -n "END CERTIFICATE" | tail -1 | cut -d':' -f1`
cat $0 | sed -n "$LINE_BEG,$LINE_END p" > $EC2_CERT

# create the private key
LINE_BEG=`cat $0 | grep -n "BEGIN PRIVATE KEY" | tail -1 | cut -d':' -f1`
LINE_END=`cat $0 | grep -n "END PRIVATE KEY" | tail -1 | cut -d':' -f1`
cat $0 | sed -n "$LINE_BEG,$LINE_END p" > $EC2_PRIVATE_KEY

# get the status of the volume
echo "Checking volume status..."
VOLUME_STAT=`ec2-describe-volumes $EBS_VOLUME | cut -f6`

# check if the volume is available
if [ $VOLUME_STAT = "available" ]
then
    echo "Volume is available"
elif [ $VOLUME_STAT = "in-use" ]
then
    echo "Volume is already in-use."
    exit 2
else
    echo "Volume does not exist."
    exit 2
fi

# attach the volume
echo "Attaching volume..."
VOLUME_STAT=`ec2-attach-volume $EBS_VOLUME -i $INSTANCE_ID -d /dev/sdh 2> /dev/null | cut -f5`
RETRIES=0
while [ ! $VOLUME_STAT = "attached" ]
do
    if [ $RETRIES -ge 12 ]
    then
        echo "Volume is taking too long to attach."
        exit 3
    fi

    # sleep 5 seconds
    sleep 5
    VOLUME_STAT=`ec2-describe-volumes $EBS_VOLUME 2> /dev/null | grep ATTACHMENT | cut -f5`

    # increment no. of retries
    RETRIES=`expr $RETRIES + 1`
done

echo "Volume attached."

# mount volume to our known mount points
echo "Creating /ebs directory"
mkdir -p /ebs

echo "Mounting /dev/sdh1 to /ebs... this might take a few minutes"
mount /dev/sdh1 /ebs

echo "Mounting /dev/sdh2 to /home... this might take a few minutes"
mount /dev/sdh2 /home

# run initialization script if exists
if [ -f /ebs/admin/init.sh ] && [ -x /ebs/admin/init.sh ]
then
    echo "Running initialization script..."
    /ebs/admin/init.sh
fi

echo "Copying bootstrap script to /mnt..."
cp -f $0 /mnt/bootstrap.sh

echo "*** BOOTSTRAP COMPLETE ***"
exit 0

-----BEGIN CERTIFICATE-----
MIICdzCCAeCgAwIBAgIGAPF9w/+bMA0GCSqGSIb3DQEBBQUAMFMxCzAJBgNVBAYT
AlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMQwwCgYDVQQLEwNBV1MxITAfBgNVBAMT
GEFXUyBMaW1pdGVkLUFzc3VyYW5jZSBDQTAeFw0wOTAyMTgxNTA1NDBaFw0xMDAy
MTgxNTA1NDBaMFIxCzAJBgNVBAYTAlVTMRMwEQYDVQQKEwpBbWF6b24uY29tMRcw
FQYDVQQLEw5BV1MtRGV2ZWxvcGVyczEVMBMGA1UEAxMMeW54NnkxZnRnMmRuMIGf
MA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCKh70DtYaR7DPWJKUfhxNLeOX35bhm
i8qQgvrGojWguFBDxsN7ydQkERvkTXmBh+xAnCWak985RZ7+KmjDLbXDvPJQ1MSj
j+rHiHSL6o7BvXqW44T7ZiFtCOVo6BfjUTh3kgmH22buJBr3xMmXVksd50TX25wr
uQ7aieN+zpDaqQIDAQABo1cwVTAOBgNVHQ8BAf8EBAMCBaAwFgYDVR0lAQH/BAww
CgYIKwYBBQUHAwIwDAYDVR0TAQH/BAIwADAdBgNVHQ4EFgQUOoFCixofnTyXgwXV
SeCHSD4oyHswDQYJKoZIhvcNAQEFBQADgYEAn3qp14qeJvw9xFvfGIV62i8SNMc3
/i60bgpryVpD1A9W4/gaC435IrDX1fY7w5vPbxYI1Dnjn1HufsAOAJii/GNJ+XcN
G/tmf5G90J8Wr8d0yBmygK/IubF+ulBa3ewBB3QWehN6Jjn3xRQm6QzAQKdlZ9J0
EtiP9Em/h+Dxo9o=
-----END CERTIFICATE-----

-----BEGIN PRIVATE KEY-----
MIICdQIBADANBgkqhkiG9w0BAQEFAASCAl8wggJbAgEAAoGBAIqHvQO1hpHsM9YkpR+HE0t45ffl
uGaLypCC+saiNaC4UEPGw3vJ1CQRG+RNeYGH7ECcJZqT3zlFnv4qaMMttcO88lDUxKOP6seIdIvq
jsG9epbjhPtmIW0I5WjoF+NROHeSCYfbZu4kGvfEyZdWSx3nRNfbnCu5DtqJ437OkNqpAgMBAAEC
gYAmA5maqvWClY6j9Opa/HYO/94baK5xdWrgvRCT8W9F604bSy/ZiEjunMNKovf005fBIxgukuVu
kexPUtPsu15lMCovU51QoAEaovX0Kh3hxLmbLT+g0W+wQNDoczER/4mtwV0WziRC64koJC+4whkC
Sf/GaNUCXz9KoZjjnsxNAQJBANNJnzVOUfk9bHtsdvtO48q84UBInbLuPOCrxDHa37u0ZxMuo3V4
e8t/VyHJbjHxcE7NBzt2HtE94XoqXV3ChdcCQQCn2Ie/Kxkki7lI7Gvv51Pjxl2gHwND0a8C2zg9
nmupKiN4Us3rAyhT4X8xsWZiDC2NgZpL8YGvTwKdVqo2upN/AkBIYx/FjoitIGsrOfTlkpieW+m8
MWS96bs3qgF0py0hzOPHgaIE2/tls8HxVGaJe9NjXAEPUR+rxkyaoysLtVpPAkAEKWlMQyxPbKt+
dGZEv46j8jI2Gy7Air11K6xcUsZGnoXcoOj8L8rbMZcuy0BHpBepD5Kc2XMmvqXI8vIrgzrPAkBr
IEEvjWSL7YXkTFmG9ryn0/TqbbmUzVK1k/h2kvaW4ZQC5m7KZX/0L38jqv2gBPVVIV/ERN2Erkz3
0ESBDVPY
-----END PRIVATE KEY-----
