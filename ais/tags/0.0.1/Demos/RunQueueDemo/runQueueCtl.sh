#!/bin/bash
#
# Usage: $0 -p jobqueue_path -t template_path [-n num_instances] [-m memory_size] [-r prefix]
# jobqueue_path   : specifies the path to the jobqueue path (relative to the instance path)
# template_path   : specifies the path to the template (contains AStartup.sl and context.ini)
# num_instances   : specifies the no. of instances to generate
# memory_size     : specifies the amount of memory for each context
# prefix          : specifies the prefix for the context name

# context.ini
# ContextName = 
# Memory = 
# DefaultJitMode = on

# AStartup.sl
# (runScript "/usr/share/ais/Libraries/BrowseLib/BrowseLib.sl")            ;; for binary packages
# (runScript (append _ais.installPath "Libraries/BrowseLib/BrowseLib.sl")) ;; for source packages
# (browseLib aKey aExtentFileName aExtentLocation aExtentStorageScope aImportSync aExportSync aAutoCompile aForceSetting)

# The following command tells RunQueue to start processing jobs in the JobQueue
# (runQueue.run)

function usage()
{
	echo "Usage: $0 -p jobqueue_path -t template_path [-n num_instances] [-m memory_size] [-r prefix]"
}

declare -i NUM_INSTANCES=1
declare -i MEMORY_SIZE=512
JOBQUEUE_PATH=""
TEMPLATE_PATH=""
PREFIX="RunQueue"

while getopts ":n:m:p:t:r:" opt; do
	case $opt in
		n) NUM_INSTANCES=$OPTARG ;;
		m) MEMORY_SIZE=$OPTARG ;;
		p) JOBQUEUE_PATH=$OPTARG ;;
		t) TEMPLATE_PATH=$OPTARG ;;
        r) PREFIX=$OPTARG ;;
		\?) usage
			exit 1 ;;
	esac
done

if [ -z "$JOBQUEUE_PATH" ]
then
	echo "$0: Please specify a valid jobqueue_path"
	usage
	exit 1
fi

if [ -z "$TEMPLATE_PATH" -o ! -d "$TEMPLATE_PATH" ]
then
	echo "$0: Please specify a valid template_path"
	usage
	exit 1
fi

if [ $NUM_INSTANCES -le 0 ]
then
	echo "$0: Please specify valid num_instances"
	usage
	exit 1
fi

if [ $MEMORY_SIZE -le 0 ]
then
	echo "$0: Please specifiy valid memory_size"
	usage
	exit 1
fi

CURRENT_INDEX=1

while [ $CURRENT_INDEX -le $NUM_INSTANCES ]
do
	CURRENT_DIR="$PREFIX"_"$CURRENT_INDEX"
	if [ -e "$CURRENT_DIR" ]
	then
        # directory already exists
		echo "$0: Directory ($CURRENT_DIR) already exists, skipping"
	else
        echo "Creating directory ($CURRENT_DIR)..."
		mkdir -p "$CURRENT_DIR"

		echo "Copying files from template..."
		cp -r "$TEMPLATE_PATH"/* "$CURRENT_DIR"
		
		cd "$CURRENT_DIR"
		# replace ContextName and Memory parameters
		cat context.ini | sed -e "s/^[cC][oO][nN][tT][eE][xX][tT][nN][aA][mM][eE][\ ]*=.*/ContextName = $CURRENT_DIR/" -e "s/^[mM][eE][mM][oO][rR][yY][\ ]*=.*/Memory = $MEMORY_SIZE/" > context.tmp
		mv context.tmp context.ini

		# create symbolic link to JobQueue
		if [ ! -d "$JOBQUEUE_PATH" ]
		then
			echo "Warning: Directory ($JOBQUEUE_PATH) does not exist"
		else
			echo "Creating symbolic link to jobqueue..."
			ln -s "$JOBQUEUE_PATH" JobQueue
		fi

		# return to top-level directory
		cd ..
	fi

	# move to the next instance
	CURRENT_INDEX=$(($CURRENT_INDEX + 1))
done

exit 0

