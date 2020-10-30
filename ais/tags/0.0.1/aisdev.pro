# /**********************************************************************************
# Copyright (C) 2008 Investment Science Corp.
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
# ***********************************************************************************/
include(aisdev.prf)

SUBDIRS += glue \
    jit \
    afilewatcher \
    aforms \
    ahttpsvr \
    aismgr \
    aissvr \
    alogmgr \
    appclient \
    appsvr \
    asessionmgr \
    asysmgr \
    atextedit \
    ausrmgr \
    autilities \
    axmlsvr \
    smtbase \
    webide \
    testaisclient \
    aissvc \
    ride \
    aised

win32 {
	SUBDIRS += asvcmgr
}

INCLUDEPATH += include
DESTDIR = .
TEMPLATE = subdirs

CONFIG += warn_on build_all debug_and_release 
win32 {
	# we will try to determine the location of the MySQL Error Message file
	# The default install locations are:
	# C:\Program Files\MySQL\MySQL Server 5.1\share\english - Windows XP 32-bit, Windows Vista 32-bit
	# C:\Program Files (x86)\MySQL\MySQL Server 5.1\share\english - Windows XP 64-bit
	MYSQL_LANG_DIR = "C:\Program Files (x86)\MySQL\MySQL Server 5.1\share\english"
	MYSQL_LANG_FILE = "$$MYSQL_LANG_DIR\errmsg.sys"

    # we may have to update the errmsg.sys which we are using
    AIS_LANG_DIR = "mysqlmsgs"
    AIS_LANG_FILE = "$$AIS_LANG_DIR\errmsg.sys"	
	
	exists($$MYSQL_LANG_FILE) {
		# copy it
		message("copying from $$MYSQL_LANG_FILE .")
		system("copy \"$$MYSQL_LANG_FILE\" \"$$AIS_LANG_FILE\"")
	} else {
		# try the other location
		MYSQL_LANG_DIR = "C:\Program Files\MySQL\MySQL Server 5.1\share\english"
		MYSQL_LANG_FILE = "$$MYSQL_LANG_DIR\errmsg.sys"
		exists($$MYSQL_LANG_FILE) {
			# copy it
			message("copying from $$MYSQL_LANG_FILE .")
			system("copy \"$$MYSQL_LANG_FILE\" \"$$AIS_LANG_FILE\"")	
		}
	}
}
unix {
    # we will try to determine the location of the MySQL Error Message file
    MYSQL_ROOT_DIR = $$system("which mysql_config | sed -e \"s@/bin/mysql_config@@\"")
    MYSQL_LANG_DIR = "$$MYSQL_ROOT_DIR/share/mysql/english"
    MYSQL_LANG_FILE = "$$MYSQL_LANG_DIR/errmsg.sys"

    # we may have to update the errmsg.sys which we are using
    AIS_LANG_DIR = "mysqlmsgs"
    AIS_LANG_FILE = "$$AIS_LANG_DIR/errmsg.sys"

    exists($$AIS_LANG_FILE) {
        # the language file exists, compare them first
        system("diff \"$$MYSQL_LANG_FILE\" $$AIS_LANG_FILE") {
            # we already have the correct version, nothing else to do
            message("$$AIS_LANG_FILE is up-to-date.")
        } else {
            # copy the installed errmsg.sys
            message("$$AIS_LANG_FILE is out-of-date, copying from $$MYSQL_LANG_FILE .")
            system("mkdir -p \"$$AIS_LANG_DIR\"")
            system("cp \"$$MYSQL_LANG_FILE\" $$AIS_LANG_FILE")
        }
    } else {
        # the language file does not exist, copy the installed errmsg.sys
        message("$$AIS_LANG_FILE does not exist, copying from $$MYSQL_LANG_FILE .")
        system("mkdir -p \"$$AIS_LANG_DIR\"")
        system("cp \"$$MYSQL_LANG_FILE\" $$AIS_LANG_FILE")
    }

    release {
        SRC_CONFIG_DIR = "extras/config"
        config_files.files += $$SRC_CONFIG_DIR/aisinstall.ini \
                              $$SRC_CONFIG_DIR/rideinstall.ini
        config_files.path = $$AIS_CONFIG_DIR

        library_files.files += Libraries
        library_files.path = $$AIS_DATA_DIR

        demos_files.files += Demos
        demos_files.path = $$AIS_SHARE_DIR

        shared_library_files.files += Libraries
        shared_library_files.path = $$AIS_SHARE_DIR

        mysql_files.files += mysqlmsgs
        mysql_files.path = $$AIS_SHARE_DIR

        aissvc_data_files.files += $$SRC_CONFIG_DIR/ais.ini \
                                   $$SRC_CONFIG_DIR/AStartup.sl \
                                   $$SRC_CONFIG_DIR/context.ini \
                                   $$SRC_CONFIG_DIR/contextusers.ini \
                                   $$SRC_CONFIG_DIR/usr \
                                   $$SRC_CONFIG_DIR/mysqldata
        aissvc_data_files.path = $$AIS_DATA_DIR

        aissvc_log.path = $$AIS_LOG_DIR

        INSTALLS += config_files \
                    library_files \
                    demos_files \
                    shared_library_files \
                    mysql_files \
                    aissvc_data_files \
                    aissvc_log
    }
}

