/**********************************************************************************
    Copyright (C) 2008 Investment Science Corp.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

***********************************************************************************/

#include <QApplication>
#include "asvcmgr.h"

int main(int iArgc, char **ipArgv)
{
	QApplication aApp(iArgc, ipArgv);
	ASvcMgr aMainWindow(NULL, Qt::Dialog | Qt::WindowMinimizeButtonHint);

	aMainWindow.setWindowTitle("AIS Service Control");
	
	QObject::connect(&aApp, SIGNAL(lastWindowClosed()), &aApp, SLOT(quit()));
	aMainWindow.show();

    return aApp.exec();
}
