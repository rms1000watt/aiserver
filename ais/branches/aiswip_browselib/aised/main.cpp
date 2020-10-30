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

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aised/main.cpp
														AIS Editor

AisEd is a fast, flexible screen editor.  See atextedit project for more details.

CHANGE HISTORY
Version	Date		Who		Change
1.0057	 3/18/2005	tlw		Add documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QApplication>
#include "aisedit.h"

//	------------------------------------------------------ GLOBALS ------------------------------------------------------------
extern int qInitResources_aisedit();

//	----------------------------------------------------- FUNCTIONS -----------------------------------------------------------
int main(int argc, char ** argv)
{
	// MainWindow. Ok to set DeleteOnClose since apMainWindow is a heap object. (Don't try this with a stack object.)
	QApplication aApp(argc, argv);
	QObject aObject(NULL);
	qInitResources_aisedit();
	AisEdit* apMainWindow = new AisEdit(NULL, "AisEdit", Qt::Window);
	apMainWindow->setAttribute(Qt::WA_DeleteOnClose);
	apMainWindow->show();
	aApp.connect(&aApp, SIGNAL(lastWindowClosed()), apMainWindow, SLOT(onExit()));
	return aApp.exec();
}
// end
