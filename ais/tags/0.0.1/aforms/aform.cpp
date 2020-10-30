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
aisdev/aforms/aform.cpp
														Form

CHANGE HISTORY
Version	Date		Who		Change
1.0100	6/9/2006	tlw		Add Doxygen documentation
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "aform.h"

//	------------------------------------------------------ METHODS -------------------------------------------------------------
/*!
\brief AForm - constructor instantiates a new QTabbed widget for monitoring an AIS server or as a development environment.
 
\par Args:
\param ipParent -> Parent window that created this instance of a form
\param ipFormName -> Name assigned to this instance of a form
\return void
\par Notes:
- ipParent is the AMainWindow that has one or more forms in its Workspace.
- ipFormName is the local server name for Server forms and the Context name for Session forms.
 - AForm inherits QTabWidget. AForm has one or more tabs containing pages which are QWidgets.
 */
AForm::AForm(QWidget* ipParent, const char* ipFormName)
: QTabWidget(ipParent)
{
	setObjectName(QString(ipFormName));
}

// end

