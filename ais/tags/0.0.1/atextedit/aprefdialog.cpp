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
aisdev/atextedit/aprefdialog.cpp
													Preference Dialog

CHANGE HISTORY
Version	Date		Who		Change
3.2008	 4/03/2008	fchua	Initial version.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "aprefdialog.h"
#include <QtCore/QtDebug>

APrefDialog::APrefDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlgs):
	QDialog(ipParent, iFlgs)
{
	setObjectName(ipName);
}

APrefDialog::~APrefDialog()
{

}

void APrefDialog::setParameters(AParameters *ipParams)
{
    Q_UNUSED(ipParams);
	qDebug("APrefsDialog::setParameters");
}

void APrefDialog::getParameters(AParameters *iopParams)
{
    Q_UNUSED(iopParams);
	qDebug("APrefsDialog::getParameters");
}
