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
aisdev/atextedit/aoperationsdialog.cpp
													Operations Dialog

CHANGE HISTORY
Version	Date		Who		Change
3.2006	 3/11/2008	fchua	Initial version.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "aoperationsdialog.h"
#include <QtCore/QtDebug>

AOperationsDialog::AOperationsDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlgs):
	QDialog(ipParent, iFlgs)
{
    Q_UNUSED(ipName);
}

AOperationsDialog::~AOperationsDialog()
{
}

void AOperationsDialog::init()
{
	qDebug("AOperationsDialog::init()");
}

void AOperationsDialog::init(ATextEdit* ipTextEdit, QStringList& irList)
{
    Q_UNUSED(ipTextEdit);
    Q_UNUSED(irList);
	qDebug("AOperationsDialog::init(ATextEdit*, QStringList&)");
}

void AOperationsDialog::init(QSettings* ipSettings, ATextEdit* ipTextEdit)
{
    Q_UNUSED(ipSettings);
    Q_UNUSED(ipTextEdit);
	qDebug("AOperationsDialog::init(QSettings*, ATextEdit*)");
}

void AOperationsDialog::onFindPatternResult(bool iResult)
{
    Q_UNUSED(iResult);
	qDebug("AOperationsDialog::onFindPatternResult(bool)");
}

void AOperationsDialog::onReplaceFindPatternResult(bool iResult)
{
    Q_UNUSED(iResult);
	qDebug("AOperationsDialog::onReplaceFindPatternResult(bool)");
}

void AOperationsDialog::onReplacePatternResult(bool iResult)
{
    Q_UNUSED(iResult);
	qDebug("AOperationsDialog::onReplacePatternResult(bool)");
}

void AOperationsDialog::showOnTop()
{
	qDebug("AOperationsDialog::showOnTop()");
}
