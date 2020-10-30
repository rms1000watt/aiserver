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
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aforms/anodevaluedialog.cpp
											Node Value Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0056	 3/ 3/2005	tlw		Change request format. Allow multiple simultaneous connections.
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QVariant>
#include <QtGui/QPushButton>
#include <QtGui/QLineEdit>
#include <QtGui/QLayout>
#include <QtGui/QToolTip>
#include <QtGui/QImage>
#include <QtGui/QPixmap>
#include <QtGui/QHBoxLayout>
#include <QtGui/QGridLayout>

#include "anodevaluedialog.h"

//	------------------------------------------------------ METHODS -------------------------------------------------------------
/*!
\brief ANodeValueDialog - constructor instantiates the GUI elements generated in ui_nodevaluedialog.h
 
\par Args:
\param ipParent -> Parent tab that created this instance of the dialog
\param ipName -> Name assigned to this instance of the node value dialog
\param iFlgs Window flags that determine how this dialog is displayed.
\return void
 */
ANodeValueDialog::ANodeValueDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlgs)
    : QDialog(ipParent, iFlgs)
{
	// Gui. Configure widgets
	cUi.setupUi(this);
	setObjectName(ipName);

	// Connections.
	QObject::connect(cUi.upOkButton, SIGNAL(clicked()), this, SLOT(reject()));
}

/*!
\brief setup - Called from the cabinet page when the dialog is opened to display a node value.
 
\par Args:
\param irData Node value to be displayed in the text window.
\param irTitle Description of the variable whose value is being displayed displayed.
\return void
 */
void ANodeValueDialog::setup(const QString& irData, const QString& irTitle)
{
	cUi.upDataLineEdit->clear();
	cUi.upDataTextEdit->clear();
	cUi.upDataLineEdit->setText(irTitle);
	cUi.upDataTextEdit->append(irData);
}
// end
