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
aisdev/aforms/apage.cpp
														Page

CHANGE HISTORY
Version	Date		Who		Change
3.1003	10/29/2007	fchua	Added member function clear(). Fixed Doxygen documentation.
1.0107	9/21/2006	tlw		Add move to implement cursor stack instructions.
1.0100	6/11/2006	tlw		Add Doxygen documentation
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QMessageBox>
#include "apage.h"

//	------------------------------------------------------ METHODS -------------------------------------------------------------
/*!
 * \brief Constructor instantiates a base class that is inherited by all pages of the forms which are tabbed widgets.
 *
 */
APage::APage()
{
}

/*!
 * \brief Clear or reset the page contents or configuration.
 *
 * \note
 * -# Base class method if not reimplemented by the page class that inherits APage.
 */  
void APage::clear()
{

}

/*!
 * \brief Copy the currently selected text to the clipboard.
 *
 * \note
 * -# Base class method if not reimplemented by the page class that inherits APage.
 */
void APage::copy()
{
	errorMessage("Copy");
}

/*!
 * \brief Delete the currently selected text and add it to the clipboard.
 *
 * \note
 * -# Base class method if not reimplemented by the page class that inherits APage.
 */
void APage::cut()
{
	errorMessage("Paste");
}

/*!
 * \brief Display an error message.
 *
 * \param[in] ipMsg Error message.
 */
void APage::errorMessage(const char* ipMsg)
{
	QString aOperation(ipMsg);
	QString aText = QString("Operation %1 is not supported by the target tab").arg(aOperation);
	QMessageBox::information(NULL, aOperation, aText, QMessageBox::Ok);
}

/*!
 * \brief Invoke the find dialog.
 *
 * \note
 * -# Base class method if not reimplemented by the page class that inherits APage.
 */
void APage::find()
{
	errorMessage("Find");
}

/*!
 * \brief Return the widget that currently has focus.
 *
 * \note
 * -# Base class method if not reimplemented by the page class that inherits APage.
 */
QWidget* APage::getFocusWidget()
{
	return NULL;
}

/*!
 * \brief Move forward/back in cursor stack.
 *
 * \note
 * -# Base class method if not reimplemented by the page class that inherits APage.
 */
void APage::move(bool iNext)
{
    Q_UNUSED(iNext);

	errorMessage("Move");
}

/*!
 * \brief Open a new page.
 *
 * \note
 * -# Base class method if not reimplemented by the page class that inherits APage.
 */
void APage::openPage(const QString& irExtentName, const QString& irPageName, AEditPage::APageType iPageType)
{
    Q_UNUSED(irExtentName);
    Q_UNUSED(iPageType);

	QByteArray aMsg("openPage ");
	aMsg += irPageName;
	errorMessage(aMsg.data());
}

/*!
 * \brief Insert the contents of the clipboard into the current text widget at the cursor.
 *
 * \note
 * -# Base class method if not reimplemented by the page class that inherits APage.
 */
void APage::paste()
{
	errorMessage("Paste");
}

/*!
 * \brief Print the text pane on the system default printer
 *
 * \note
 * -# Base class method if not reimplemented by the page class that inherits APage.
 */
void APage::print()
{
	errorMessage("Print");
}

/*!
 * \brief Reverse the last undo operation on the current text pane.
 *
 * \note
 * -# Base class method if not reimplemented by the page class that inherits APage.
 */
void APage::redo()
{
	errorMessage("Redo");
}

/*!
 * \brief Invoke the find/replace dialog.
 *
 * \notes
 * -# Base class method if not reimplemented by the page class that inherits APage.
 */
void APage::replace()
{
	errorMessage("Replace");
}

/*!
 * \brief Fetch the currently selected text form the current text pane.
 *
 * \note
 * -# Base class method if not reimplemented by the page class that inherits APage.
 */
QString APage::selectedText()
{
	errorMessage("SelectedText");
	return QString();
}

/*!
 * \brief Initialize the page when it is made visible.
 *
 * \note
 * -# Base class method if not reimplemented by the page class that inherits APage.
 */
void APage::show()
{
	errorMessage("Show");
}

/*!
 * \brief Reverse the last edit operation on the current text pane.
 *
 * \note
 * -# Base class method if not reimplemented by the page class that inherits APage.
 */
void APage::undo()
{
	errorMessage("Undo");
}


// end
