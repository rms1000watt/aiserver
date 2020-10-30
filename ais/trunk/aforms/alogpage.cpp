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
aisdev/aforms/alogpage.cpp
														Log Page Widget

CHANGE HISTORY
Version	Date		Who		Change
3.2008	4/08/2008	fchua	Updated constructor to use ipTextEditParams argument.
3.2006	3/10/2008	fchua	Removed find and replace functions. Added getTextEdit.
3.2002	 2/4/2008	fchua	Fixed Doxygen comments.
3.2002	 2/4/2008	fchua	Added onUsrAccessCheckBox.
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0109	10/1/2006	tlw		editSetParameter. Modify parameter names
1.0104	 9/8/2006	tlw		Remove MinSpinBox which was not being used
1.0070	10/3/2005	tlw		Change request format. Allow multiple simultaneous connections.
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "aglobals.h"
#include "aserverform.h"
#include "alogpage.h"

//	------------------------------------------------------ METHODS -------------------------------------------------------------
/*!
 * \brief Constructor instantiates the GUI elements generated in ui_logpage.h
 *
 * \param iLogType[in] - The type of logs for this page (geLogAll, geLogAmp, geLogConsole, geLogReqHdr, geLogSysMsg, geLogUserAccess)
 * \param ipParentForm[in] -> Parent form that created this instance of a cabinet page
 * \param ipParent[in] -> Parent widget for this form
 * \param ipName[in] -> Name assigned to this instance of the log page
 *
 * \return void
 */
ALogPage::ALogPage(AReqType iLogType, AServerForm* ipParentForm, QWidget* ipParent,
				   const char* ipName, AParameters* ipTextEditParams):
	QWidget(ipParent), cpParentForm(ipParentForm)
{
    Q_UNUSED(ipName);

	// Setup. Establish the widgets for all types of log pages.
	cUi.setupUi(this);
	cDisplayEnabled = false;
	cLogType = iLogType;
	cMaxLines = -1;
	cMonitorEnabled = false;

	// Monitor. Set the monitor page widgets
	if (cLogType == geLogAll)
	{	cUi.upDisplayCheckBox->setCheckState(Qt::Checked);
		cUi.upSysMsgsCheckBox->setCheckState(Qt::Checked);

		connect(cUi.upClearAllButton,  SIGNAL(clicked()),     this, SIGNAL(clearAll()));
		connect(cUi.upAmpCheckBox,     SIGNAL(toggled(bool)), this, SLOT(onAmpCheckBox(bool)));
		connect(cUi.upConsoleCheckBox, SIGNAL(toggled(bool)), this, SLOT(onConsoleCheckBox(bool)));
		connect(cUi.upReqHdrsCheckBox, SIGNAL(toggled(bool)), this, SLOT(onReqHdrsCheckBox(bool)));
		connect(cUi.upSysMsgsCheckBox, SIGNAL(toggled(bool)), this, SLOT(onSysMsgsCheckBox(bool)));
		connect(cUi.upUsrAccessCheckBox, SIGNAL(toggled(bool)), this, SLOT(onUsrAccessCheckBox(bool)));
	}
	else	// Logs. Remove selected widgets from regular log pages. These widgets are not deleted.
	{	cUi.upClearAllButton->setVisible(false);
		cUi.upLogLabel->setVisible(false);
		cUi.upAmpCheckBox->setVisible(false);
		cUi.upConsoleCheckBox->setVisible(false);
		cUi.upReqHdrsCheckBox->setVisible(false);
		cUi.upSysMsgsCheckBox->setVisible(false);
		cUi.upUsrAccessCheckBox->setVisible(false);
		// Must remove layout to collapse space reserved for the check boxes.
		cUi.vboxLayout->removeItem(cUi.hboxLayout1);
		if (cLogType == geLogSysMsg)
			cUi.upDisplayCheckBox->setCheckState(Qt::Checked);
	}
	//  Connections. Connect up all the buttons and check boxes common to all pages.
	connect(cUi.upClearButton, SIGNAL(clicked()), this, SLOT(onClear()));
	connect(cUi.upDisplayCheckBox, SIGNAL(toggled(bool)), this, SLOT(onDisplayCheckBox(bool)));
	connect(cUi.upTextEdit, SIGNAL(statusAlert(const QString&)), this, SLOT(onStatusAlert(const QString&)));

	if (ipTextEditParams != NULL)
	{
		cUi.upTextEdit->editSetParameter("Font", QVariant(ipTextEditParams->mFont));
		cUi.upTextEdit->editSetParameter("FontSize", QVariant((int)ipTextEditParams->mFontSize));
		cUi.upTextEdit->editSetParameter("LineNumbers", QVariant(ipTextEditParams->mLineNumbers));
		cUi.upTextEdit->editSetParameter("WordWrap", QVariant(ipTextEditParams->mWrap));
	}
}

/*!
 * \brief Copy the currently selected text to the clipboard
 *
 * \return void
 * \note
 * -# Inherited from the APage base class.
*/
void ALogPage::copy()
{
	cUi.upTextEdit->editCopy(false/*Append*/);
}

/*!
 * \brief Move the currently selected text to the clipboard
 *
 * \return void
 *
 * \note
 * -# Inherited from the APage base class.
 * -# Some text must be currently selected (highlighted)
 */
void ALogPage::cut()
{
	cUi.upTextEdit->editCut(false/*Append*/);
}

/*!
 * \brief Returns the widget that currently has focus.
 *
 * \return apFocus - The widget that currently has focus
 *
 * \note
 * -# Inherited from the APage base class.
 * -# The widget that has focus receives keyboard and mouse input.
 */
QWidget* ALogPage::getFocusWidget()
{
	QWidget* apFocus = focusWidget();
	if (apFocus == NULL)
		apFocus = cUi.upTextEdit;
	return apFocus;
}

/*!
 * \brief Returns the menus and the toolbar actions for the console page.
 *
 * \param[out] orTabMenus Reference to place to put the console page menu list.
 * \param[out] orTabTools Reference to place to put the console page tool list.
 * \param[in] iSelected True iff this tab is currently selected.
 *
 * \return void
 *
 * \note
 * -# Inherited from the APage base class.
 * -# Used to update the Session Form menus and toolbar list.
 */
void ALogPage::getTabMenus(AMenuList& orTabMenus, AToolList& orTabTools, bool iSelected)
{
    Q_UNUSED(iSelected);

	// No menus yet
	orTabMenus.clear();
	orTabTools.clear();
}

/*!
 * \brief Returns a pointer to the underlying ATextEdit object.
 */
ATextEdit* ALogPage::getTextEdit()
{
	return cUi.upTextEdit;
}

/*!
 * \brief Initializes the log page.
 *
 * \note
 * Must be done after the parent has established connections to the signals generated by init.
 */
void ALogPage::init()
{
	// Initialize. Sync up with initial GUI settings
	if (cUi.upDisplayCheckBox->isChecked())
		onDisplayCheckBox(true);
}

/*!
 * \brief Called when the AMP Checkbox is checked/unchecked.
 *
 * \note
 * The Amp CheckBox is only visible in the Monitor Page.
 */
void ALogPage::onAmpCheckBox(bool iChecked)
{
	//  If display CheckBox is checked, enable/disable this log. Send signal is to the log.
	if (cUi.upDisplayCheckBox->isChecked())
		emit logEnabled(geLogAmp, 0, iChecked);
}

/*!
 * \brief Clear the text pane.
 *
 * \return void
 *
 * \note
 * -# Called from a connection formed here or in the ServerForm.
 */
void ALogPage::onClear()
{
	cUi.upTextEdit->editClear();
}

/*!
 * \brief Called when the Console Checkbox is checked/unchecked.
 *
 * \note
 * The Console CheckBox is only visible in the Monitor Page.
 */
void ALogPage::onConsoleCheckBox(bool iChecked)
{
	//  If Monitor display CheckBox is checked, enable/disable this log. Send signal to the log.
	if (cUi.upDisplayCheckBox->isChecked())
		emit logEnabled(geLogConsole, 0, iChecked);
}

/*!
 * \brief Enable/disable this log if display is enabled/disabled
 *
 * \param[in] iLogType Type of log to be modified (geLogAll, geLogAmp, geLogConsole, geLogReqHdr, geLogSysMsg, geLogUserAccess).
 * \param[in] iMinLevel Minimum log level for this type of message to be sent.
 * \param[in] iOn Enable/disable flag.
 *
 * \return void
 *
 * \note
 * -# Called from a connection formed here or in the ServerForm.
 */
void ALogPage::onLogEnabled(AReqType iLogType, long iMinLevel, bool iOn)
{
    Q_UNUSED(iMinLevel);

	// Log. If Display not checked, enable/disable log.  Signal is sent to parent form.
	if (iLogType == cLogType)
	{	cMonitorEnabled = iOn;
		if (!cDisplayEnabled)
			emit logEnabled(cLogType, geInfo, iOn);
	}
}

/*!
 * \brief Called when the Display Checkbox is checked/unchecked.
 *
 */
void ALogPage::onDisplayCheckBox(bool iChecked)
{
	// Log. If not already enabled elsewhere, enable/disable log if Display checked/unchecked. Send signal to parent form.
	cDisplayEnabled = iChecked;
	if (cLogType != geLogAll)
	{	if (!cMonitorEnabled)
			emit logEnabled(cLogType, geInfo, iChecked);
	}
	else // Monitor Page.
	{	// If log is checked, enable/disable this log.  Send signal to logPage.
		if (cUi.upAmpCheckBox->isChecked())
			emit logEnabled(geLogAmp, 0, iChecked);
		if (cUi.upConsoleCheckBox->isChecked())
			emit logEnabled(geLogConsole, 0, iChecked);
		if (cUi.upReqHdrsCheckBox->isChecked())
			emit logEnabled(geLogReqHdr, 0, iChecked);
		if (cUi.upSysMsgsCheckBox->isChecked())
			emit logEnabled(geLogSysMsg, 0, iChecked);
		if (cUi.upUsrAccessCheckBox->isChecked())
			emit logEnabled(geLogUserAccess, 0, iChecked);
	}
}

/*!
 * \brief Process returned messages from AIS in response to locally generated requests.
 *
 * \param[in] iLogType Type of request that generated this message (geLogAll, geLogAmp, geLogConsole, geLogReqHdr, geLogSysMsg, geLogUserAccess).
 * \param[in] irMsg Returned message.
 *
 * \return void
 *
 * \note
 * -# Inherited from the APage base class.
 * -# The clipboard is not modified.
 */
void ALogPage::onReturnMsg(AReqType iLogType, const QString& irMsg)
{
	if (iLogType == cLogType)
	{	if (cDisplayEnabled)
		{ 	long aMaxLines = cUi.upMaxSpinBox->value();
			if (cMaxLines != aMaxLines)
			{	cMaxLines = aMaxLines;
				cUi.upTextEdit->editSetParameter("MaximumRows", QVariant((int)aMaxLines));
			}
			cUi.upTextEdit->editAppend(irMsg, true/*MoveToEnd*/);
		}
		if (iLogType != geLogAll && cMonitorEnabled)
			emit returnMsg(geLogAll, irMsg);
	}
}

/*!
 * \brief Called when the Request Headers Checkbox is checked/unchecked.
 *
 * \note
 * The Request Headers CheckBox is only visible in the Monitor Page.
 */
void ALogPage::onReqHdrsCheckBox(bool iChecked)
{
	//  If display CheckBox is checked, enable/disable this log. Send signal to the log.
	if (cUi.upDisplayCheckBox->isChecked())
		emit logEnabled(geLogReqHdr, 0, iChecked);
}

/*!
 * \brief Display status alert in parent form.
 *
 */
void ALogPage::onStatusAlert(const QString& irMsg)
{
	cpParentForm->statusAlert(irMsg);
}

/*!
 * \brief Called when the System Messages Checkbox is checked/unchecked.
 *
 * \note
 * The System Messages CheckBox is only visible in the Monitor Page.
 */
void ALogPage::onSysMsgsCheckBox(bool iChecked)
{
	//  If display CheckBox is checked, enable/disable this log. Send signal to the log.
	if (cUi.upDisplayCheckBox->isChecked())
		emit logEnabled(geLogSysMsg, 0, iChecked);
}

/*!
 * \brief Called when the User Access Checkbox is checked/unchecked.
 *
 * \note
 * The User Access CheckBox is only visible in the Monitor Page.
 */
void ALogPage::onUsrAccessCheckBox(bool iChecked)
{
	//  If display CheckBox is checked, enable/disable this log. Send signal to the log.
	if (cUi.upDisplayCheckBox->isChecked())
		emit logEnabled(geLogUserAccess, 0, iChecked);
}

/*!
 * \brief Insert the text from the clipboard into the text at the cursor.
 *
 * \return void
 *
 * \note
 * -# Inherited from the APage base class.
 * -# The clipboard is not modified.
 */
void ALogPage::paste()
{
	cUi.upTextEdit->editPaste();
}

/*!
 * \brief Print the text in the console pane to the default printer.
 *
 * \return void
 *
 * \note
 * -# Inherited from the APage base class.
 * -# Requires that a printer be available to this machine.
 */
void ALogPage::print()
{
	cUi.upTextEdit->editPrint();
}

/*!
 * \brief Redo the last undo operation.
 *
 * \return void
 *
 * \note
 * -# Inherited from the APage base class.
 * -# Requires that at least one undo operation was performed after the last edit operation.
 */
void ALogPage::redo()
{
	cUi.upTextEdit->editRedo(true/*ToMark*/);
}

/*!
 * \brief Undo the last edit operation on the console output pane.
 *
 * \return void
 *
 * \note
 * -# Inherited from the APage base class.
 * -# Requires at least one prior edit operation on the console output pane.
 */
void ALogPage::undo()
{
	cUi.upTextEdit->editUndo(true/*ToMark*/);
}

// end
