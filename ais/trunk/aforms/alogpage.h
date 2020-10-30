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
#ifndef ALOGPAGE_H
#define ALOGPAGE_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/forms/alogpage.h
															Log Page

CHANGE HISTORY
Version	Date		Who		Change
3.2008	4/08/2008	fchua	Added new parameter ipTextEditParams in constructor.
3.2006	3/10/2008	fchua	Removed find and replace functions. Added getTextEdit.
3.2002	 2/4/2008	fchua	Added onUsrAccessCheckBox.
1.0113	11/7/2006	tlw		Omit unused destructor.
1.0104	9/8/2006	tlw		Remove MinSpinBox which was not being used
1.0100	6/11/2006	tlw		Add Doxygen documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "ais.h"		// geLogAll, geLogAmp, geLogConsole, geLogReqHdr, geLogSysMsg, geUserAccess geLogStatus
#include "apage.h"			// APage, AMenuList, AToolList
#include "ui_alogpage.h"

class QWidget;
class AServerForm;
class ATextEdit;

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
\brief ALogPage - Implements the log page used by server form.

 */
class ALogPage : public QWidget, public APage
{
    Q_OBJECT
public:
    ALogPage(AReqType iLogType, AServerForm* ipParentForm, QWidget* ipParent,
			 const char* ipName = NULL, AParameters* ipTextEditParams = NULL);
	ATextEdit* getTextEdit();

	// APage methods
	virtual void	copy();
	virtual void	cut();
	virtual QWidget*getFocusWidget();
	virtual	void	getTabMenus(AMenuList& orTabMenus, AToolList& orTabTools, bool iSelected);
	virtual void	paste();
	virtual void	print();
	virtual void	redo();
	virtual void	undo();
	void			init();

public slots:
	void	onClear();
	void	onLogEnabled(AReqType iLogType, long iMinLevel, bool iOn);
	void	onReturnMsg(AReqType iLogType, const QString& irMsg);

signals:
	void	clearAll();
	void	consoleEnabled(bool iOn);
	void	logEnabled(AReqType iLogType, long iMinLevel, bool iOn);
	void	reqHdrEnabled(bool iOn);
	void	returnMsg(AReqType iLogType, const QString& irMsg);
	void	sysMsgEnabled(bool iOn);

private slots:
	void	onAmpCheckBox(bool iOn);
	void	onConsoleCheckBox(bool iOn);
	void	onDisplayCheckBox(bool iOn);
	void	onReqHdrsCheckBox(bool iOn);
	void	onStatusAlert(const QString& irMsg);
	void	onSysMsgsCheckBox(bool iOn);
	void	onUsrAccessCheckBox(bool iOn);

private:
	bool			cDisplayEnabled;	// Saved status of Display check box.
	AReqType		cLogType;			// The page type as noted above.
	long			cMaxLines;			// MaxLines setting in the text edit page.
	bool			cMonitorEnabled;	// Monitor page enabled and this log selected.
	AServerForm*	cpParentForm;		// For call backs to the parent.
	Ui::ALogPageClass	cUi;
};

#endif // ALOGPAGE_H

