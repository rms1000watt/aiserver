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

#ifndef AISEDIT_H
#define AISEDIT_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aised/aisedit.h
														AIS Editor

CHANGE HISTORY
Version	Date		Who		Change
1.0100	 4/19/2006	tlw		Add Doxygen top-level documents
1.0057	 3/18/2005	tlw		Add documentation
												--------------- ---------------


Pending:
	See atextedit Change Requests
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	--------------------------------------------------- DOCUMENTATION ---------------------------------------------------------
/*!
\brief AisEdit Documentation
 */
/*!
\mainpage Aised Source Documentation

The AisEd Source Documentation documents the AisEd project in the aisdev folder.  It documents aisedit.h, aisedit.cpp, and
main.cpp.  Someday, this documentation will be folded into the documentation for all of the aisdev folders.
This documentation is patterned after the categories adopted by Trolltech in their QT library.

- \ref aisedit

\section aisedit AisEdit Features
AisEd is a fast, flexible screen editor with the following features:
- Edits large files gracefully.
- Insert, move, delete, undo, redo
- SmartLisp, JavaScript syntax highlighting
- Matches parens, braces, quotes
- Supports word wrap, tabs, paragraph attributes (e.g. line numbers)
- Toolbars, menus, customization (tab width, font size)
- Context menus including Lambda list.
- Find/replace dialog.
- Fixed-width fonts only. No font styles within a paragraph (bold, underline...)
- Config file to set font size, tab width, word wrap, window position & size, line attributes,...

\subsection structure Ais Edit Structure
AisEdit uses a very small character representation. Each new-line terminated row is stored in a dynamic array of 16-bit unsigned
integers [ushort].  Each ushort holds a character code + color attributes.  The array is always terminated by a newline.  In the
display, a row may wrap (if wrap enabled) so that it is displayed on one or more lines in the edit window.

\subsection other Other Applications
No other applications are currently implemented

 */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QSettings>
#include <QtGui/QMainWindow>

class QLabel;
class QCloseEvent;
class QContextMenuEvent;
class QTabWidget;
class QWidget;

class ATextEdit;

//	---------------------------------------------------- DEFINITIONS ----------------------------------------------------------
// ATED_ definitions are found in atextedit.h
/*! \def AED_UNTITLED
Set to the name of new, blank text files. Typically set to Untitled */
#define	AED_UNTITLED			"Untitled"
/*! \def AED_DEFSCREENHEIGHT
Set to the default screen height in pixels. This value is only used if a setting has not been previously saved for this user */
#define AED_DEFSCREENHEIGHT		480
/*! \def AED_DEFSCREENWIDTH
Set to the default screen width in pixels.  This value is only used if a setting has not been previously saved for this user */
#define AED_DEFSCREENWIDTH		640

/*! \def AED_HORIZMARGIN
Set to the horizontal print margin in inches. Print margin in inches expressed as a ratio of integers. */
#define AED_HORIZMARGIN			3 / 8

/*! \def AED_VERTMARGIN
Set to the vertical print margin in inches. Print margin is expressed as a ratio of integers. */
#define AED_VERTMARGIN			1 / 2

//	------------------------------------------------- CLASS DECLARATIONS ------------------------------------------------------
/*!
\brief AisEdit - A flexible, fast screen editor with syntax highlighting for several languages.

AisEdit is a GUI-based screen editor that is designed to handle large files with a small footprint.  Each character only requires
a 16-bit integer (ushort).  The upper byte holds two 4-bit color indices and the lower byte holds a latin-1 character code.  The
upper 4-bits is an index into a table of background colors and the next 4-bits holds an index into a table of foreground colors.
\sa aisedit
\par Notes:
- AisEdit uses the ATextEdit class to do all of the text processing.
\par Example:
aisdev/aised/main.cpp creates an instance of AisEdit to implement aised.exe.
*/
class AisEdit : public QMainWindow
{
	Q_OBJECT
public:
	AisEdit(QWidget* ipParent, const char* ipName, Qt::WFlags iFlgs);
	~AisEdit();

protected:
	virtual void closeEvent(QCloseEvent* ipEvent);
	virtual void contextMenuEvent(QContextMenuEvent* ipContextEvent);

private slots:
	void		onAbout();
	void		onClose();
	void		onClose(ATextEdit* ipPage);
	void		onComment();
	void		onContextMenu();
	void		onCopy();
	void		onCurrentTabChanged(int iTabIdx);
	void		onCursorPositionChanged(int iRow, int iCol);
	void		onCut();
	void		onExit();
	void		onFind();
	void		onFunctionList();
	void		onHelp();
	void		onHelpApi();
	void		onHelpFind();
	void		onIndent();
	void		onNew();
	void		onOpen();
	void		onOutdent();
	void		onPaste();
	void		onPrint();
	void		onRedo();
	void		onReplace();
	void		onSave();
	void		onSave(ATextEdit* ipPage);
	void		onSaveAll();
	void		onSaveAs();
	void		onSaveAs(ATextEdit* ipPage);
	void		onStatusAlert(const QString& irMsg);
	void		onTextModified(bool iModified);
	void		onUncomment();
	void		onUndo();

private:
	bool		addPage(const QString& irFileSpec);
	void		closeAll();
	void		createMenus();

	QLabel*		cpAlertLabel;
	QString		cApplication;
	QMenu*		cpContextMenu;
	ATextEdit*	cpCurPage;
	QString		cInstallPath;
	QLabel*		cpRowColLabel;
	QSettings	cSettings;
	QTabWidget*	cpTabWidget;
};
#endif		// AISEDIT_H
