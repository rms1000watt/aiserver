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
aisdev/aised/aisedit.cpp
														AIS Editor

AisEdit is a fast, flexible screen editor.  See aisedit.cpp for more details.

CHANGE HISTORY
Version	Date		Who		Change
3.2008	 4/01/2008	fchua	Added loadSettings, saveSettings, onEditPreferences, onWordWrap, onLineNumber, onShowWhiteSpace
3.2006	 3/10/2008	fchua	Incorporated the Find, Replace, Go, Function List dialogs.
2.0002	1/20/2007	tlw		onCursorPositionChanged. Use long arguments.
1.0119	12/17/2006	tlw		Constructor. If Origin from cSettings is not in the window, move it back to (0,0).
1.0110	10/13/2006	tlw		CursorStack. Enable/disable cursor move arrows.
1.0107	 9/21/2006	tlw		doClose. Add doClose which returns false if close operation is cancelled by user.
1.0107	 9/20/2006	tlw		Add cursor stack tool bar icons. Add version number to title.
1.0100	 4/19/2006	tlw		Convert to Doxygen
1.0057	 3/ 3/2005	tlw		Add documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QFile>
#include <QtCore/QFileInfo>
#include <QtCore/QTextStream>

#include <QtGui/QApplication>
#include <QtGui/QAction>
#include <QtGui/QCloseEvent>
#include <QtGui/QIcon>
#include <QtGui/QLabel>
#include <QtGui/QMenuBar>
#include <QtGui/QToolBar>
#include <QtGui/QMessageBox>
#include <QtGui/QStatusBar>
#include <QtGui/QTabWidget>

#include "aglobaldefs.h"
#include "aisedit.h"
#include "atextedit.h"
#include "aoperationsdialog.h"
#include "aprefdialog.h"
#include "aparameters.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
\brief AisEdit - Constructor restores settings and initializes GUI

AisEdit provides the GUI for the ATextEdit editor.  It includes a multi-tabbed window that allows the user to simultaneously
edit multiple text files.
\sa ATextEdit
\par Args:
\param ipParent -> to parent widget (typically the main window)
\param ipName -> name of this instance of AisEdit
\param iFlgs optional set of flags to control the disposition of this window.
\par Returns:
Nothing
\par Notes:
-# The GUI is handcrafted here.  A better solution would be to use Qt Designer to implement the GUI.
*/
AisEdit::AisEdit(QWidget* ipParent, const char* ipName, Qt::WFlags iFlgs)
 	: QMainWindow(ipParent, iFlgs), cApplication("aisedit"), cpCurPage(NULL),
 	  cSettings(QSettings::IniFormat, QSettings::UserScope, "ais", cApplication, this),
	  cpFcnListDialog(0), cpFindDialog(0), cpGoDialog(0), cpReplaceDialog(0), cpPrefDialog(0)
{
    Q_UNUSED(ipName);

	// Settings. Restore settings.
	loadSettings();

	// MainWindow. Set main window's attributes
	resize(cSize.expandedTo(minimumSizeHint()));
	move(cOrigin);
	setMouseTracking(true/*Enable*/);
	setWindowTitle(tr("AIS Editor Ver:" AGBL_AISVERSION));
	QIcon aAisEditIcon;
	aAisEditIcon.addFile(":/images/aisedit.png", QSize(22,22), QIcon::Normal, QIcon::Off);
	setWindowIcon(aAisEditIcon);

	// Menus. Populate menus and toolbars including context menu.
	createMenus();

	// StatusBar. Add normal message, permanent alert box, and permanent RowCol box to status bar.
	QStatusBar* apStatusBar = statusBar();
	apStatusBar->showMessage(tr("AIS Edit"));

	// Status Message. Add a permanent and a normal status widget to hold alerts and messages. Connected in createForm.
	cpAlertLabel = new QLabel(this);
	cpAlertLabel->setObjectName(tr("AlertLabel"));
	cpAlertLabel->setFixedHeight(22);			// 22 Pixels tall
    QSizePolicy aSizePolicy(QSizePolicy::Minimum, QSizePolicy::Fixed);
    aSizePolicy.setHorizontalStretch(0);		// % change in horizontal char size.
    aSizePolicy.setVerticalStretch(0);			// % change in vertical char size.
    cpAlertLabel->setSizePolicy(aSizePolicy);	// Minimum but may expand
	QSize aMinSize(125, 22);					// Allow min of 16 chars (7.8 pixels/char)
    cpAlertLabel->setMinimumSize(aMinSize);
	QFont aFont("Courier New", 10, QFont::Bold, false/*Italic*/);
	cpAlertLabel->setFont(aFont);
	QPalette aPalette;
	aPalette.setColor(QPalette::BrightText, Qt::red);
	cpAlertLabel->setPalette(aPalette);
	cpAlertLabel->setForegroundRole(QPalette::BrightText);
	cpAlertLabel->setText(cSettings.fileName());
	apStatusBar->addPermanentWidget(cpAlertLabel);

	cpRowColLabel = new QLabel(this);
	cpRowColLabel->setObjectName("RowColLabel");
    aSizePolicy.setHorizontalStretch(0);		// % change in horizontal char size.
    aSizePolicy.setVerticalStretch(0);			// % change in vertical char size.
    cpRowColLabel->setSizePolicy(aSizePolicy);	// Fixed size
    cpRowColLabel->setMinimumSize(aMinSize);	// Allow 16 chars
	aFont.setBold(false/*Enable*/);
	cpRowColLabel->setFont(aFont);
	cpRowColLabel->setText(tr("Row:  0 Col:  0"));
	apStatusBar->addPermanentWidget(cpRowColLabel);

	// TabWidget. Connect the tab widget's signals to this MainWindow's slots.
	cpTabWidget = new QTabWidget(this);
	setCentralWidget(cpTabWidget);
	connect(cpTabWidget, SIGNAL(currentChanged(int)), this, SLOT(onCurrentTabChanged(int)));

	// Initialize parameters

	// Open. Open files passed as args, if any.
	long aArgc = QApplication::argc();
	char** apArgv = QApplication::argv();
	cInstallPath = QApplication::applicationFilePath();
	if (aArgc <= 1)
		onNew();		// Launch new file
	else
	{	for (long i = 1; i < aArgc; ++i)
		{	QFileInfo aInfo(apArgv[i]);
			if (addPage(aInfo.absoluteFilePath()))
			{	onCurrentTabChanged(cpTabWidget->currentIndex());
				cpCurPage->editRead(apArgv[i]);
			}
		}
	}
}

/*!
\brief ~AisEdit - Destructor saves current settings

\par Notes:
-# AisEdit has no remote ownership of any referents.
*/
AisEdit::~AisEdit()
{
	// Settings. Save current settings.
	saveSettings();

#ifdef AIS_DEBUG
	qDebug("%s", "~~~~AisEdit()");
#endif
}

//  NOTE! Private methods are not normally included in the documentation, so most of this documentation will not appear
// in the generated HTML pages. It is left here just in case private methods are included at some future date.

/*!
\brief addPage - Initialize new text edit page with tab at top

AisEdit provides the GUI for the ATextEdit editor.  It includes a multi-tabbed window that allows the user to simultaneously
edit multiple text files.
\sa ATextEdit
\par Args:
\param irFileSpec File path + file name
\par Returns:
aNew true if a new page created, false if switch to an existing page.
\par Notes:
-# If FileSpec is empty, set file spec to "Untitled"
*/
bool AisEdit::addPage(const QString& irFileSpec)
{
	// Existing. Switch pages if this file is already open.
	QString aFileSpec(irFileSpec.isEmpty() ? "Untitled" : irFileSpec);
	ATextEdit* apExisting = findChild<ATextEdit *>(aFileSpec);
	if (apExisting != NULL)
	{	if (cpCurPage != apExisting)
		{	cpCurPage = apExisting;
			cpTabWidget->setCurrentWidget(cpCurPage);
		}
	}
	else
	{	// Tab. Add a new tab
		cpCurPage = new ATextEdit(this, aFileSpec);
		cpCurPage->setObjectName(aFileSpec);

		// Set ATextEdit parameters
		cpCurPage->editSetParameter("Font", QVariant(cParameters.mFont));
		cpCurPage->editSetParameter("FontSize", QVariant((int)cParameters.mFontSize));
		cpCurPage->editSetParameter("TabWidth", QVariant((int)cParameters.mTabWidth));
		cpCurPage->editSetParameter("LineNumbers", QVariant(cParameters.mLineNumbers));
		cpCurPage->editSetParameter("ShowWhiteSpace", QVariant(cParameters.mShowWhiteSpace));
		cpCurPage->editSetParameter("WordWrap", QVariant(cParameters.mWrap));
		cpCurPage->editSetParameter("MaxRowLgth", QVariant((int)cParameters.mMaxRowWidth));
		cpCurPage->editSetParameter("MaxUndoDepth", QVariant((int)cParameters.mMaxUndoDepth));

		// Connections. Connect up TextEdit signals
		QString aFileName(QFileInfo(aFileSpec).fileName());	// Strip off the path
		connect(cpCurPage, SIGNAL(cursorPositionChanged(long,long)),this,SLOT(onCursorPositionChanged(long,long)));
		connect(cpCurPage, SIGNAL(cursorStackChanged(bool,bool)),this,SLOT(onCursorStackChanged(bool,bool)));
		connect(cpCurPage, SIGNAL(statusAlert(const QString&)), this, SLOT(onStatusAlert(const QString&)));
		connect(cpCurPage, SIGNAL(textModified(bool)), this, SLOT(onTextModified(bool)));
		connect(cpCurPage, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(onContextMenu()));
		cpTabWidget->addTab(cpCurPage, aFileName + ' ');
		cpTabWidget->setCurrentWidget(cpCurPage);
		cpCurPage->setFocus();
	}
	return (apExisting == NULL);
}

/*!
\brief closeEvent - Reimplemented method inherited from QMainWindow

closeEvent is called when an event is generated that closes the application. close event calls onClose for each page.
\par Args:
\param ipEvent	-> Close event methods
\par Returns:
nothing
\par Notes:
-# Close events are sent to widgets that the user wants to close, usually by choosing "Exit" from the window menu, or by
clicking the 'X' titlebar button. They are also sent when you call QWidget::close() to close a widget programmatically.
-# If close should proceed, call ipEvent->accept(); else, to abort close, call ipEvent->ignore();
-# Note that onClose removes the tabbed page.
*/
void AisEdit::closeEvent(QCloseEvent* ipEvent)
{
	// SaveAs. Allow user to save selected files.
	bool aAccept = true;
	ATextEdit* apPage;
	while ((apPage = qobject_cast<ATextEdit*>(cpTabWidget->currentWidget())) != NULL)
		if (!(aAccept = doClose(apPage)))
			break;
	if (aAccept)
		ipEvent->accept();
	else
		ipEvent->ignore();
}

/*!
\brief contextMenuEvent - Show context menu

\par Args:
\param ipContextEvent -> position of right-mouse click
\par Returns:
nothing
\par Notes:
-# By default, Qt::ContextMenuPolicy is Qt::DefaultContextMenu which causes this routine to be called on right-mouse click.
*/
void AisEdit::contextMenuEvent(QContextMenuEvent* ipContextEvent)
{
	cpContextMenu->exec(ipContextEvent->globalPos());
	ipContextEvent->accept();
}

/* Not currently used. ATextEdit sends a signal on a right mouse click.
void AisEdit::contextMenuEvent(QContextMenuEvent *ipEvent)
{	cpContextMenu->exec(ipEvent->globalPos());} */

/*!
\brief createMenus - Initialize menu and toolbar icons

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::createMenus()
{
	// Edit. Add edit actions.
	QMenuBar* apMenuBar = menuBar();

	QAction* apCommentAct = new QAction(QIcon(":/images/codecomment.png"), tr("Comment"), this);
	apCommentAct->setShortcut(tr("Ctrl+;"));
	apCommentAct->setToolTip("Comment Lines Ctrl-;");
	apCommentAct->setStatusTip("Prefix comment delimiter to highlighted lines");
	connect(apCommentAct, SIGNAL(triggered()), this, SLOT(onComment()));

	QAction* apCopyAct = new QAction(QIcon(":/images/editcopy.png"), tr("Copy"), this);
	apCopyAct->setShortcut(tr("Ctrl+C"));
	apCopyAct->setToolTip("Copy Selected Ctrl-C");
	apCopyAct->setStatusTip("Copy highlighted text to the clipboard");
	connect(apCopyAct, SIGNAL(triggered()), this, SLOT(onCopy()));

	QAction* apCutAct = new QAction(QIcon(":/images/editcut.png"), tr("Cut"), this);
	apCutAct->setShortcut(tr("Ctrl+X"));
	apCutAct->setToolTip("Cut Selected Ctrl-X");
	apCutAct->setStatusTip("Copy highlighted text to clipboard, then delete highlighted text");
	connect(apCutAct, SIGNAL(triggered()), this, SLOT(onCut()));

	QAction* apFindAct = new QAction(QIcon(":/images/editfind.png"), tr("Find"), this);
	apFindAct->setShortcut(tr("Ctrl+F"));
	apFindAct->setToolTip("Open Find Dialog Ctrl-F");
	apFindAct->setStatusTip("Open dialog to find text in current page.");
	connect(apFindAct, SIGNAL(triggered()), this, SLOT(onFind()));

	QAction* apShowLambdasAct = new QAction(QIcon(":/images/codeshowlambdas.png"), tr("Lambda List"), this);
	apShowLambdasAct->setShortcut(tr("Alt+L"));
	apShowLambdasAct->setToolTip("Show Lambdas Alt-L");
	apShowLambdasAct->setStatusTip("Show list of functions/Lambdas and thier line number");
	connect(apShowLambdasAct, SIGNAL(triggered()), this, SLOT(onFunctionList()));

	QAction* apIndentAct = new QAction(QIcon(":/images/codeindent.png"), tr("Indent"), this);
	apIndentAct->setToolTip("Indent Selected Lines Tab");
	apIndentAct->setStatusTip("Prefix tab to highlighted lines.");
	connect(apIndentAct, SIGNAL(triggered()), this, SLOT(onIndent()));

	cpNextAct = new QAction(QIcon(":/images/editforward.png"), tr("Next"), this);
	cpNextAct->setShortcut(tr("Ctrl+PgDown"));
	cpNextAct->setToolTip("Move next Ctrl-PgDown");
	cpNextAct->setStatusTip("Move forward to next cursor position.");
	cpNextAct->setEnabled(false);
	connect(cpNextAct, SIGNAL(triggered()), this, SLOT(onNext()));

	QAction* apOutdentAct = new QAction(QIcon(":/images/codeoutdent.png"), tr("Outdent"), this);
	apOutdentAct->setShortcut(tr("Shift+Tab"));
	apOutdentAct->setToolTip("Unindent Selected Lines Shift-Tab");
	apOutdentAct->setStatusTip("Remove leading tab from selected lines");
	connect(apOutdentAct, SIGNAL(triggered()), this, SLOT(onOutdent()));

	QAction* apPasteAct = new QAction(QIcon(":/images/editpaste.png"), tr("Paste"), this);
	apPasteAct->setShortcut(tr("Ctrl+V"));
	apPasteAct->setToolTip("Paste Clipboard Ctrl-V");
	apPasteAct->setStatusTip("Insert clipboard contents into document at cursor.");
	connect(apPasteAct, SIGNAL(triggered()), this, SLOT(onPaste()));

	cpPrevAct = new QAction(QIcon(":/images/editback.png"), tr("Prev"), this);
	cpPrevAct->setShortcut(tr("Ctrl+PgUp"));
	cpPrevAct->setToolTip("Move previous Ctrl-PgUp");
	cpPrevAct->setStatusTip("Move back to previous cursor position.");
	cpPrevAct->setEnabled(false);
	connect(cpPrevAct, SIGNAL(triggered()), this, SLOT(onPrev()));

	QAction* apPrintAct = new QAction(QIcon(":/images/editprint.png"), tr("Print"), this);
	apPrintAct->setShortcut(tr("Alt+P"));
	apPrintAct->setToolTip("Print Alt-P");
	apPrintAct->setStatusTip("Print the current document on the default printer");
	connect(apPrintAct, SIGNAL(triggered()), this, SLOT(onPrint()));

	QAction* apEditPrefAct = new QAction(tr("Preferences"), this);
	apEditPrefAct->setShortcut(tr("Ctrl+,"));
	apEditPrefAct->setToolTip("Edit Preferences Ctrl-;");
	apEditPrefAct->setStatusTip("Change Editor Preferences");
	connect(apEditPrefAct, SIGNAL(triggered()), this, SLOT(onEditPreferences()));

	QAction* apRedoAct = new QAction(QIcon(":/images/editredo.png"), tr("Redo"), this);
	apRedoAct->setShortcut(tr("Ctrl+Y"));
	apRedoAct->setToolTip("Redo Ctrl-Y");
	apRedoAct->setStatusTip("Reverse the last undo operation.");
	connect(apRedoAct, SIGNAL(triggered()), this, SLOT(onRedo()));

	QAction* apReplaceAct = new QAction(tr("Replace"), this);
	apReplaceAct->setShortcut(tr("Ctrl+R"));
	connect(apReplaceAct, SIGNAL(triggered()), this, SLOT(onReplace()));

	QAction* apUncommentAct = new QAction(QIcon(":/images/codeuncomment.png"), tr("Uncomment"), this);
	apUncommentAct->setShortcut(tr("Ctrl+Shift+;"));
	apUncommentAct->setToolTip("Uncomment Ctrl-Shift-;");
	apUncommentAct->setStatusTip("Remove leading comment delimiter from selected lines.");
	connect(apUncommentAct, SIGNAL(triggered()), this, SLOT(onUncomment()));

	QAction* apUndoAct = new QAction(QIcon(":/images/editundo.png"), tr("Undo"), this);
	apUndoAct->setShortcut(tr("Ctrl+Z"));
	apUndoAct->setToolTip(tr("Undo Ctrl-Z"));
	apUndoAct->setStatusTip(tr("Undo the last edit operation."));
	connect(apUndoAct, SIGNAL(triggered()), this, SLOT(onUndo()));

	QAction* apGoToAct = new QAction(this);
	apGoToAct->setShortcut(tr("Ctrl+G"));
	connect(apGoToAct, SIGNAL(triggered()), this, SLOT(onGoTo()));
	addAction(apGoToAct);

	QAction* apLineNumberAct = new QAction(this);
	apLineNumberAct->setShortcut(tr("Shift+Ins"));
	connect(apLineNumberAct, SIGNAL(triggered()), this, SLOT(onLineNumber()));
	addAction(apLineNumberAct);

	QAction* apWordWrapAct = new QAction(this);
	apWordWrapAct->setShortcut(tr("Ctrl+W"));
	connect(apWordWrapAct, SIGNAL(triggered()), this, SLOT(onWordWrap()));
	addAction(apWordWrapAct);

	QAction* apWhiteSpaceAct = new QAction(this);
	apWhiteSpaceAct->setShortcut(tr("Ins"));
	connect(apWhiteSpaceAct, SIGNAL(triggered()), this, SLOT(onShowWhiteSpace()));
	addAction(apWhiteSpaceAct);

	// Edit Menu and Toolbar
	QMenu* apEditMenu = apMenuBar->addMenu(tr("&Edit"));
	apEditMenu->addAction(cpPrevAct);
	apEditMenu->addAction(cpNextAct);
	apEditMenu->addSeparator();
	apEditMenu->addAction(apFindAct);
	apEditMenu->addAction(apReplaceAct);
	apEditMenu->addSeparator();
	apEditMenu->addAction(apCutAct);
	apEditMenu->addAction(apCopyAct);
	apEditMenu->addAction(apPasteAct);
	apEditMenu->addSeparator();
	apEditMenu->addAction(apUndoAct);
	apEditMenu->addAction(apRedoAct);
	apEditMenu->addSeparator();
	apEditMenu->addAction(apIndentAct);
	apEditMenu->addAction(apOutdentAct);
	apEditMenu->addAction(apCommentAct);
	apEditMenu->addAction(apUncommentAct);
	apEditMenu->addSeparator();
	apEditMenu->addAction(apPrintAct);
	apEditMenu->addSeparator();
	apEditMenu->addAction(apEditPrefAct);

	QToolBar* apEditToolBar = addToolBar(tr("Edit"));
	apEditToolBar->addAction(cpPrevAct);
	apEditToolBar->addAction(cpNextAct);
	apEditToolBar->addAction(apFindAct);
	apEditToolBar->addAction(apCutAct);
	apEditToolBar->addAction(apCopyAct);
	apEditToolBar->addAction(apPasteAct);
	apEditToolBar->addAction(apUndoAct);
	apEditToolBar->addAction(apRedoAct);
	apEditToolBar->addAction(apIndentAct);
	apEditToolBar->addAction(apOutdentAct);
	apEditToolBar->addAction(apCommentAct);
	apEditToolBar->addAction(apUncommentAct);
	apEditToolBar->addAction(apShowLambdasAct);
	apEditToolBar->addAction(apPrintAct);

	// File actions:
	QAction* apCloseAct = new QAction(QIcon(":/images/fileclose.png"), tr("Close"), this);
	apCloseAct->setShortcut(tr("Alt+C"));
	apCloseAct->setToolTip(tr("Close Document"));
	apCloseAct->setStatusTip("Close the current document");
	connect(apCloseAct, SIGNAL(triggered()), this, SLOT(onClose()));

	QAction* apExitAct = new QAction(tr("Exit"), this);
	apExitAct->setShortcut(tr("Alt+X"));
	connect(apExitAct, SIGNAL(triggered()), this, SLOT(onExit()));

	QAction* apNewAct = new QAction(QIcon(":/images/filenew.png"), tr("New"), this);
	apNewAct->setShortcut(tr("Alt+N"));
	apNewAct->setToolTip("New Document");
	apNewAct->setStatusTip("Start edit of an empty document named 'Untitled'");
	connect(apNewAct, SIGNAL(triggered()), this, SLOT(onNew()));

	QAction* apOpenAct = new QAction(QIcon(":/images/fileopen.png"), tr("Open..."), this);
	apOpenAct->setShortcut(tr("Alt+O"));
	apOpenAct->setToolTip("Open File");
	apOpenAct->setStatusTip("Locate and resume edit of an existing file.");
	connect(apOpenAct, SIGNAL(triggered()), this, SLOT(onOpen()));

	QAction* apSaveAct = new QAction(QIcon(":/images/filesave.png"), tr("Save"), this);
	apSaveAct->setShortcut(tr("Ctrl+S"));
	apSaveAct->setToolTip(tr("Save Document"));
	apSaveAct->setStatusTip(tr("Save the document to a file"));
	connect(apSaveAct, SIGNAL(triggered()), this, SLOT(onSave()));

	QAction* apSaveAllAct = new QAction(QIcon(":/images/filesaveall.png"), tr("Save All"), this);
	apSaveAllAct->setShortcut(tr("Ctrl+Shift+S"));
	apSaveAllAct->setToolTip(tr("Save All Documents"));
	apSaveAllAct->setStatusTip(tr("Save all open documents on the disk"));
	connect(apSaveAllAct, SIGNAL(triggered()), this, SLOT(onSaveAll()));

	QAction* apSaveAsAct = new QAction(tr("Save As..."), this);
	connect(apSaveAsAct, SIGNAL(triggered()), this, SLOT(onSaveAs()));

	// File menu and toolbar.
	QMenu* apFileMenu = apMenuBar->addMenu(tr("File"));
	apFileMenu->addAction(apNewAct);
	apFileMenu->addAction(apOpenAct);
	apFileMenu->addAction(apCloseAct);
	apFileMenu->addSeparator();
	apFileMenu->addAction(apSaveAct);
	apFileMenu->addAction(apSaveAllAct);
	apFileMenu->addAction(apSaveAsAct);
	apFileMenu->addSeparator();
	apFileMenu->addAction(apExitAct);

	QToolBar* apFileToolBar = addToolBar(tr("File"));
	apFileToolBar->addAction(apNewAct);
	apFileToolBar->addAction(apOpenAct);
	apFileToolBar->addAction(apCloseAct);
	apFileToolBar->addAction(apSaveAct);
	apFileToolBar->addAction(apSaveAllAct);

	// Help actions
	QAction* apAboutAct = new QAction(tr("About AIS Editor"), this);
	connect(apAboutAct, SIGNAL(triggered()), this, SLOT(onAbout()));

	QAction* apAboutQtAct = new QAction(tr("About Qt"), this);
	connect(apAboutQtAct, SIGNAL(triggered()), this, SLOT(onAboutQt()));

	QAction* apHelpAct = new QAction(QIcon(":/images/mainhelp.png"), tr("Edit Help"), this);
	apHelpAct->setShortcut(tr("F1"));
	apHelpAct->setStatusTip("Show Help");
	apHelpAct->setStatusTip("Show help on edit operations");
	connect(apHelpAct, SIGNAL(triggered()), this, SLOT(onHelp()));

	QAction* apHelpApiAct = new QAction(tr("API Help"), this);
	apHelpApiAct->setShortcut(tr("Ctrl+F1"));
	connect(apHelpApiAct, SIGNAL(triggered()), this, SLOT(onHelpApi()));

	QAction* apHelpFindAct = new QAction(tr("Find Help"), this);
	apHelpFindAct->setShortcut(tr("Shift+F1"));
	connect(apHelpFindAct, SIGNAL(triggered()), this, SLOT(onHelpFind()));

	// Help menu and toolbar.
	QMenu* apHelpMenu = apMenuBar->addMenu(tr("Help"));
	apHelpMenu->addAction(apHelpAct);
	apHelpMenu->addAction(apHelpApiAct);
	apHelpMenu->addAction(apHelpFindAct);
	apHelpMenu->addSeparator();
	apHelpMenu->addAction(apAboutAct);
	apHelpMenu->addAction(apAboutQtAct);

	QToolBar* apHelpToolBar = addToolBar(tr("Help"));
	apHelpToolBar->addAction(apHelpAct);

	// Context menu.
	cpContextMenu = new QMenu("AIS Edit Context Menu", this);
	cpContextMenu->addAction(apNewAct);
	cpContextMenu->addAction(apOpenAct);
	cpContextMenu->addAction(apSaveAct);
	cpContextMenu->addAction(apSaveAsAct);
	cpContextMenu->addAction(apPrintAct);
	cpContextMenu->addAction(apCloseAct);
	cpContextMenu->addSeparator();
	cpContextMenu->addAction(apUndoAct);
	cpContextMenu->addAction(apRedoAct);
	cpContextMenu->addSeparator();
	cpContextMenu->addAction(apCopyAct);
	cpContextMenu->addAction(apCutAct);
	cpContextMenu->addAction(apPasteAct);
	cpContextMenu->addSeparator();
	cpContextMenu->addAction(apIndentAct);
	cpContextMenu->addAction(apOutdentAct);
	cpContextMenu->addSeparator();
	cpContextMenu->addAction(apCommentAct);
	cpContextMenu->addAction(apUncommentAct);
	cpContextMenu->addSeparator();
	cpContextMenu->addAction(apShowLambdasAct);
}

/*!
\brief doClose - Called by onClose or closeEvent when a page is being terminated

\par Args:
\param ipPage -> page being closed
\par Returns:
aIsClosed - true file is closed. It will not be closed if user cancels the close operation.
*/
bool AisEdit::doClose(ATextEdit* ipPage)
{
	bool aIsClosed = true;
	if (ipPage == NULL)
		onStatusAlert("No page is open. Nothing to close");
	else
	{	// Save. Ask user and then save.
		if (ipPage->editIsModified())
		{	QString aFileSpec(ipPage->objectName());
			QString aFileName(QFileInfo(aFileSpec).fileName());
			QString aCaption = QString("Closing %1").arg(aFileSpec);
			QString aText = QString("%1 has been modified.\nDo you wish to save the changes?").arg(aFileName);
			switch (QMessageBox::information(this, aCaption, aText, QMessageBox::Yes, QMessageBox::No, QMessageBox::Cancel))
			{
			case QMessageBox::Yes:		// Save and close file
				onSave(ipPage);
				break;
			case QMessageBox::No:		// Don't save, but close file.
				break;
			case QMessageBox::Cancel:	// Don't save and don't close.
				aIsClosed = false;
				break;
			}
		}
		// Remove. Remove will trigger onCurrentTabChanged.
		if (aIsClosed)
		{	if (ipPage == cpCurPage)
				cpCurPage = NULL;
			cpTabWidget->removeTab(cpTabWidget->indexOf(ipPage));
			delete ipPage;
		}
	}
	return aIsClosed;
}

/*!
 * \brief Load the application settings.
 */
void AisEdit::loadSettings()
{
	QSize aDefaultSize(AED_DEFSCREENWIDTH, AED_DEFSCREENHEIGHT);

	// Settings. Restore settings.
	cSettings.beginGroup("Main Window");
	cSize = cSettings.value("Size", QVariant(aDefaultSize)).toSize();
	cOrigin = cSettings.value("Position", QVariant(QPoint(0,0))).toPoint();
	if (cOrigin.x() < 0)
		cOrigin.setX(0);
	if (cOrigin.y() < 0)
		cOrigin.setY(0);
	cSettings.endGroup();

	cSettings.beginGroup("Text Editor");
	cParameters.mFont = cSettings.value("Font", QVariant(ATextEdit::scDefaultFont)).toString();
	cParameters.mFontSize = cSettings.value("FontSize", QVariant((int)ATextEdit::scDefaultFontSize)).toInt();
	cParameters.mLineNumbers = cSettings.value("LineNumbers", QVariant(false)).toBool();
	cParameters.mMaxRowWidth = cSettings.value("MaxRowLgth", QVariant((int)ATextEdit::scDefaultMaxRowWidth)).toInt();
	cParameters.mMaxUndoDepth = cSettings.value("MaxUndoDepth", QVariant((int)ATextEdit::scDefaultMaxUndoDepth)).toInt();
	cParameters.mShowWhiteSpace = cSettings.value("ShowWhiteSpace", QVariant(false)).toBool();
	cParameters.mTabWidth = cSettings.value("TabWidth", QVariant((int)ATextEdit::scDefaultTabWidth)).toInt();
	cParameters.mWrap = cSettings.value("WordWrap", QVariant(false)).toBool();
	cSettings.endGroup();
}

/*!
\brief onAbout - Information about AIS Edit

\par Args:
none
\par Returns:
nothing
Notes:
 1.	A comment delimiter is prepended to all lines that are selected or partially selected.
 2.	For example, a semicolon is prepended if AisLisp, a double slash (//) is prepended if C or C++
*/
void AisEdit::onAbout()
{
	QString aAbout = QString("<b>AIS Editor</b><br>"
		"Copyright &copy; 2003-2009 All rights reserved. Investment Science Corporation<br>"
		"AIS Edit Version " AGBL_AISVERSION "<br>"
		"Installed at %1<br><br>"
		"<b>Features</b><br>"
		"Ais Editor is a fast, flexible screen editor:<br><ol>"
		"<li>Edits large files gracefully.</li>"
		"<li>Insert, move, delete, undo, redo.</li>"
		"<li>SmartLisp, JavaScript syntax highlighting</li>"
		"<li>Matches parens, braces, quotes.</li>"
		"<li>Supports word wrap, tabs, paragraph attributes</li>"
		"<li>Toolbars, menus, customization (tab width, font size)</li>"
		"<li>Context menus</li>"
		"<li>Find/replace using regular expressions</li>"
		"<li>Fixed-width fonts only. No font styles within a paragraph (bold, underline...)</li></ol>"
		"<b>For More Information</b><ul>"
		"<li>Select Help in this menu.</li>"
		"<li>Visit us at <a href=https://sourceforge.net/projects/aiserver>https://sourceforge.net/projects/aiserver</a></li></ul>"
		).arg(cInstallPath);
	QMessageBox::about(this, tr("About AIS Editor"), aAbout);
}

void AisEdit::onAboutQt()
{
	QMessageBox::aboutQt(this, tr("About Qt"));
}

/*!
\brief onClose - Called by action signal when the close menu item or close icon selected

\par Args:
none
\par Returns:
nothing
\par Note:
-# If operation cancelled in doClose, cpCurPage will not be NULL.
*/
void AisEdit::onClose()
{
	doClose(cpCurPage);
	if (cpCurPage == NULL)
		onNew();
}

/*!
\brief onComment - Called by action signal when comment key or comment icon selected

\par Args:
none
\par Returns:
nothing
\par Notes:
-# A comment delimiter is prepended to all lines that are selected or partially selected.
-# For example, a semicolon is prepended if AisLisp, a double slash (//) is prepended if C or C++
*/
void AisEdit::onComment()
{
	if (cpCurPage == NULL)
		onStatusAlert("No page is open. Use New or Open to create a text page");
	else
		cpCurPage->editComment(true/*In*/);
}

/*!
\brief onContextMenu - Called by signal requesting a context menu for this page.

\par Args:
none
\par Returns:
nothing
\par Notes:
-# Typically called by a right-button mouse click
*/
void AisEdit::onContextMenu()
{
	cpContextMenu->exec(QCursor::pos());
}

/*!
\brief onCopy - Called by action signal when copy key or copy icon selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onCopy()
{
	if (cpCurPage == NULL)
		onStatusAlert("No page is open. Use New or Open to create a text page");
	else
		cpCurPage->editCopy();
}

/*!
\brief onCurrentTabChanged - Called by tab widget when a new page is selected.

\param iTabIdx Tab index 
\return void
*/
void AisEdit::onCurrentTabChanged(int iIx)
{
	// Open. No pages left. Ask user to open a page
	if (iIx < 0)
		onNew();
	else // Switch. Switch another page.
	{	cpCurPage = qobject_cast<ATextEdit*>(cpTabWidget->widget(iIx));
		QString aCaption(cpCurPage->objectName() + "[*]");
		bool aIsModified = cpCurPage->isWindowModified();
		setWindowModified(aIsModified);
		setWindowTitle(aCaption);
	}
}

/*!
\brief onCursorPositionChanged - Called from text editor when cursor position changes

\par Args:
\param iX Horizontal offset from left side in chars.
\param iY Vertical offset from top in rows
\return void
*/
void AisEdit::onCursorPositionChanged(long iX, long iY)
{
	cpRowColLabel->setText(QString("Row:%1 Col:%2").arg(iY + 1, -3).arg(iX + 1, -3));
}

/*!
\brief onCursorStackChanged - Called from text editor when cursor stack changes

Called when either the number of previous or forward positions in the stack changes from zero to non-zero or vice versa.
\par Args:
\param iForward Enable/disable forward cursor operation
\param iPrevious Enable/disable previous cursor operation
\return void
*/
void AisEdit::onCursorStackChanged(bool iForward, bool iPrevious)
{
	cpNextAct->setEnabled(iForward);
	cpPrevAct->setEnabled(iPrevious);
}

/*!
\brief onCut - Called by action signal when cut key or cut icon selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onCut()
{
	if (cpCurPage == NULL)
		onStatusAlert("No page is open. Use New or Open to create a text page");
	else
	 	cpCurPage->editCut();
}

/*!
 * \brief Displays the editor preference dialog and applies the changes
 * to all active documents.
 */
void AisEdit::onEditPreferences()
{
	AParameters aAisEditParams;
	bool aFixFont = false;
	bool aFixFontSize = false;
	bool aFixLineNum = false;
	bool aFixWrap = false;
	bool aFixWhiteSpace = false;
	bool aFixTabWidth = false;

	aAisEditParams.mFont = cParameters.mFont;
	aAisEditParams.mFontSize = cParameters.mFontSize;
	aAisEditParams.mLineNumbers = cParameters.mLineNumbers;
	aAisEditParams.mWrap = cParameters.mWrap;
	aAisEditParams.mShowWhiteSpace = cParameters.mShowWhiteSpace;
	aAisEditParams.mTabWidth = cParameters.mTabWidth;

	// Create the dialog if necessary
	if (cpPrefDialog == 0)
		cpPrefDialog = ATextEdit::createPrefDialog(this, "Editor Preferences");

	if (cpPrefDialog == 0)
	{
		qDebug("cpPrefDialog is NULL");
		return;
	}

	cpPrefDialog->setParameters(&aAisEditParams);

	// Display the dialog
	if (cpPrefDialog->exec() == QDialog::Accepted)
	{
		// For each opened page, change the font settings
		cpPrefDialog->getParameters(&aAisEditParams);

		// If font was changed
		if (cParameters.mFont != aAisEditParams.mFont)
		{
			cParameters.mFont = aAisEditParams.mFont;
			aFixFont = true;
		}

		// If font size was changed
		if (cParameters.mFontSize != aAisEditParams.mFontSize)
		{
			cParameters.mFontSize = aAisEditParams.mFontSize;
			aFixFontSize = true;
		}

		// If line number setting was changed
		if (cParameters.mLineNumbers != aAisEditParams.mLineNumbers)
		{
			cParameters.mLineNumbers = aAisEditParams.mLineNumbers;
			aFixLineNum = true;
		}

		// If word wrap setting was changed
		if (cParameters.mWrap != aAisEditParams.mWrap)
		{
			cParameters.mWrap = aAisEditParams.mWrap;
			aFixWrap = true;
		}

		// If white space setting was changed
		if (cParameters.mShowWhiteSpace != aAisEditParams.mShowWhiteSpace)
		{
			cParameters.mShowWhiteSpace = aAisEditParams.mShowWhiteSpace;
			aFixWhiteSpace = true;
		}

		// If tab width value was changed
		if (cParameters.mTabWidth != aAisEditParams.mTabWidth)
		{
			cParameters.mTabWidth = aAisEditParams.mTabWidth;
			aFixTabWidth = true;
		}

		if (aFixFont || aFixFontSize || aFixLineNum || aFixWrap || aFixWhiteSpace || aFixTabWidth )
		{
			int aIdx = 0;
			int aSz = cpTabWidget->count();
			ATextEdit *apTextEdit = 0;	

			for (aIdx = 0; aIdx < aSz; aIdx++)
			{
				apTextEdit = qobject_cast<ATextEdit*>(cpTabWidget->widget(aIdx));
				if (apTextEdit != NULL)
				{
					if (aFixFont)
						apTextEdit->editSetParameter("Font", QVariant(cParameters.mFont));

					if (aFixFontSize)
						apTextEdit->editSetParameter("FontSize", QVariant((int)cParameters.mFontSize));

					if (aFixTabWidth)
						apTextEdit->editSetParameter("TabWidth", QVariant((int)cParameters.mTabWidth));

					if (aFixLineNum)
						apTextEdit->editSetParameter("LineNumbers", QVariant(cParameters.mLineNumbers));

					if (aFixWrap)
						apTextEdit->editSetParameter("WordWrap", QVariant(cParameters.mWrap));

					if (aFixWhiteSpace)
						apTextEdit->editSetParameter("ShowWhiteSpace", QVariant(cParameters.mShowWhiteSpace));
				}
			}
		}
	}
}

/*!
\brief onExit - Called by action signal when exit key or exit is selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onExit()
{
	close();		// See closeEvent
}

/*!
\brief onFind - Called by action signal when cut key or cut icon selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onFind()
{
	// Load the Find Dialog
	if (cpFindDialog == NULL)
	{
		cpFindDialog = ATextEdit::createFindDialog(this, "Find Dialog");
		connect(cpFindDialog, SIGNAL(findPattern(const QString&, bool, bool, bool, bool, bool, bool)),
				this, SLOT(onFindPattern(const QString&, bool, bool, bool, bool, bool, bool)));
		connect(this, SIGNAL(findPatternResult(bool)),
				cpFindDialog, SLOT(onFindPatternResult(bool)));
	}

	if (cpCurPage != NULL)
	{
		cpFindDialog->init(&cSettings, cpCurPage);
	}

	cpFindDialog->showOnTop();
}

/*!
 * \brief Handles the findPattern signal.
 */
void AisEdit::onFindPattern(const QString& irPattern, bool iAll, bool iMatchCase, bool iDown, bool iRegExp, bool iSelect, bool iMatchWord)
{
	bool iResult = false;
	if (cpCurPage == NULL)
	{
		onStatusAlert("No page is open. Use New or Open to create a text page");
	}
	else
	{
		iResult = cpCurPage->editFind(irPattern, iAll, iMatchCase, iDown, iRegExp, iSelect, iMatchWord);
		emit findPatternResult(iResult);
	}
}

/*!
\brief onFunctionList - Show list of functions and line numbers. Allow user to move to a function

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onFunctionList()
{
	// Load the Function List Dialog
	if (cpFcnListDialog == NULL)
	{
		cpFcnListDialog = ATextEdit::createFcnListDialog(this, "Lambda List Dialog");
	}

	// Get list of Lambdas or functions
	QStringList aFcnList = cpCurPage->editFunctionList();
	if (aFcnList.isEmpty())
	{
		onStatusAlert("Function List: No functions / Lambdas found");
	}
	else
	{
		// List is not empty, display it
		cpFcnListDialog->init(cpCurPage, aFcnList);
		cpFcnListDialog->showOnTop();
	}
}

/*!
 * \brief Displays the go to dialog.
 */
void AisEdit::onGoTo()
{
	// Load the Go To Line Dialog
	if (cpGoDialog == NULL)
	{
		cpGoDialog = ATextEdit::createGoDialog(this, "Go Dialog");
		connect(cpGoDialog, SIGNAL(goToLine(long)),
				this, SLOT(onGoToLine(long)));
	}

	cpGoDialog->init();
	cpGoDialog->showOnTop();
}

/*!
 * \brief Handles the goToLine signal.
 */
void AisEdit::onGoToLine(long iLineNum)
{
	if (cpCurPage == NULL)
		onStatusAlert("No page is open. Use New or Open to create a text page");
	else
		cpCurPage->editGoToLine(iLineNum);
}

/*!
\brief onHelp - Called by action signal when cut key or cut icon selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onHelp()
{
	cpCurPage->dialogHelp();
}

/*!
\brief onHelpApi - Called by action signal when cut key or cut icon selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onHelpApi()
{
	cpCurPage->dialogHelpApi();
}


/*!
\brief onHelpFind - Called by action signal when cut key or cut icon selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onHelpFind()
{
	cpCurPage->dialogHelpFind();
}

/*!
\brief onIndent - Called by action signal when indent menu item or indent icon selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onIndent()
{
	if (cpCurPage == NULL)
		onStatusAlert("No page is open. Use New or Open to create a text page");
	else
		cpCurPage->editIndent(true/*Insert*/);
}

/*!
 * \brief Activates/Deactives the display of line numbers.
 */
void AisEdit::onLineNumber()
{
	int aIdx = 0;
	int aSz = cpTabWidget->count();
	ATextEdit *apTextEdit = 0;		

	cParameters.mLineNumbers = !cParameters.mLineNumbers;

	for (aIdx = 0; aIdx < aSz; aIdx++)
	{
		apTextEdit = qobject_cast<ATextEdit*>(cpTabWidget->widget(aIdx));
		apTextEdit->editSetParameter("LineNumbers", QVariant(cParameters.mLineNumbers));
	}

	onStatusAlert((cParameters.mLineNumbers)? "Line Numbers ON": "Line Numbers OFF");
}

/*!
\brief onNew - Called by action signal when new key or new icon selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onNew()
{
	addPage("Untitled");
}

/*!
\brief onNext - Called by action signal when the move next menu or toolbar icon selected.

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onNext()
{
	if (cpCurPage == NULL)
		onStatusAlert("No page is open. Use New or Open to create a text page");
	else
		cpCurPage->editMove(ATextEdit::eNextCur, false/*Select*/);
}

/*!
\brief onOpen - Called by action signal when open key or open icon selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onOpen()
{
	QString aFileSpec;
	if (!(aFileSpec = ATextEdit::dialogOpen(this)).isEmpty() && addPage(aFileSpec)&& !cpCurPage->editRead(aFileSpec))
			onStatusAlert("Warning! This file has been truncated.");
}

/*!
\brief onOutdent - Called by action signal when unindent key or unindent icon selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onOutdent()
{
	if (cpCurPage == NULL)
		onStatusAlert("No page is open. Use New or Open to create a text page");
	else
		cpCurPage->editIndent(false/*Insert*/);
}

/*!
\brief onPaste - Called by action signal when paste key or paste icon selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onPaste()
{
	if (cpCurPage == NULL)
		onStatusAlert("No page is open. Use New or Open to create a text page");
	else
	 	cpCurPage->editPaste();
}

/*!
\brief onPrev - Called by action signal when the move previous menu or toolbar icon selected.

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onPrev()
{
	if (cpCurPage == NULL)
		onStatusAlert("No page is open. Use New or Open to create a text page");
	else
		cpCurPage->editMove(ATextEdit::ePrevCur, false/*Select*/);
}

/*!
\brief onPrint - Called by action signal when print menu item or print icon selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onPrint()
{
	bool aPrinter = true;
	#ifdef QT_NO_PRINTER
		aPrinter = false;
	#endif	// QT_NO_PRINTER

	if (!aPrinter || cpCurPage == NULL)
	{	QString aText;
		if (!aPrinter)
			aText = "No printer is available for this application";
		else
			aText = "No page is open. Use New or Open to create a text page";
		onStatusAlert(aText);
	}
	else
		cpCurPage->editPrint();
}

/*!
\brief onRedo - Called by action signal when redo key or redo icon selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onRedo()
{
	if (cpCurPage == NULL)
		onStatusAlert("No page is open. Use New or Open to create a text page");
	else
	 	cpCurPage->editRedo(true/*ToMark*/);
}

/*!
\brief onReplace - Called by action signal when replace menu item is selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onReplace()
{
	// Load the Replace Dialog
	if (cpReplaceDialog == NULL)
	{
		cpReplaceDialog = ATextEdit::createReplaceDialog(this, "Replace Dialog");
		connect(cpReplaceDialog, SIGNAL(replaceFindPattern(const QString&, bool, bool, bool, bool, bool, bool)),
				this, SLOT(onReplaceFindPattern(const QString&, bool, bool, bool, bool, bool, bool)));
		connect(cpReplaceDialog, SIGNAL(replacePattern(const QString&, const QString&, bool, bool, bool, bool)),
				this, SLOT(onReplacePattern(const QString&, const QString&, bool, bool, bool, bool)));
		connect(cpReplaceDialog, SIGNAL(replaceAllPattern(const QString&, const QString&, bool, bool, bool, bool, bool)),
				this, SLOT(onReplaceAllPattern(const QString&, const QString&, bool, bool, bool, bool, bool)));

		connect(this, SIGNAL(replaceFindPatternResult(bool)),
				cpReplaceDialog, SLOT(onReplaceFindPatternResult(bool)));
		connect(this, SIGNAL(replacePatternResult(bool)),
				cpReplaceDialog, SLOT(onReplacePatternResult(bool)));
	}

	if (cpCurPage != NULL)
	{
		cpReplaceDialog->init(&cSettings, cpCurPage);
	}

	cpReplaceDialog->showOnTop();
}

void AisEdit::onReplaceFindPattern(const QString& irPattern, bool iAll, bool iMatchCase,
							bool iDown, bool iRegExp, bool iSelect, bool iMatchWord)
{
	bool iResult = false;
	if (cpCurPage == NULL)
	{
		onStatusAlert("No page is open. Use New or Open to create a text page");
	}
	else
	{
		iResult = cpCurPage->editFind(irPattern, iAll, iMatchCase, iDown, iRegExp, iSelect, iMatchWord);
		emit replaceFindPatternResult(iResult);
	}
}

void AisEdit::onReplacePattern(const QString& irPattern, const QString& irText,
								bool iAllText, bool iMatchCase, bool iRegExp, bool iMatchWord)
{
	bool iResult = false;
	if (cpCurPage == NULL)
	{
		onStatusAlert("No page is open. Use New or Open to create a text page");
	}
	else
	{
		iResult = cpCurPage->editReplace(irPattern, irText, iAllText, iMatchCase, iRegExp, iMatchWord);
		emit replacePatternResult(iResult);
	}
}

void AisEdit::onReplaceAllPattern(const QString& irPattern, const QString& irText,
								bool iAllText, bool iMatchCase, bool iRegExp, bool iSelect, bool iMatchWord)
{
	bool iResult = false;
	if (cpCurPage == NULL)
	{
		onStatusAlert("No page is open. Use New or Open to create a text page");
	}
	else
	{
		iResult = cpCurPage->editReplaceAll(irPattern, irText, iAllText, iMatchCase, iRegExp, iSelect, iMatchWord);
		emit replacePatternResult(iResult);
	}
}

/*!
\brief onSave - Called by action signal when save menu item or file-save icon selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onSave()
{
	onSave(cpCurPage);
}
void AisEdit::onSave(ATextEdit* ipPage)
{
	QString aFileSpec;
	if (ipPage == NULL)
		onStatusAlert("No page is open. Nothing to save.");
	else if ((aFileSpec =ipPage->objectName()) == "Untitled")
		onSaveAs();
	else
		ipPage->editWrite(aFileSpec);
}

/*!
\brief onSaveAll - Called by action signal when save menu item or file-save icon selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onSaveAll()
{
	long aIx, aNPages = cpTabWidget->count();		// Number of text edit pages.
	if (aNPages <= 0)
		onStatusAlert("No pages opened. Use New or Open to create a text page.");
	else
	{	ATextEdit* apPage;
		QString aFileSpec;
		for (aIx = 0; aIx < aNPages; ++aIx)
		{	if ((apPage = qobject_cast<ATextEdit*>(cpTabWidget->widget(aIx))) != NULL)
			{	if ((aFileSpec = apPage->objectName()) == "Untitled")
					onSaveAs(apPage);
				else
					onSave(apPage);
			}
		}
	}
}

/*!
\brief onSaveAs - Called by action signal when saveAs menu item is selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onSaveAs() { onSaveAs(cpCurPage);}
void AisEdit::onSaveAs(ATextEdit* ipPage)
{
	if (ipPage == NULL)
		onStatusAlert("No page is open. Use New or Open to create a text page");
	else
	{	for (;;)
		{	QString aFileSpec = ipPage->dialogSaveAs();
			if (aFileSpec.isEmpty())			// User selected cancel
				break;
			else if (aFileSpec == "Untitled")
				onStatusAlert("Untitled reserved for new files. Please choose a different name");
			else
			{	long aIx, aNPages = cpTabWidget->count();
				ATextEdit* apPage;
				for (aIx = 0; aIx < aNPages; ++aIx)
				{	apPage = qobject_cast<ATextEdit*>(cpTabWidget->widget(aIx));
					if (ipPage != apPage && apPage->objectName() == aFileSpec)
					{	onStatusAlert("This file already in use. Please choose different file name");
						break;
					}
				}
				if (aIx >= aNPages)
				{	// Save. Save file. Update window title, tab label, page name.
					QString aFileName(QFileInfo(aFileSpec).fileName() + ' '); // Strip off the path, add space
					cpTabWidget->setTabText(cpTabWidget->indexOf(ipPage), aFileName);
					ipPage->setObjectName(aFileSpec);
					if (cpCurPage == ipPage)
					{	QString aTitle(aFileSpec + "[*]");
						setWindowModified(false/*Modified*/);
						setWindowTitle(aTitle);
					}
					onSave(ipPage);
					break;
				}
			}
		}
	}
}

/*!
 * \brief Activates/Deactivates the display of white spaces.
 */
void AisEdit::onShowWhiteSpace()
{
	int aIdx = 0;
	int aSz = cpTabWidget->count();
	ATextEdit *apTextEdit = 0;		

	cParameters.mShowWhiteSpace = !cParameters.mShowWhiteSpace;

	for (aIdx = 0; aIdx < aSz; aIdx++)
	{
		apTextEdit = qobject_cast<ATextEdit*>(cpTabWidget->widget(aIdx));
		apTextEdit->editSetParameter("ShowWhiteSpace", QVariant(cParameters.mShowWhiteSpace));
	}
	
	onStatusAlert((cParameters.mShowWhiteSpace)? "Show Whitespace ON": "Show Whitespace OFF");
}

/*!
\brief onStatusAlert - Called by action signal on an operation that cannot be implemented

\par Args:
\param irMsg Message to be displayed on status line
\par Returns:
nothing
*/
void AisEdit::onStatusAlert(const QString& irMsg)
{
	cpAlertLabel->setText(irMsg);
	if (!irMsg.isEmpty())
		QApplication::beep();
}

/*!
\brief onTextModified - Called by action signal when text is modified or when text is saved.

\par Args:
\param iModified True iff the text has been modified.
\par Returns:
nothing
\par Notes:
-# onTextModified is called with iModified set to false whenever the text is saved.
*/
void AisEdit::onTextModified(bool iModified)
{
	// MainWindow Caption. Update trailing [*] in caption.
	setWindowModified(iModified);

	// Tab Label. Update trailing character of tab label.
	QChar aCh(iModified ? '*' : ' ');
	long aIx = cpTabWidget->currentIndex();
	QString aFileName(cpTabWidget->tabText(aIx));
	long aEnd = aFileName.length() - 1;
	aFileName[(int)aEnd] = aCh;
	cpTabWidget->setTabText(aIx, aFileName);
}

/*!
\brief onUncomment - Called by action signal when uncomment key or uncomment icon selected

\par Args:
none
\par Returns:
nothing
\par Notes:
-# The comment delimiter is dependent upon the current language
*/
void AisEdit::onUncomment()
{
	if (cpCurPage == NULL)
		onStatusAlert("No page is open. Use New or Open to create a text page");
	else
		cpCurPage->editComment(false/*In*/);
}

/*!
\brief onUndo - Called by action signal when undo key or undo icon selected

\par Args:
none
\par Returns:
nothing
*/
void AisEdit::onUndo()
{
	if (cpCurPage == NULL)
		onStatusAlert("No page is open. Use New or Open to create a text page");
	else
		cpCurPage->editUndo(true/*ToMark*/);
}

/*!
 * \brief Handle the Word Wrap switch.
 */
void AisEdit::onWordWrap()
{
	int aIdx = 0;
	int aSz = cpTabWidget->count();
	ATextEdit *apTextEdit = 0;		

	cParameters.mWrap = !cParameters.mWrap;

	for (aIdx = 0; aIdx < aSz; aIdx++)
	{
		apTextEdit = qobject_cast<ATextEdit*>(cpTabWidget->widget(aIdx));
		apTextEdit->editSetParameter("WordWrap", QVariant(cParameters.mWrap));
	}
	
	onStatusAlert((cParameters.mWrap)? "Word Wrap ON": "Word Wrap OFF");
}

/*!
 * \brief Save the application settings.
 */
void AisEdit::saveSettings()
{
	cSettings.beginGroup("Main Window");
	cSettings.setValue("Size", QVariant(size()));
	cSettings.setValue("Position", QVariant(pos()));
	cSettings.endGroup();

	cSettings.beginGroup("Text Editor");
	cSettings.setValue("Font", QVariant(cParameters.mFont));					// Font Family
	cSettings.setValue("FontSize", QVariant((int)cParameters.mFontSize));		// Font Size [pixels]
	cSettings.setValue("LineNumbers", QVariant((int)cParameters.mLineNumbers));	// Show line numbers
	cSettings.setValue("MaxRowLgth",QVariant((int)cParameters.mMaxRowWidth));	// Maximum display width if no wrap [chars]
	cSettings.setValue("MaxUndoDepth", QVariant((int)cParameters.mMaxUndoDepth));// Maximum number of saved operations	
	cSettings.setValue("ShowWhiteSpace",QVariant(cParameters.mShowWhiteSpace));	// Display tabs, newlines
	cSettings.setValue("TabWidth", QVariant((int)cParameters.mTabWidth));		// Default tab width [char]
	cSettings.setValue("WordWrap", QVariant((int)cParameters.mWrap));			// Enable word wrap
	cSettings.endGroup();
}

// end
