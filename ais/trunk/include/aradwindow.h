/**********************************************************************************
    Copyright (C) 2013 AIS Foundation.

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

#ifndef Aradwindow_H
#define Aradwindow_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/Aradwindow.h
															Main Window

CHANGE HISTORY
Version	Date		Who		Change
1.0000	2/14/2013	mfk 	First experiments with remote console window object.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QMenuBar>
#include <QtGui/QMenu>
#include <QtCore/QProcess>
#include <QtCore/QSettings>
#include <QtGui/QIcon>
#include <QtGui/QMainWindow>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QMessageBox>
#include <QtGui/QTextEdit>
#include <QtGui/QLineEdit>
#include <QtGui/QHBoxLayout>
#include <QtGui/QvBoxLayout>
#include <QtGui/QScrollArea>
#include "aparameters.h"


class QMenuBar;
class QLineEdit;
class QPushButton;
class QTextEdit;

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
\brief Aradwindow - Implements the main application window.

This is remote AIS console object meant to be manipulated by both local and remote contexts.
 */
class ARadWindow : public QMainWindow
{
  	Q_OBJECT

public:
//  Properties
	QMenuBar*		cpMenuBar;
	QLineEdit*		cpCommand;
	QPushButton*	cpRunButton;
	QTextEdit*      cpConsole;

//	Methods
	ARadWindow(const QString& irTitle);
	void createMenu();
	
private:
};

#endif
