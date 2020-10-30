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
aisdev/aradgui/aradwindow.cpp
aisdev/include/aradwindow,h
														Main Window

CHANGE HISTORY
Version	Date		Who		Change
1.0000	2/14/2013	mfk 	First experiments with rad console window object.
												 
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QProcess>
#include <QtCore/QSettings>
#include <QtGui/QIcon>
#include <QtGui/QMainWindow>

#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QMessageBox>
#include <QtGui/QHBoxLayout>
#include <QtGui/QSlider>
#include <QtGui/QSpinBox>

#include "aradwindow.h"
#include "aparameters.h"
//	------------------------------------------------------ METHODS -------------------------------------------------------------
/*!
 * \brief Constructor instantiates the remote console window when invoked.
 *
 * \return void
 */
ARadWindow::ARadWindow(const QString& irTitle)
{
	setWindowTitle("AIS - Analytic Information Server");
	cpCommand = new QLineEdit;
	cpRunButton = new QPushButton("Run");
	QHBoxLayout *commandLayout = new QHBoxLayout;
	commandLayout->addWidget(cpRunButton);
	commandLayout->addWidget(cpCommand);

	QScrollArea *scrollArea = new QScrollArea;
	cpConsole = new QTextEdit;
	scrollArea->setWidget(cpConsole);
	scrollArea->setWidgetResizable(true);
	QVBoxLayout *mainLayout = new QVBoxLayout;
	mainLayout->addLayout(commandLayout);
	mainLayout->addWidget(scrollArea);
//	mainLayout->setMenuBar(cpMenuBar);

//	createMenu();
	setLayout(mainLayout);
	show();
}

 void ARadWindow::createMenu()
 {
     cpMenuBar = new QMenuBar;

     QMenu *fileMenu = new QMenu(tr("&File"), this);
     QAction *exitAction = fileMenu->addAction(tr("E&xit"));
     cpMenuBar->addMenu(fileMenu);

     connect(exitAction, SIGNAL(triggered()), this, SLOT(accept()));
 }


