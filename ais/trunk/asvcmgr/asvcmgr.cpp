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

#include "asvcmgr.h"
#include "aservicecontrolwidget.h"
#include "aservice.h"
#include "asvcctrl.h"
#include "asvcmgr.h"

ASvcMgr::ASvcMgr(QWidget* ipParent, Qt::WFlags iFlgs) :
	QMainWindow(ipParent, iFlgs), cpWidget(NULL), cpControl(NULL), cpService(NULL)
{
	cpWidget = new AServiceControlWidget(this);
	cpControl = new AServiceControl();
	cpService = new AService();

	cpControl->setServicePath(QApplication::applicationDirPath());
	cpControl->setServiceObject(cpService);
	cpControl->setServiceWidget(cpWidget);
	
	setCentralWidget(cpWidget);
}

ASvcMgr::~ASvcMgr()
{
	delete cpService;
	delete cpControl;
}
