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

#ifndef ASVCMGR_H
#define ASVCMGR_H

#include <QtGui/QMainWindow>

class AServiceControlWidget;
class AServiceControl;
class AService;

class ASvcMgr : public QMainWindow
{
	Q_OBJECT
	
	public:
		ASvcMgr (QWidget* ipParent = 0, Qt::WFlags iFlgs = 0);
		virtual ~ASvcMgr();
		
	private:
		AServiceControlWidget *cpWidget;
		AServiceControl *cpControl;
		AService *cpService;
};

#endif
