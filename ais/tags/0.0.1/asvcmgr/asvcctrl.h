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

#ifndef ASVCCTRL_H
#define ASVCCTRL_H

#include <QObject>
#include <QString>

class AServiceControlWidget;
class AService;
class QTimer;

class AServiceControl : public QObject
{
	Q_OBJECT

	public:
		AServiceControl(QObject *ipParent = NULL);
		virtual ~AServiceControl();

		// Sets the Widget Interface Object
		void setServiceWidget(AServiceControlWidget *ipWidget);
		void setServiceObject(AService *ipService);
		void setServicePath(const QString &irFilePath);

	private slots:
		void onStart();
		void onStop();
		void onInstall();
		void onRemove();
		void onTimeout();
		
	private:
		AServiceControlWidget *cpWidget;	// Pointer to Widget Interface Object
		AService *cpService;				// Pointer to Service Interface Object
		QString cFilePath;					// File Path
		QTimer *cpTimer;					// Timer Object
};

#endif
