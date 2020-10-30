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

#include <QTimer>
#include <QMessageBox>

#include "asvcctrl.h"
#include "aservice.h"
#include "aissvc.h"
#include "aservicecontrolwidget.h"

AServiceControl::AServiceControl(QObject *ipParent) : QObject(ipParent), cpWidget(NULL), cpService(NULL), cpTimer(NULL)
{
	cpTimer = new QTimer(this);
	
	connect(cpTimer, SIGNAL(timeout()), this, SLOT(onTimeout()));
	cpTimer->setInterval(2000);
	cpTimer->start();
}

AServiceControl::~AServiceControl()
{

}

void AServiceControl::setServiceWidget(AServiceControlWidget *ipWidget)
{
	if (cpWidget == NULL)
	{
		cpWidget = ipWidget;
	}
	else if (cpWidget != ipWidget)
	{
		// disconnect old connections
		disconnect(cpWidget, SIGNAL(start()), this, SLOT(onStart()));
		disconnect(cpWidget, SIGNAL(stop()), this, SLOT(onStop()));
		disconnect(cpWidget, SIGNAL(install()), this, SLOT(onInstall()));
		disconnect(cpWidget, SIGNAL(remove()), this, SLOT(onRemove()));

		cpWidget = ipWidget;
	}
	else
	{
		// same dialog, do nothing
		return;
	}
	
	// setup new connections
	connect(cpWidget, SIGNAL(start()), this, SLOT(onStart()));
	connect(cpWidget, SIGNAL(stop()), this, SLOT(onStop()));
	connect(cpWidget, SIGNAL(install()), this, SLOT(onInstall()));
	connect(cpWidget, SIGNAL(remove()), this, SLOT(onRemove()));
	
	cpWidget->setServiceName(AIS_DEFAULT_SERVICE_NAME);
	onTimeout();
}

void AServiceControl::setServiceObject(AService *ipService)
{
	cpService = ipService;
}

void AServiceControl::setServicePath(const QString &irFilePath)
{
	cFilePath = irFilePath + "/" AIS_DEFAULT_SERVICE_EXE;
}

void AServiceControl::onStart()
{
	QApplication::setOverrideCursor(Qt::WaitCursor);
	cpService->start(AIS_DEFAULT_SERVICE_NAME);
	onTimeout();
	QApplication::restoreOverrideCursor();
}

void AServiceControl::onStop()
{
	QApplication::setOverrideCursor(Qt::WaitCursor);
	cpService->stop(AIS_DEFAULT_SERVICE_NAME);
	onTimeout();
	QApplication::restoreOverrideCursor();
}

void AServiceControl::onInstall()
{
	QString aFilePathWithParams = cFilePath;
	QString aStartupFilePath = cpWidget->getStartupFilePath();
	int aStartupType = cpWidget->getStartupType();
	
	aFilePathWithParams.append(" --exec ");
	aFilePathWithParams.append('"' + QString(AIS_DEFAULT_SERVICE_NAME) + '"');
	
	if (aStartupFilePath.length())
		aFilePathWithParams.append(QString(" \"%1\"").arg(aStartupFilePath));

	QApplication::setOverrideCursor(Qt::WaitCursor);
	cpService->install(aStartupType, AIS_DEFAULT_SERVICE_NAME, AIS_DEFAULT_DISPLAY_NAME, aFilePathWithParams);
	onTimeout();
	QApplication::restoreOverrideCursor();
}

void AServiceControl::onRemove()
{
	QApplication::setOverrideCursor(Qt::WaitCursor);
	cpService->remove(AIS_DEFAULT_SERVICE_NAME);
	onTimeout();
	QApplication::restoreOverrideCursor();
}

void AServiceControl::onTimeout()
{
	if ((cpService == NULL) || (cpWidget == NULL))
		return;

	if (cpService->isService(AIS_DEFAULT_SERVICE_NAME))
	{
		if (cpService->isRunning(AIS_DEFAULT_SERVICE_NAME))
		{
			cpWidget->setStatus("Running");
			cpWidget->setButtonEnabled("all", false);
			cpWidget->setButtonEnabled("stop", true);
		}
		else
		{
			cpWidget->setStatus("Stopped");
			cpWidget->setButtonEnabled("all", false);
			cpWidget->setButtonEnabled("start", true);
			cpWidget->setButtonEnabled("remove", true);
		}
	}
	else
	{
		cpWidget->setStatus("Not installed");
		cpWidget->setButtonEnabled("all", false);
		cpWidget->setButtonEnabled("install", true);
	}
}
