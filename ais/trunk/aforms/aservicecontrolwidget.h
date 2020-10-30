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

#ifndef ASERVICECONTROLWIDGET_H
#define ASERVICECONTROLWIDGET_H

#include <QWidget>
#include "ui_aservicecontrolwidget.h"

class AServiceControlWidget : public QWidget
{
    Q_OBJECT

public:
	AServiceControlWidget(QWidget* ipParent = NULL, const char* ipName = NULL, Qt::WFlags iFlgs = 0);
	virtual ~AServiceControlWidget();
	
	QString getStartupFilePath();
	int getStartupType();

	void setStatus(const QString& irStatus);
	void setServiceName(const QString& irServiceName);
	void setButtonEnabled(const QString& irButton, bool iEnabled);

signals:
	void start();
	void stop();
	void install();
	void remove();
	
protected slots:
	void onBrowse();
	
private:
	Ui::AServiceControlWidget cUi;
};

#endif
