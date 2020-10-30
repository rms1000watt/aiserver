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

#include "aservicecontrolwidget.h"
#include <QFileDialog>

AServiceControlWidget::AServiceControlWidget(QWidget* ipParent, const char* ipName, Qt::WFlags iFlgs)
    : QWidget(ipParent, iFlgs)
{
	cUi.setupUi(this);
	
	connect(cUi.upStartButton, SIGNAL(clicked()), this, SIGNAL(start()));
	connect(cUi.upStopButton, SIGNAL(clicked()), this, SIGNAL(stop()));
	connect(cUi.upInstallButton, SIGNAL(clicked()), this, SIGNAL(install()));
	connect(cUi.upRemoveButton, SIGNAL(clicked()), this, SIGNAL(remove()));
	connect(cUi.upBrowseButton, SIGNAL(clicked()), this, SLOT(onBrowse()));
	
	cUi.upStartupTypeComboBox->insertItem(0, tr("Automatic"), QVariant((int)1));
	cUi.upStartupTypeComboBox->insertItem(1, tr("Manual"), QVariant((int)0));
	
	cUi.upServiceStatusLineEdit->setReadOnly(true);
}

AServiceControlWidget::~AServiceControlWidget()
{

}

QString AServiceControlWidget::getStartupFilePath()
{
	return (cUi.upStartupPathLineEdit->text());
}

int AServiceControlWidget::getStartupType()
{
	return cUi.upStartupTypeComboBox->itemData(cUi.upStartupTypeComboBox->currentIndex()).toInt();
}

void AServiceControlWidget::setStatus(const QString& irStatus)
{
	cUi.upServiceStatusLineEdit->setText(irStatus);
}

void AServiceControlWidget::setServiceName(const QString& irServiceName)
{
	cUi.upServiceNameLineEdit->setText(irServiceName);
}

void AServiceControlWidget::setButtonEnabled(const QString& irButton, bool iEnabled)
{
	QPushButton* apButton = NULL;

	if (irButton == "start")
		apButton = cUi.upStartButton;
	else if (irButton == "stop")
		apButton = cUi.upStopButton;
	else if (irButton == "install")
		apButton = cUi.upInstallButton;
	else if (irButton == "remove")
		apButton = cUi.upRemoveButton;
	else if (irButton == "all")
	{
		cUi.upStartButton->setEnabled(iEnabled);
		cUi.upStopButton->setEnabled(iEnabled);
		cUi.upInstallButton->setEnabled(iEnabled);
		cUi.upRemoveButton->setEnabled(iEnabled);
	}
	
	if (apButton != NULL)
		apButton->setEnabled(iEnabled);
}

void AServiceControlWidget::onBrowse()
{
	QString aFileName = QFileDialog::getOpenFileName(this, tr("Select start-up script"), ".", tr("Lisp Source (*.sl)"));
	if (!aFileName.isNull())
	{
		cUi.upStartupPathLineEdit->setText(aFileName);
	}
}
