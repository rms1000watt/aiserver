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
aisdev/aforms/asysresmonpage.cpp
												System Resource Monitor Page

CHANGE HISTORY
Version	Date		Who		Change
3.2003	02/15/2008	fchua	Initial version.
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "asysresmonpage.h"

/*!
 * \brief Class constructor.
 *
 * \param[in] ipParent QObject parent.
 * \param[in] ipSvrForm Pointer to AServerForm instance.
 */
ASysResMonPage::ASysResMonPage(QWidget *ipParent, AServerForm *ipSvrForm)
	: QWidget(ipParent), cpSvrForm(ipSvrForm)
{
	cUi.setupUi(this);
	setTableHeaders();
}

/*!
 * \brief Class destructor.
 */
ASysResMonPage::~ASysResMonPage()
{

}

/*!
 * \brief Returns the value of the monitor interval.
 */
int ASysResMonPage::getMonitorInterval()
{
	return cUi.upIntervalSpinBox->value();
}

/*!
 * \brief Part of the APage interface, not used for now.
 */
void ASysResMonPage::getTabMenus(AMenuList& orTabMenus, AToolList& orTabTools, bool iSelected)
{
    Q_UNUSED(iSelected);

	// No menus and toolbars yet...
	orTabMenus.clear();
	orTabTools.clear();
}

/*!
 * \brief Returns true if the monitoring of a given resource is enabled.
 *
 * \param[in] iResType Resource Type.
 *
 * \return true or false.
 */
bool ASysResMonPage::isResourceMonitorEnabled(int iResType)
{
	if (iResType == geGetConnectionStats)
		return (cUi.upConnectionsCheckBox->isChecked());
	else if (iResType == geGetLogonStats)
		return (cUi.upLogonsCheckBox->isChecked());
	else if (iResType == geGetSessionStats)
		return (cUi.upSessionsCheckBox->isChecked());
	else if (iResType == geGetRequestStats)
		return (cUi.upRequestsCheckBox->isChecked());
	else
		return false;
}

/*!
 * \brief This function updates the list based from the request type.
 *
 * \param[in] iResType Resource type.
 * \param[in] irOut Return output from the server.
 *
 * \note
 * The output string from the server contains records which are new-line ('\n') separated.
 * Each record contains two fields which are tab ('\t') separated.
 */
void ASysResMonPage::onMonitorResult(int iResType, const QString& irOut)
{
	// Get a pointer to the tree widget first
	QTreeWidget* apResourceTreeWidget = 0;
	switch(iResType)
	{
	case geGetConnectionStats:
		apResourceTreeWidget = cUi.upConnectionsTreeWidget;
		break;
	case geGetLogonStats:
		apResourceTreeWidget = cUi.upLogonsTreeWidget;
		break;
	case geGetSessionStats:
		apResourceTreeWidget = cUi.upSessionsTreeWidget;
		break;
	case geGetRequestStats:
		apResourceTreeWidget = cUi.upRequestsTreeWidget;
		break;
	default:
		break;
	}

	// check if the resource type was valid
	if (apResourceTreeWidget != 0)
	{
		// clear old data
		apResourceTreeWidget->clear();

		// check if there's something to display
		if (!irOut.isEmpty())
		{
			QTreeWidgetItem* aItem = 0;
			QStringList aDetails;
			QStringList aEntries = irOut.split(QChar('\n'), QString::KeepEmptyParts);
			long aSz = aEntries.size();

			for (long i = 0; i < aSz; i++)
			{
				aDetails = aEntries[i].split(QChar('\t'), QString::KeepEmptyParts);
				if (aDetails.size() == 2)
				{
					aItem = new QTreeWidgetItem(apResourceTreeWidget);
					for (long j = 0; j < 2; j++)
						aItem->setText(j, aDetails[j]);
				}
			}
		}
	}
}

/*!
 * \brief This function is invoked when the value of interval was changed.
 */
void ASysResMonPage::on_upIntervalSpinBox_valueChanged(int)
{
	emit monitorIntervalChanged();
}

/*!
 * \brief Set the labels of the tree widgets.
 */
void ASysResMonPage::setTableHeaders()
{
	// Set header labels
	QStringList aHeaderLabels;
	aHeaderLabels << "Connection Type" << "No. of Connections";
	cUi.upConnectionsTreeWidget->setHeaderLabels(aHeaderLabels);

	aHeaderLabels.clear();
	aHeaderLabels << "Username" << "No. of Logons";
	cUi.upLogonsTreeWidget->setHeaderLabels(aHeaderLabels);

	aHeaderLabels.clear();
	aHeaderLabels << "Username" << "No. of Sessions";
	cUi.upSessionsTreeWidget->setHeaderLabels(aHeaderLabels);

	aHeaderLabels.clear();
	aHeaderLabels << "Session" << "No. of Pending Requests";
	cUi.upRequestsTreeWidget->setHeaderLabels(aHeaderLabels);
}
