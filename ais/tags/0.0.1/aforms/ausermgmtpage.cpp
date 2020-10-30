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
aisdev/aforms/ausermgmtpage.h
													User Management Page

CHANGE HISTORY
Version	Date		Who		Change
3.2001	01/27/2008	fchua	Initial version.
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtCore/QtDebug>
#include <QtGui/QMessageBox>

#include "ausermgmtpage.h"
#include "aadduserdialog.h"
#include "aserverform.h"

//	------------------------------------------------------ METHODS ------------------------------------------------------------
/*!
 * \brief Constructor.
 */
AUserMgmtPage::AUserMgmtPage(QWidget *ipParent, AServerForm* ipSvrForm)
	: QWidget(ipParent), cpAddUserDialog(0), cpSvrForm(ipSvrForm)
{
	cUi.setupUi(this);

	// Create Add User Dialog instance
	cpAddUserDialog = new AAddUserDialog(this);

	// Set header labels
	QStringList aHeaderLabels;
	aHeaderLabels << "User Id" << "User Name" << "Security Level"
		<< "Begin Date" << "End Date" << "Comment";
	cUi.upUserList->setHeaderLabels(aHeaderLabels);
}

/*!
 * \brief Destructor.
 */
AUserMgmtPage::~AUserMgmtPage()
{

}

/*!
 * \brief Part of the APage interface, not used for now.
 */
void AUserMgmtPage::getTabMenus(AMenuList& orTabMenus, AToolList& orTabTools, bool iSelected)
{
    Q_UNUSED(iSelected);

	// No menus and toolbars yet...
	orTabMenus.clear();
	orTabTools.clear();
}

/*!
 * \brief This function is called when the Add button is pressed.
 */
void AUserMgmtPage::on_upAddButton_clicked()
{
	// Disable Edit Mode
	cpAddUserDialog->setEditOnly(false);

	// Show Add User Dialog
	if (cpAddUserDialog->exec() == QDialog::Accepted)
	{
		// Emit userAdded signal
		emit userAdded(cpAddUserDialog->getUsername(),
			cpAddUserDialog->getPasssword(),
			cpAddUserDialog->getSecurityLevel(),
			cpAddUserDialog->getEndDate(),
			cpAddUserDialog->getComment());
	}
	else
		cpAddUserDialog->clearAll();
}

/*!
 * \brief This function is called when the Delete button is pressed.
 */
void AUserMgmtPage::on_upDelete_clicked()
{
	// Delete button is pressed
	long aUserId = -1;

	// Get user id of selected user
	QList<QTreeWidgetItem*> apSelectedItems = cUi.upUserList->selectedItems();
	//qDebug() << apSelectedItems.size();
	
	// Make sure something is selected
	if (apSelectedItems.size() > 0)
	{
		// Get the selected item
		QTreeWidgetItem* apItem = apSelectedItems[0];
		aUserId = apItem->text(0).toLong();

		// Prompt user (Yes/No)
		QMessageBox aMsgBox(QMessageBox::Question,
							"Confirm User Delete", 
							"Are you sure you want to delete this user?",
							(QMessageBox::Yes | QMessageBox::No),
							this);
		if (aMsgBox.exec() == QMessageBox::Yes)
			emit userDeleted(aUserId);
	}
}

/*!
 * \brief This function is called when the Edit button is pressed.
 */
void AUserMgmtPage::on_upEditButton_clicked()
{
	// Edit button is pressed
	long aUserId = -1;

	// Get user id of selected user
	QList<QTreeWidgetItem*> apSelectedItems = cUi.upUserList->selectedItems();
	//qDebug() << apSelectedItems.size();
	
	// Make sure something is selected
	if (apSelectedItems.size() > 0)
	{
		// Get the selected item
		QTreeWidgetItem* apItem = apSelectedItems[0];
		aUserId = apItem->text(0).toLong();

		// Set edit mode
		cpAddUserDialog->setEditOnly(true);
		// Copy values to user dialog
		cpAddUserDialog->clearAll();
		cpAddUserDialog->setUsername(apItem->text(1));
		cpAddUserDialog->setSecurityLevel(apItem->text(2).toLong());
		cpAddUserDialog->setEndDate(QDate::fromString(apItem->text(4),"MM/dd/yyyy"));
		cpAddUserDialog->setComment(apItem->text(5));

		if (cpAddUserDialog->exec() == QDialog::Accepted)
		{
			// Password is optional, make sure the checkbox is checked
			// Encryption can be performed before signal is emited
			QString aPassword;
			if (cpAddUserDialog->isPasswordSet())
				aPassword = cpAddUserDialog->getPasssword();

			// Emit userUpdated signal
			emit userUpdated(aUserId,
				cpAddUserDialog->getUsername(),
				aPassword,
				cpAddUserDialog->getSecurityLevel(),
				cpAddUserDialog->getEndDate(),
				cpAddUserDialog->getComment());
		}

		cpAddUserDialog->clearAll();
	}
}

/*!
 * \brief This function is called when the Refresh button is pressed.
 */
void AUserMgmtPage::on_upRefreshButton_clicked()
{
	// Refresh button is pressed
	emit listRefreshed();
	if (cpSvrForm != NULL)
		cpSvrForm->statusAlert("");
}

/*!
 * \brief Slot function that handles the result of Add operation.
 *
 * \param[in] iRetValue Result of the add operation.
 */
void AUserMgmtPage::onUserAddResult(long iRetValue)
{
	if (iRetValue == 0)
	{
		emit listRefreshed();
		cpAddUserDialog->clearAll();
	}
}

/*!
 * \brief Slot function that handles the result of Update operation.
 *
 * \param[in] iRetValue Result of the update operation.
 */
void AUserMgmtPage::onUserUpdateResult(long iRetValue)
{
	if (iRetValue == 0)
		emit listRefreshed();
}

/*!
 * \brief Slot function that handles the result of Delete operation.
 *
 * \param[in] iRetValue Result of the delete operation.
 */
void AUserMgmtPage::onUserDeleteResult(long iRetValue)
{
	if (iRetValue == 0)
		emit listRefreshed();
}

/*!
 * \brief Slot function that handles the result of Refresh operation.
 *
 * \param[in] iRetValue Result of the list operation.
 * \param[in] irOut List of user information.
 */
void AUserMgmtPage::onListRefreshResult(long iRetValue, const QString& irOut)
{
	// format of the contents:
	// <userid>[tab]<username>[tab]<security level>[tab]<begindate>[tab]<enddate>[tab]<comments>[newline]
	// <userid>[tab]<username>[tab]<security level>[tab]<begindate>[tab]<enddate>[tab]<comments>[newline]
	// ...

	if (iRetValue == 0)
	{
		cUi.upUserList->clear();
		if (!irOut.isEmpty())
		{
			QTreeWidgetItem* aItem = 0;
			QStringList aUserInfo;
			QStringList aUserEntries = irOut.split(QChar('\n'), QString::KeepEmptyParts);
			long aUserEntriesSz = aUserEntries.size();

			for (long i = 0; i < aUserEntriesSz; i++)
			{
				aUserInfo = aUserEntries[i].split(QChar('\t'), QString::KeepEmptyParts);
				if (aUserInfo.size() == 6)
				{
					aItem = new QTreeWidgetItem(cUi.upUserList);
					for (long j = 0; j < 6; j++)
						aItem->setText(j,aUserInfo[j]);
				}
			}
		}
	}
}

/*!
 * \brief This function explicitly populates the user list.
 */
void AUserMgmtPage::show()
{
	// Called by AServerForm when User Management tab is activated
	emit listRefreshed();
}
