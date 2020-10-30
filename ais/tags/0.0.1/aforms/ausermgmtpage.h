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
#ifndef AUSERMGMTPAGE_H
#define AUSERMGMTPAGE_H

/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aforms/ausermgmtpage.h
													User Management Page

CHANGE HISTORY
Version	Date		Who		Change
3.2001	02/18/2008	fchua	Added Doxygen documentation.
3.2001	01/27/2008	fchua	Initial version.
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "apage.h"
#include "ui_ausermgmtpage.h"

//  ------------------------------------------------- FORWARD DECLARATIONS ----------------------------------------------------
class QWidget;
class AServerForm;
class AAddUserDialog;

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
 * \brief Provides a page for user account management.
 */
class AUserMgmtPage : public QWidget, public APage
{
	Q_OBJECT

public:
	AUserMgmtPage(QWidget *ipParent = 0, AServerForm *ipSvrForm = 0);
	~AUserMgmtPage();

	virtual void getTabMenus(AMenuList& orTabMenus, AToolList& orTabTools, bool iSelected);
	virtual void show();

signals:
	/*!
	 * \brief This signal is emitted when an add user request is made.
	 *
	 * \param[in] irUsername Username.
	 * \param[in] irPassword Password.
	 * \param[in] iSecurityLevel Security level.
	 * \param[in] irEndDate Date of expiration.
	 * \param[in] irComment Additional remarks.
	 */
	void userAdded(const QString& irUsername, const QString& irPassword,
		int iSecurityLevel, const QDate& irEndDate, const QString& irComment);

	/*!
	 * \brief This signal is emitted when an update user request is made.
	 *
	 * \param[in] iUserId Existing User Id.
	 * \param[in] irUsername New Username.
	 * \param[in] irPassword New Password. Blank if no change.
	 * \param[in] iSecurityLevel New Security level.
	 * \param[in] irEndDate New date of expiration.
	 * \param[in] irComment New remarks.
	 */
	void userUpdated(long iUserId, const QString& irUsername, const QString& irPassword,
		int iSecurityLevel, const QDate& irEndDate, const QString& irComment);

	/*!
	 * \brief This signal is emitted when a delete user request is made.
	 *
	 * \param[in] iUserId Existing User Id.
	 */
	void userDeleted(long iUserId);

	/*!
	 * \brief This signal is emitted when the user list is refreshed.
	 */
	void listRefreshed();

public slots:
	void onUserAddResult(long iRetValue);
	void onUserUpdateResult(long iRetValue);
	void onUserDeleteResult(long iRetValue);
	void onListRefreshResult(long iRetValue, const QString& irOut);

private:
	Ui::AUserMgmtPageClass cUi;
	AAddUserDialog *cpAddUserDialog;
	AServerForm *cpSvrForm;

private slots:
	void on_upRefreshButton_clicked();
	void on_upDelete_clicked();
	void on_upEditButton_clicked();
	void on_upAddButton_clicked();
};

#endif // AUSERMGMTPAGE_H
