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

#ifndef AFCNLISTDIALOG_H
#define AFCNLISTDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/atextedit/afcnlistdialog.h
													Function List Dialog

CHANGE HISTORY
Version	Date		Who		Change
3.2006	 3/10/2008	fchua	Simplified the class interface and event handling.
1.0116	11/29/2006	tlw		onHeaderClicked. No longer needed as QT handles it.
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0057	 3/18/2005	tlw		Update documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "ui_afcnlistdialog.h"
#include "atextedit.h"		// Here just so that moc_afcnlistdialog.cpp will compile
#include "aoperationsdialog.h"

class QSortFilterProxyModel;
class QStandardItemModel;

//	------------------------------------------------ CLASS DEFINITIONS	-------------------------------------------------------
/*!
\brief AFcnListDialog - Shows a list of Lambdas/functions for the specified language.

The function-list dialog allows the user to quickly locate an Lambda/function in a file.  A two-column list is shown. The first
column shows the line number and the second column shows the function name.  By double clicking on an entry, the user can move
to the top of the Lambda/function.
/par Notes:
-# The columns in the table view can be sorted by line number or by Lambda name is ascending or descending order by clicking on
the column headers.
-# The function syntax depends upon the language associated with the Lambda/file.
 */
class AFcnListDialog : public AOperationsDialog
{
	Q_OBJECT

public:
	AFcnListDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlags = 0);
	virtual void init(ATextEdit* ipTextEdit, QStringList& irList);
	virtual void showOnTop();

private slots:
	void	onSelectCurrentItem();
	void	onSelectItem(const QModelIndex& irX);

private:
	QSortFilterProxyModel*	cpListProxyModel;
	QStandardItemModel*		cpListModel;
	long					cLineNum;
	long					cNRows;
	long					cSortColumn;
	ATextEdit*				cpTextEdit;
	Ui::AFcnListClass		cUi;
};

#endif // AFCNLISTDIALOG_H
