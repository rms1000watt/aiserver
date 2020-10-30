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
aisdev/atextedit/afcnlistdialog.cpp
													Function List Dialog
Afcnlistdialog is a modal pop-up dialog used to display the names and line number of the functions
in the file.
 
CHANGE HISTORY
Version	Date		Who		Change
3.2006	 3/10/2008	fchua	Simplified the class interface and event handling.
1.0116	11/29/2006	tlw		onHeaderClicked. Remove. Revise TableView settings in constructor to enable sort.
1.0113	11/7/2006	tlw		destructor. Omit unused destructor.
1.0057	 3/18/2005	tlw		Update documentation
												---------------------------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QHeaderView>
#include <QtGui/QKeyEvent>
#include <QtGui/QSortFilterProxyModel>
#include <QtGui/QStandardItemModel>
#include "afcnlistdialog.h"
#include "atextedit.h"

//	--------------------------------------------------- CLASS METHODS ---------------------------------------------------------
/*!
\brief AFcnListDialog - The constructor configures the table list view.

\param ipParent -> the parent that created this instance of the class.
\param ipName -> the name assigned to this instance of the class
\param iFlags - determines the behavior and layout of the dialog.
\return void
 */
AFcnListDialog::AFcnListDialog(QWidget* ipParent, const char* ipName, Qt::WFlags iFlags)
	: AOperationsDialog(ipParent, ipName, iFlags), cLineNum(-1), cNRows(0), cSortColumn(0), cpTextEdit(0)
{
	cUi.setupUi(this);

	// Widgets. Initialize GUI.
	setObjectName(QString(ipName));
	setSizeGripEnabled(true);

	// Table View. Configure the table view
	cUi.upTableView->setEditTriggers(QAbstractItemView::NoEditTriggers);
	cUi.upTableView->setLineWidth(0);
	cUi.upTableView->setSelectionBehavior(QAbstractItemView::SelectRows);
	cUi.upTableView->setSelectionMode(QAbstractItemView::SingleSelection);
	cUi.upTableView->setShowGrid(false/*Show*/);
	QHeaderView* apVertHeader = cUi.upTableView->verticalHeader();
	apVertHeader->setHighlightSections(true/*Highlight*/);
	apVertHeader->hide();
	connect(cUi.upTableView, SIGNAL(activated(const QModelIndex&)), this, SLOT(onSelectItem(const QModelIndex&)));

	// Model. Configure the table model to feed data to the table view
	cpListModel = new QStandardItemModel(1/*Rows*/, 2/*Cols*/,this);
	cpListModel->setObjectName("LambdaListModel");
	cpListModel->setHeaderData(0/*Column*/, Qt::Horizontal, QVariant("Line"), Qt::EditRole);
	cpListModel->setHeaderData(1/*Column*/, Qt::Horizontal, QVariant("Name"), Qt::EditRole);

	// Sorting. Configure Proxy Model to sort columns. Configure Table View to show sort indicator in header.
	cpListProxyModel = new QSortFilterProxyModel(this);
	cpListProxyModel->setObjectName("LambdaListProxyModel");
	cpListProxyModel->setDynamicSortFilter(true);
	cpListProxyModel->setSortCaseSensitivity(Qt::CaseSensitive);
	cpListProxyModel->setSourceModel(cpListModel);
	cUi.upTableView->setModel(cpListProxyModel);
	cUi.upTableView->sortByColumn(0/*Column*/, Qt::AscendingOrder);
	cUi.upTableView->setSortingEnabled(true);

	connect(cUi.upCloseButton, SIGNAL(clicked()), this, SLOT(reject()));
	connect(cUi.upSelectButton, SIGNAL(clicked()), this, SLOT(onSelectCurrentItem()));
}

/*!
\brief Fill out list of functions when dialog is selected (by Right-click)

\param ipTextEdit - not used
\param irList - List of function names
\return void
\par Notes:
-# Probably can omit first argument.
 */
void AFcnListDialog::init(ATextEdit* ipTextEdit, QStringList& irList)
{
	// Set a couple properties. Restore previous sort column, sort order
	if (irList.empty())
		return;

	// Load. Fill the list view from the function list
	long aCount = irList.count() & ~1;	// Round down to even number
	long aY;
	QModelIndex aIndex;
	cpListModel->removeRows(0/*StartRow*/, cpListModel->rowCount());
	cNRows = aCount / 2;
	cpListModel->insertRows(0, cNRows);
	for (aY = 0; aY < aCount; ++aY)
	{	aIndex = cpListModel->index(aY / 2, aY % 2);
		cpListModel->setData(aIndex, QVariant(irList[aY]));
	}
	// Resize. Eliminate the double-spacing of rows.
	for (aY = 0; aY < cNRows; ++aY)
		cUi.upTableView->resizeRowToContents(aY);

	// Select. Select the first item.
	if (cNRows > 0)
		cUi.upTableView->selectRow(0);

	cpTextEdit = ipTextEdit;
}

/*	---------------------------------------------------------------------------------------------------------------------------
onSelectCurrentItem - Item in list of functions is selected
Args:
	none
Returns:
	nothing
	------------------------------------------------------------------------------------------------------------------------ */
void AFcnListDialog::onSelectCurrentItem( )
{
    onSelectItem(cUi.upTableView->currentIndex());
}

// Double-click item
/*	---------------------------------------------------------------------------------------------------------------------------
selectItem - An function is selected from the list.
Args:
	ipItem		Ptr to selected item in List View
Returns:
	nothing
Notes:
 1.	Sets cLineNum to line number for this function and then closes the dialog
	------------------------------------------------------------------------------------------------------------------------ */
void AFcnListDialog::onSelectItem(const QModelIndex& irX)
{
	// Save the line number, sort column, sort order currently selected
	if (irX.isValid())
	{	long aRow = irX.row();
		QModelIndex aIndex = cpListProxyModel->index(aRow, 0/*Column*/);
		cLineNum = cpListProxyModel->data(aIndex, Qt::DisplayRole).toInt();
		cpTextEdit->editGoToLine(cLineNum);
		accept();
	}
	else
	{	cLineNum = -1;
		reject();
	}
}

void AFcnListDialog::showOnTop()
{
	show();
	raise();
	activateWindow();
}

// end
