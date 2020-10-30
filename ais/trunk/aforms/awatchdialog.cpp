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
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/aforms/awatchdialog.cpp
											Watch AIS Dialog

CHANGE HISTORY
Version	Date		Who		Change
1.0000	10/27/2005	rca		New Window for watching variables
												--------------- --------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include "awatchdialog.h"
#include <QtGui/QInputDialog>
#include <QtGui/QMessageBox>

//	------------------------------------------------------ METHODS -------------------------------------------------------------
/*!
\brief AWatchDialog - Constructor instantiates the GUI elements generated in ui_awatchdialog.h
 
 \par Args:
 \param ipParent -> Parent widget that created this instance of the dialog.
 \param ipName -> Name assigned to this instance of the dialog
 \param iModal true iff this dialog is modal
 \param iFlgs Optional flags to set the disposition of this dialog.
 \par Returns:
 nothing
 */
AWatchDialog::AWatchDialog(QWidget* ipParent, AAppClient* ipAppClient, AReturnRcvr* ipReturnRcvr, const char* ipName, bool iModal, Qt::WFlags iFlgs)
    : QDialog(ipParent, iFlgs),cpAppClient(ipAppClient), cpReturnRcvr(ipReturnRcvr)
{
    Q_UNUSED(iModal);
	QStringList headerLabels;

	cUi.setupUi(this);
	setObjectName(ipName);
	QObject::connect(cUi.upCloseButton, SIGNAL(clicked()), this, SLOT(accept()));
	QObject::connect(cUi.upAddButton, SIGNAL(clicked()), this, SLOT(onAddVarPressed()));
	QObject::connect(cUi.upRemoveButton, SIGNAL(clicked()), this, SLOT(onRemoveVarPressed()));
	QObject::connect(cUi.upRemoveAllButton, SIGNAL(clicked()), this, SLOT(onRemoveAllVarPressed()));
    QObject::connect(cUi.upVarListTree, SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(onVarListDoubleClicked(const QModelIndex&)));

	headerLabels << "Name" << "Value";
	cpVarListModel = new QStandardItemModel( 0/*Rows*/, 2/*Columns*/, this );
	cpVarListModel->setHorizontalHeaderLabels( headerLabels );
	cpVarListModel->setObjectName( "VarListModel" );

	cUi.upVarListTree->setModel( cpVarListModel );
	cUi.upVarListTree->selectionModel()->setCurrentIndex( cpVarListModel->index(0,0), QItemSelectionModel::SelectCurrent );
}

// Item. An item may have one of the following formats where * represents zero or more chars:
//	*#<StructureName>
//	Av.ArgumentName=*
//	Cv.ConstantName=*
//	Tv.TemporaryName=*
//	Pv.PersistentName=*
//	Rules.
//	 1.	If a structure (#<), show previous descent information in Vars display and descend one level.
//	 2.	If a variable, show value.
void AWatchDialog::onVarListDoubleClicked(const QModelIndex& irIdx)
{
	//if (!cpParentForm->debuggerActive()) return;
	long aCharPos = 0;
	QString aCmd;
	QString aVar = cpVarListModel->data(irIdx, Qt::DisplayRole).toString().simplified();
	long aBeg, aEnd;

	// VarName. Compose command for Xv.Name format.
	if ((aBeg = aVar.indexOf("#<")) < 0)
	{	if (aVar.mid(1, 2) == "v." && (aCharPos = aVar.indexOf('=', 0)) > 0)
		{	aCmd = "=(string " + aVar.left(aCharPos - 1) + " true)";
			cpAppClient->debug(cpReturnRcvr, aCmd);
		}
	}
	// Structure. Else, extract history before structure., if any.
	else if ((aEnd = aVar.indexOf('>')) > 0)
	{	// History. History is a list of lines beginning and ending with  "===" that precedes the structure.
		QString aHistory;
		long aIy = 0;
		long aCurIy = irIdx.row();
		QModelIndex aIdy = cpVarListModel->index(0/*Row*/, 0/*Column*/);
		QString aLine = cpVarListModel->data(aIdy, Qt::DisplayRole).toString();
		if (aLine == "===")
		{	for (;;)
			{	aIdy = cpVarListModel->index(++aIy, 0/*column*/);
				aLine = cpVarListModel->data(aIdy, Qt::DisplayRole).toString();
				if (aIy < aCurIy && aLine != "===")
					aHistory += '\t' + aLine.simplified();
				else
					break;
			}
			aHistory += '\t';
		}
		else
			aHistory = "###\t";
		aHistory += aVar + "\t###\t";

		// Target. Extract target to be expanded.
		QString aTarget(aVar.mid(aBeg, aEnd - aBeg + 1));

		//cIdx = irIdx;
		// Cmd. Format and submit command.
		aCmd = "=(append {" + aHistory + "} (browseLib.Pv.delimitedString " + aTarget + " { = } #\\tab true))";
		cpAppClient->debug(cpReturnRcvr, aCmd);
	}
	cIdx = irIdx;
}

void AWatchDialog::onAddVarPressed()
{
	bool aOk = false;
	QString aVarName = QInputDialog::getText(this, "Watch Variable", "Enter Variable Name:", QLineEdit::Normal, "", &aOk);
	if (aOk && aVarName.length() > 0 )
	{
		watchVariable( aVarName );
	}
}


void AWatchDialog::watchVariable( const QString& iVarName )
{
	if (iVarName.length() > 0 )
	{
		bool aMatch = false;
		long rowCount = cpVarListModel->rowCount();
		for( int ctr = 0; ctr < rowCount; ++ctr )
		{
			QStandardItem* aItem = cpVarListModel->item( ctr );
			if( aItem->text() == iVarName )
			{
				aMatch = true;
				break;
			}
		}
		if( aMatch == false )
		{
			QList<QStandardItem*> aItem;
			QString aCmd;
			aItem.append( new QStandardItem( iVarName ) );
			aItem.append( new QStandardItem( tr("#void") ) );
			cpVarListModel->appendRow( aItem );
			cUi.upVarListTree->expandAll();

			// Send command to retrieve the current value
			aCmd = "=(cond ((<> #void rv." + iVarName + ")(append {## " + iVarName + " = }rv." + iVarName +  "))";
			aCmd += "((<> #void cv." + iVarName + ")(append {## " + iVarName + " = } cv." + iVarName +  "))";
			aCmd += "((<> #void pv." + iVarName + ")(append {## " + iVarName + " = } pv." + iVarName +  "))";
			aCmd += "((<> #void tv." + iVarName + ")(append {## " + iVarName + " = } tv." + iVarName +  "))";
			aCmd += "((<> #void av." + iVarName + ")(append {## " + iVarName + " = } av." + iVarName +  "))";
			aCmd += "((<> #void Sv." + iVarName + ")(append {## " + iVarName + " = } Sv." + iVarName +  "))";
			aCmd += "((<> #void In." + iVarName + ")(append {## " + iVarName + " = } In." + iVarName +  "))";
			aCmd += "(else (append {## " + iVarName + " = } " + iVarName + ")))";
			cpAppClient->debug(cpReturnRcvr, aCmd);
		}
		else
		{
			QMessageBox::information( this, "Add Variable", "The variable name is already in the list");
		}
	}
}

void AWatchDialog::onRemoveVarPressed()
{
	QModelIndex aSelectedIndex = cUi.upVarListTree->currentIndex();
	QModelIndex aParent = aSelectedIndex.parent();
	
	if( !aParent.isValid() )
	{
		//QStandardItem* aParent2 = cpVarListModel->itemFromIndex( aParent );
		cpVarListModel->removeRow(aSelectedIndex.row());
	}

}
void AWatchDialog::onRemoveAllVarPressed()
{
	cpVarListModel->removeRows(0, cpVarListModel->rowCount());
}

QStringList AWatchDialog::getWatchList()
{
	long ctr = 0;
	long rowCount = 0;
	QStringList aWatchList;
	rowCount = cpVarListModel->rowCount();
	for( ctr = 0; ctr < rowCount; ++ctr )
	{
		QStandardItem* aItem = cpVarListModel->item( ctr, 0 );
		aWatchList.append( aItem->text() );
	}
	return aWatchList;
}

void AWatchDialog::updateValue(const QString& irVarValue)
{
	if( irVarValue.startsWith( "###" ) )
	{
		QString data = cIdx.data().toString();
		QModelIndex aIdx = cIdx.sibling( cIdx.row(), 0 );
		QStandardItem* aParent = cpVarListModel->itemFromIndex( aIdx );
		aParent->removeRows( 0, aParent->rowCount() );
		QStringList aVarLines = irVarValue.split('\177', QString::KeepEmptyParts);
		for( int ctr = 3; ctr < aVarLines.count(); ctr++ )
		{
			QList<QStandardItem*> aItem;
			QString name = aVarLines[ctr].left( aVarLines[ctr].indexOf( " " ));
			QString value = aVarLines[ctr].mid( aVarLines[ctr].indexOf( " " ) + 1);

			if( name.length() > 0 )
			{
				aItem.append(new QStandardItem( name ));
				aItem.append(new QStandardItem( value ));
				aParent->insertRow( aParent->rowCount(), aItem );
			}
			else
			{
				QMessageBox::information(this, "No data in variable", "There is no data variable: " + aParent->text());
			}
		}
		cUi.upVarListTree->setExpanded( aParent->index(), true );
	}
	else
	if( irVarValue.startsWith( "##" ) )
	{
		QString data = irVarValue;
		QString name = data.mid( 3, data.indexOf( " = " ) - 3);
		QString value = data.mid( data.indexOf( " = " ) + 3);
		// Traverse the tree and replace the corresponding value for the given variable name
		int rowCount = cpVarListModel->rowCount();
		for( int ctr = 0; ctr < rowCount; ++ctr )
		{
			QStandardItem* aItem = cpVarListModel->item( ctr );
			if( aItem->text() == name )
			{
				if( cpVarListModel->item( ctr )->hasChildren() )
					cpVarListModel->item( ctr )->removeRows( 0, cpVarListModel->item( ctr )->rowCount() );
				cpVarListModel->setItem( ctr, 1, new QStandardItem( value ));
			}
		}
	}
	{
		QString data = irVarValue;
		QString name = data.mid( 3, data.indexOf( " = " ) - 3);
		QString value = data.mid( data.indexOf( " = " ) + 3);
		// Traverse the tree and replace the corresponding value for the given variable name
		int rowCount = cpVarListModel->rowCount();
		for( int ctr = 0; ctr < rowCount; ++ctr )
		{
			QStandardItem* aItem = cpVarListModel->item( ctr );
			if( aItem->text() == name )
			{
				if( cpVarListModel->item( ctr )->hasChildren() )
					cpVarListModel->item( ctr )->removeRows( 0, cpVarListModel->item( ctr )->rowCount() );
				cpVarListModel->setItem( ctr, 1, new QStandardItem( value ));
			}
		}
	}
}

void AWatchDialog::clearValues()
{
	// DO SOMETHING HERE	
}

// end
