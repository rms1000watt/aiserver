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

#ifndef APREFDIALOG_H
#define APREFDIALOG_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/include/aprefdialog.h
														Preference Dialog

CHANGE HISTORY
Version	Date		Who		Change
3.2008	 4/03/2008	fchua	Initial version.
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QDialog>
#include "aparameters.h"

/*!
 * \brief This class serves as a generic interface for preference dialog.
 *
 * This purpose of this class is to minimize inter-project dependencies.
 *
 * \sa
 * ATextEdit
 */
class APrefDialog: public QDialog
{
	Q_OBJECT

public:
	APrefDialog(QWidget* ipParent, const char* ipName = 0, Qt::WFlags iFlgs = 0);
	virtual ~APrefDialog();

	virtual void setParameters(AParameters *ipParams);
	virtual void getParameters(AParameters *iopParams);
};

#endif
