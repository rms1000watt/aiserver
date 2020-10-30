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
#ifndef APAGE_H
#define APAGE_H
/*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aisdev/forms/aform.h
															Form

CHANGE HISTORY
Version	Date		Who		Change
3.1003	10/29/2007	fchua	Added member function clear(). Fixed Doxygen documentation.
1.0107	9/21/2006	tlw		Add move to implement cursor stack instructions.
1.0100	6/9/2006	tlw		Add Doxygen documentation
												--------------- ---------------
	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
//	------------------------------------------------------ IMPORTS ------------------------------------------------------------
#include <QtGui/QTabWidget>
#include "aeditpage.h"
class QMenu;

//	------------------------------------------------------- TYPEDEFS ----------------------------------------------------------
typedef QList<QMenu*> AMenuList;
typedef QList<QAction*> AToolList;

//	------------------------------------------------------- CLASSES -----------------------------------------------------------
/*!
 * \brief A base class that is inherited by all pages that are instantiated by the tabbed widgets ASessionForm and AServerForm.
 *
 * This interface describes the common set of public methods provided by every page put into a form.
 * \note
 * -# Either form can call common methods on any of its child pages that it adds to its tabs.
 * -# If a form chooses to not implement any of the following public methods, a default is supplied by APage.
 */
class APage
{
public:
	APage();
	virtual void	copy();
	virtual void	cut();
	virtual void	find();
	virtual QWidget*getFocusWidget();
	virtual void	getTabMenus(AMenuList& orTabMenus, AToolList& orTabTools, bool iSelected) = 0;
	virtual void	move(bool iNext);
	virtual void	openPage(const QString& irExtentName, const QString& irPageName, AEditPage::APageType iPageType);
	virtual void	paste();
	virtual void	print();
	virtual void	replace();
	virtual void	redo();
	virtual QString	selectedText();
	virtual void	show();
	virtual void	undo();
	virtual void	clear();

private:
	void			errorMessage(const char* ipMsg);
};

#endif		// APAGE_H
