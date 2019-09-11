//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#ifndef ListModelJAGSDataInput_H
#define ListModelJAGSDataInput_H

#include "listmodeltableviewbase.h"


class ListModelJAGSDataInput : public ListModelTableViewBase
{
	Q_OBJECT

public:
	explicit ListModelJAGSDataInput(BoundQMLTableView * parent, QString tableType) : ListModelTableViewBase(parent, tableType)
	{
		_defaultCellVal = "...";
	}
	Qt::ItemFlags		flags(		const QModelIndex &index) const	override;

	int getMaximumColumnWidthInCharacters(size_t columnIndex) const	override;

	QString			getColName(size_t index)						override;
	OptionsTable *	createOption()									override;
	void			initValues(OptionsTable * bindHere)				override;

public slots:
	void sourceTermsChanged(Terms* termsAdded, Terms* termsRemoved)	override;
	void modelChangedSlot()											override;
};

#endif // ListModelJAGSDataInput_H

