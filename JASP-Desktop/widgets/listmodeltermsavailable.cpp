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

#include "listmodeltermsavailable.h"
#include "listmodeltermsassigned.h"
#include "../analysis/analysisform.h"
#include <QQmlProperty>

ListModelTermsAvailable::ListModelTermsAvailable(QMLListView* listView, bool mixedModelTerms)
	: ListModelAvailableInterface(listView, mixedModelTerms)
{
}

void ListModelTermsAvailable::sortItems(SortType sortType)
{	
	if (sortType == Sortable::None)
	{
		Terms suggested;
		Terms allowed;
		Terms forbidden;

		for (const Term &term : _allTerms)
		{
			if ( ! isAllowed(term))
				forbidden.add(term);
			else if (isSuggested(term))
				suggested.add(term);
			else
				allowed.add(term);
		}

		_allTerms.clear();
		_allTerms.add(suggested);
		_allTerms.add(allowed);
		_allTerms.add(forbidden);
	}

	ListModelAvailableInterface::sortItems(sortType);
}

void ListModelTermsAvailable::resetTermsFromSourceModels(bool updateAssigned)
{
	
	beginResetModel();

	Terms termsAvailable = getSourceTerms();

	if (_mixedModelTerms)
	{
		QMap<ListModel*, Terms> termsPerSource = getSourceTermsPerModel();
		QList<Terms> termsPerModel = termsPerSource.values();

		if (termsPerModel.length() > 1)
		{
			Terms mixedTerms = termsPerModel[0];
			mixedTerms.removeParent();
			for (int i = 1; i < termsPerModel.length(); i++)
			{
				const Terms& termsToBeCombined = termsPerModel[i];
				Terms extraTerms;
				for (const Term& mixedTerm : mixedTerms)
				{
					for (const Term& termToBeCombined : termsToBeCombined)
					{
						QStringList components = mixedTerm.components();
						components.append(termToBeCombined.components());
						extraTerms.add(Term(components));
					}
				}
				mixedTerms.add(extraTerms);
			}
			termsAvailable.add(mixedTerms);
		}
	}
	
	setChangedTerms(termsAvailable);
	initTerms(termsAvailable);

	endResetModel();

	if (updateAssigned)
		emit allAvailableTermsChanged(&_tempAddedTerms, &_tempRemovedTerms);
}

ListModel *ListModelTermsAvailable::getSourceModelOfTerm(const Term &term)
{
	ListModel* result = nullptr;
	QMap<ListModel*, Terms> map =  getSourceTermsPerModel();
	QMapIterator<ListModel*, Terms> it(map);
	while (it.hasNext())
	{
		it.next();
		if (it.value().contains(term))
			result = it.key();
	}
	return result;
}
