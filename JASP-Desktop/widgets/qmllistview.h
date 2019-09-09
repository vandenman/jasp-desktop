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

#ifndef QMLLISTVIEW_H
#define QMLLISTVIEW_H

#include "qmlitem.h"
#include "common.h"
#include <QObject>

class ListModel;

class QMLListView : public QObject, public virtual QMLItem
{
Q_OBJECT

public:
	struct SourceType
	{
		QString			name,
						discard,
						modelUse;
		ListModel	*	model,
					*	discardModel;

		SourceType(QString _name, QString _discard = "", QString _modelUse = "")
			: name(_name), discard(_discard), modelUse(_modelUse), model(nullptr), discardModel(nullptr) {}
	};
	
	QMLListView(QQuickItem* item, AnalysisForm* form);
	
	virtual ListModel	*	model() = 0;
			void			setUp()			override;
			void			cleanUp()		override;
	
	virtual void			setTermsAreNotVariables();
	virtual void			setTermsAreInteractions();
	
			int				variableTypesAllowed()		const	{ return _variableTypesAllowed; }
			int				variableTypesSuggested()	const	{ return _variableTypesSuggested; }

	const	QList<SourceType*>	& sourceModels()		const	{ return _sourceModels; }

protected slots:
	virtual void modelChangedHandler() {} // Model has changed: change the options
			void sourceChangedHandler();
			
	virtual void setSources();

protected:
	QList<SourceType*>	_sourceModels;
	bool				_needsSourceModels;
	int					_variableTypesAllowed,
						_variableTypesSuggested;
	
private:
	int		_getAllowedColumnsTypes();
	void	_setAllowedVariables();
};

#endif // QMLLISTVIEW_H
