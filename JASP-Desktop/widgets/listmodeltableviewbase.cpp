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

#include "log.h"
#include <QSize>
#include <fstream>
#include "listmodeltableviewbase.h"
#include "../analysis/analysisform.h"
#include "utilities/qutils.h"
#include "boundqmltableview.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optiondoublearray.h"

using namespace std;

ListModelTableViewBase::ListModelTableViewBase(BoundQMLTableView * tableView, QString tableType)
	: ListModel(tableView), _tableView(tableView), _tableType(tableType)
{
	connect(this, &ListModel::modelChanged, this, &ListModelTableViewBase::modelChangedSlot);
}

QVariant ListModelTableViewBase::data(const QModelIndex &index, int role) const
{
	if (_rowNames.length() == 0)
		return QVariant();

	int		column	= index.column(),
			row		= index.row();

	if (column < 0 || column >= columnCount() || row < 0 || row >= _rowNames.length())
		return QVariant();

	switch(role)
	{
	case int(specialRoles::lines):
	{
		bool	belowMeIsActive = row < rowCount() - 1,
				up				= true,
				left			= true,
				down			= !belowMeIsActive,
				right			= column == columnCount() - 1; //always draw left line and right line only if last col

		return	(left ?		1 : 0) +
				(right ?	2 : 0) +
				(up ?		4 : 0) +
				(down ?		8 : 0);
	}
	case Qt::DisplayRole:	return QVariant(_values[column][row]);
	default:				return QVariant();
	}
}


int ListModelTableViewBase::getMaximumColumnWidthInCharacters(size_t columnIndex) const
{
	int maxL = 3;
	for(QVariant val : _values[columnIndex])
		maxL = std::max(val.toString().size(), maxL);

	return maxL + 3;
}

void ListModelTableViewBase::addColumn()
{
	beginResetModel();

	if (columnCount() < _maxColumn)
	{
		_colNames.push_back(getColName(columnCount()));
		_values.push_back(QVector<QVariant>(_rowNames.length(), _defaultCellVal));
		_columnCount++;
	}

	endResetModel();

	emit columnCountChanged();
	emit modelChanged();
}

void ListModelTableViewBase::removeColumn(size_t col)
{
	beginResetModel();

	if (col < columnCount())
	{
		_values.removeAt(int(col));
		_colNames.pop_back();	
		_columnCount--;
	}

	endResetModel();

	emit columnCountChanged();
	emit modelChanged();
}

void ListModelTableViewBase::addRow()
{
	beginResetModel();

	if (rowCount() < _maxRow)
	{
		_rowNames.push_back(getRowName(rowCount()));
		_rowCount++;

		for (QVector<QVariant> & value : _values)
			while(value.size() < _rowCount) //Lets make sure the data is rectangular!
				value.push_back(_defaultCellVal);
	}

	endResetModel();

	emit rowCountChanged();
	emit modelChanged();
}

void ListModelTableViewBase::removeRow(size_t row)
{
	beginResetModel();

	if (row < rowCount())
	{
		for (QVector<QVariant> & value : _values)
			value.removeAt(int(row));
		_rowNames.pop_back(); //Should we remove the exact right rowName? Or I guess there just generated row for row in the base..
		_rowCount--;
	}

	endResetModel();

	emit rowCountChanged();
	emit modelChanged();
}

void ListModelTableViewBase::reset()
{
	beginResetModel();

	_colNames.clear();
	_rowNames.clear();
	_values.clear();
	_columnCount	= 0;
	_rowCount		= 0;

	for(size_t col; col < _initialColCnt; col++)
		addColumn();

	size_t rows = std::max(size_t(_rowNames.length()), _initialRowCnt);

	for(size_t row=0; row < rows; row++)
		addRow();

	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
	emit modelChanged();
}

void ListModelTableViewBase::itemChanged(int column, int row, QVariant value)
{
	//If you change this function, also take a look at ListModelFilteredDataEntry::itemChanged
	if (column > -1 && column < columnCount() && row > -1 && row < _rowNames.length())
	{
		if (_values[column][row] != value)
		{
			bool gotLarger = _values[column][row].toString().size() != value.toString().size();
			_values[column][row] = _itemType == "integer" ? value.toInt() : _itemType == "double" ? value.toDouble() : value;

			emit dataChanged(index(row, column), index(row, column), { Qt::DisplayRole });
			emit modelChanged();

			if(gotLarger)
				emit headerDataChanged(Qt::Orientation::Horizontal, column, column);
		}
	}
}

const Terms &ListModelTableViewBase::terms(const QString &what)
{
	_tempTerms.clear();
	int colNb = -1;
	if (what.isEmpty() && _values.length() == 1)
		colNb = 0;
	else if (!what.isEmpty())
	{
		colNb = _colNames.indexOf(what);
		if (colNb == -1 && what.startsWith("column"))
		{
			QString tempWhat = what;
			QString colNbStr = tempWhat.remove("column");
			bool ok = false;
			colNb = colNbStr.toInt(&ok);
			if (!ok) colNb = -1;
			if (colNb > 0) colNb--;
		}
	}

	if (colNb >= 0)
	{
		if (_values.length() > colNb)
		{
			const QVector<QVariant> firstCol = _values[colNb];
			for (const QVariant& val : firstCol)
			{
				QString value = val.toString();
				if (!value.isEmpty() && value != "...")
					_tempTerms.add(val.toString());
			}
		}
		else
			addError(tr("Column number in source use is bigger than the number of columns of %1").arg(name()));
	}
	else
	{
		if (what.isEmpty())
			addError(tr("No column specified in the source of %1").arg(name()));
		else
			addError(tr("The source use does not specified a valid column in %1").arg(name()));
	}


	return _tempTerms;
}

QVariant ListModelTableViewBase::headerData( int section, Qt::Orientation orientation, int role) const
{
	if(section < 0 || section >= (orientation == Qt::Horizontal ? _colNames.length() : _rowNames.length()))
		return QVariant();

	switch(role)
	{
	case int(specialRoles::maxColString): //A query from DataSetView for the maximumlength string to be expected! This to accomodate columnwidth
	{
		QString dummyText	= headerData(section, orientation, Qt::DisplayRole).toString() + "XXXXX";
		int colWidth		= getMaximumColumnWidthInCharacters(size_t(section));

		while(colWidth > dummyText.length())
			dummyText += "X";

		return dummyText;
	}
	case Qt::DisplayRole:			return QVariant(orientation == Qt::Horizontal ? _colNames[section] : _rowNames[section]);
	case Qt::TextAlignmentRole:		return QVariant(Qt::AlignCenter);
	default:						return QVariant();
	}
}

QHash<int, QByteArray> ListModelTableViewBase::roleNames() const
{
	static QHash<int, QByteArray> roles = ListModel::roleNames();

	static bool addRoles = true;

	if(addRoles)
	{
		roles[int(specialRoles::active)]		= QString("active").toUtf8();
		roles[int(specialRoles::lines)]			= QString("lines").toUtf8();
		roles[int(specialRoles::maxColString)]	= QString("maxColString").toUtf8();
		addRoles = false;
	}

	return roles;
}

Qt::ItemFlags ListModelTableViewBase::flags(const QModelIndex &) const
{
	return Qt::ItemIsSelectable | Qt::ItemIsEnabled | Qt::ItemIsEditable;
}

void ListModelTableViewBase::runRScript(const QString & script)
{
	_tableView->runRScript(script);
}

bool ListModelTableViewBase::valueOk(QVariant value)
{
	bool	ok	= true;

	if		(_itemType == "double")		value.toDouble(&ok);
	else if	(_itemType == "integer")	value.toInt(&ok);

	return ok;
}
