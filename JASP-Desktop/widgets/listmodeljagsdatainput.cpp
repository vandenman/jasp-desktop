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
#include "utilities/qutils.h"
#include "ListModelJAGSDataInput.h"
#include "analysis/analysisform.h"
#include "analysis/options/optionstring.h"
#include "analysis/options/optionterm.h"

void ListModelJAGSDataInput::sourceTermsChanged(Terms *termsAdded, Terms *)
{
	beginResetModel();

	_rowNames.clear();
	_colNames.clear();
	_values.clear();
	_columnCount = 0;

	if (termsAdded && termsAdded->size() > 0)
	{
		const std::string	& colName	= termsAdded->at(0).asString();
		DataSet				* dataset	= listView()->form()->getDataSet();
		Column				& column	= dataset->columns().get(colName);
		Labels				& labels	= column.labels();

		for (auto label : labels)
			_rowNames.push_back(tq(label.text()));

		QVector<QVariant> newValues(_rowNames.length(), 1.0);
		_values.push_back(newValues);
		_colNames.push_back(getColName(0));
		_columnCount = 1;

	}

	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
	emit modelChanged();
}


QString ListModelJAGSDataInput::getColName(size_t index)
{
	if (_tableType == "PriorCounts")
		return "Counts";

	if (index >= _maxColumn)
		index = _maxColumn - 1;

	char letter = char(97 + index);
	return tq("H₀ (") + letter + tq(")");
}

OptionsTable *ListModelJAGSDataInput::createOption()
{
	Options* optsTemplate =		new Options();
	optsTemplate->add("name",	new OptionString());
	optsTemplate->add("levels", new OptionVariables());
	optsTemplate->add("values", new OptionTerm());

	return new OptionsTable(optsTemplate);
}

void ListModelJAGSDataInput::initValues(OptionsTable * bindHere)
{
	_colNames.clear();
	_rowNames.clear();
	_values.clear();

	_boundTo = bindHere;

	std::vector<Options *>	options = bindHere->value();

	OptionVariables		* optionLevels = nullptr;

	for (Options * newRow : options)
	{
		OptionString	*	optionName		= static_cast<OptionString		*>(newRow->get("name"));
							optionLevels	= static_cast<OptionVariables	*>(newRow->get("levels")); // why not store it once?
		OptionTerm		*	optionValues	= static_cast<OptionTerm		*>(newRow->get("values"));

		_colNames.push_back(QString::fromStdString(optionName->value()));
		//levels = optionLevels->variables(); //The old code (in boundqmltableview.cpp) seemed to specify to simply use the *last* OptionVariables called "levels" in the binding option. So I'll just repeat that despite not getting it.
		_values.push_back({});
		for (const std::string & val : optionValues->term())
			_values[_values.size()-1].push_back(tq(val));
	}

	if(optionLevels)
		for(const std::string & level : optionLevels->variables())
			_rowNames.push_back(QString::fromStdString(level));

	//No need to check colnames to cols in values because they are created during the same loop and thus crash if non-matching somehow
	if (_values.size() > 0 && int(_values[0].size()) != _rowNames.size())
		addError("Number of rows specifed in Options for ListModelJAGSDataInput does not match number of rows in values!");


	beginResetModel();

	_columnCount = _colNames.size();

	for(auto & col : _values)
		if(_rowNames.size() < col.size())
		{
			Log::log() << "Too many rows in a column of OptionsTable for ListModelJAGSDataInput! Shrinking column to fit." << std::endl;
			col.resize(_rowNames.size());
		}
		else
			for (int row = col.size(); row < _rowNames.size(); row++)
				col.push_back(1);

	//Ok, going to assume that the following: for (size_t i = values.size(); i < _columnCount; ++i) means we should add columns in case the data wasn't filled correctly (aka colNames did not match with values) but that cannot be now.

	endResetModel();

	emit columnCountChanged();
	emit rowCountChanged();
}

void ListModelJAGSDataInput::modelChangedSlot() // Should move this to listmodeltableviewbase as well probably? And also connect columnCount and colNames etc
{
	if (_boundTo)
	{
		std::vector<std::string> stdlevels;
		for (const QString& rowName : _rowNames)
			stdlevels.push_back(rowName.toStdString());

		std::vector<Options*> allOptions;

		for (int colIndex = 0; colIndex < _colNames.size(); colIndex++)
		{
			Options* options =		new Options();
			options->add("name",	new OptionString(_colNames[colIndex].toStdString()));
			options->add("levels",	new OptionVariables(stdlevels));
			
			std::vector<std::string> tempValues;
			for (QVariant val : _values[colIndex].toStdVector())
				tempValues.push_back(val.toString().toStdString());
			options->add("values",	new OptionTerm(tempValues));

			allOptions.push_back(options);
		}

		_boundTo->setValue(allOptions);
	}
}
