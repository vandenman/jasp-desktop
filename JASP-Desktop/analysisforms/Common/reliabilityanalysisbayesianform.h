//
// Copyright (C) 2018 University of Amsterdam
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

#ifndef RELIABILITYANALYSISBAYESIANFORM_H
#define RELIABILITYANALYSISBAYESIANFORM_H

#include "../analysisform.h"

namespace Ui {
class ReliabilityAnalysisBayesianForm;
}

class ReliabilityAnalysisBayesianForm : public AnalysisForm
{
	Q_OBJECT

public:
	explicit ReliabilityAnalysisBayesianForm(QWidget *parent = 0);
	~ReliabilityAnalysisBayesianForm();

	virtual void bindTo(Options *options, DataSet *dataSet) OVERRIDE;

private slots:
	void variablesChanging();
	void variablesChanged();

private:
	Ui::ReliabilityAnalysisBayesianForm *ui;

	TableModelVariablesAssigned *_variableListModel;
};

#endif // RELIABILITYANALYSISBAYESIANFORM_H
