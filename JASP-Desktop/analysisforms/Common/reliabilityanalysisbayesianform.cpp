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

#include "reliabilityanalysisbayesianform.h"
#include "ui_reliabilityanalysisbayesianform.h"

ReliabilityAnalysisBayesianForm::ReliabilityAnalysisBayesianForm(QWidget *parent) :
	AnalysisForm("ReliabilityAnalysisBayesianForm", parent),
	ui(new Ui::ReliabilityAnalysisBayesianForm)
{
	ui->setupUi(this);

	ui->reverseScaledItems->setLabels("Normal-Scaled Items", "Reverse-Scaled Items");

	ui->listAvailableVariables->setModel(&_availableVariablesModel);

	_variableListModel = new TableModelVariablesAssigned(this);
	_variableListModel->setSource(&_availableVariablesModel);
	_variableListModel->setVariableTypesSuggested(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal);
	_variableListModel->setVariableTypesAllowed(Column::ColumnTypeNominal | Column::ColumnTypeOrdinal | Column::ColumnTypeScale);
	ui->variables->setModel(_variableListModel);

    ui->listAvailableVariables->setDoubleClickTarget(ui->variables);
    ui->variables->setDoubleClickTarget(ui->listAvailableVariables);
    ui->assignButton->setSourceAndTarget(ui->listAvailableVariables, ui->variables);

	connect(_variableListModel, SIGNAL(assignmentsChanging()), this, SLOT(variablesChanging()));
	connect(_variableListModel, SIGNAL(assignmentsChanged()), this, SLOT(variablesChanged()));
	connect(_variableListModel, SIGNAL(termsChanged()), this, SLOT(termsChanged()));

	ui->containerRevScaledItems->hide();
    ui->containerAdvOptions->hide();

}

void ReliabilityAnalysisBayesianForm::variablesChanging()
{
	if (_options != NULL)
		_options->blockSignals(true);
}

void ReliabilityAnalysisBayesianForm::variablesChanged()
{
	Terms variablesAvailable;

	variablesAvailable.add(_variableListModel->assigned());

	ui->reverseScaledItems->setVariables(variablesAvailable);

	if (_options != NULL)
		_options->blockSignals(false);

}

ReliabilityAnalysisBayesianForm::~ReliabilityAnalysisBayesianForm()
{
	delete ui;
}

void ReliabilityAnalysisBayesianForm::bindTo(Options *options, DataSet *dataSet)
{

	AnalysisForm::bindTo(options, dataSet);

	variablesChanging();

	variablesChanged();

}
