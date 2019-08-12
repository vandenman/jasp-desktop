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
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0
import JASP.Theme 1.0

Form
{
    VariablesForm
    {
        AvailableVariablesList { name: "allVariablesList" }
        AssignedVariablesList { name: "dependent";	title: qsTr("Dependent Variable");  	suggestedColumns: ["nominal", "ordinal"]; singleVariable: true	}
        AssignedVariablesList { name: "covariates";	title: qsTr("Covariates");              suggestedColumns: ["scale"]									}
        AssignedVariablesList { name: "factors";	title: qsTr("Factors");                 suggestedColumns: ["nominal", "ordinal"];itemType: "fixedFactors" }
        AssignedVariablesList { name: "wlsWeights";	title: qsTr("WLS Weights (optional)");  suggestedColumns: ["scale"]; singleVariable: true; debug: true	}
    }

    BayesFactorType { }
    
    Group
	{
		title: qsTr("Output")
		columns: 2

		CheckBox{ name: "postSummaryTable"; label: qsTr("Posterior summary"); id: postSummaryTable
            RadioButtonGroup
            {
                name: "effectsType"
                RadioButton { value: "allModels";		label: qsTr("Across all models");   checked: true	}
                RadioButton { value: "matchedModels";	label: qsTr("Across matched models")				}
            }
        }

		DropDown
		{
			name: "summaryType"
			enabled: postSummaryTable.checked || postSummaryPlot.checked
			indexDefaultValue: 3
			values: [
				{ label: qsTr("Best model"),			value: "best"		},
				{ label: qsTr("Most complex model"),	value: "complex"	},
				{ label: qsTr("Median model"),			value: "median"		},
				{ label: qsTr("Model averaged"),		value: "averaged"	}
			]
		}

		CheckBox
		{
			name: "postSummaryPlot"
			label: qsTr("Plot of coefficients")
			id: postSummaryPlot
			CheckBox { name: "omitIntercept"; label: qsTr("Omit intercept") }

		}

		CIField
		{
			name: "posteriorSummaryPlotCredibleIntervalValue"
			label: qsTr("Credible interval")
			enabled: postSummaryTable.checked || postSummaryPlot.checked
		}
	}    

    RadioButtonGroup
    {
        name: "bayesFactorOrder"
        title: qsTr("Order")
        RadioButton { value: "bestModelTop"; label: qsTr("Compare to best model"); checked: true	}
        RadioButton { value: "nullModelTop"; label: qsTr("Compare to null model")					}
    }
    
    RadioButtonGroup
	{
		name: "shownModels"
		title: qsTr("Limit No. Models Shown")
		RadioButton { value: "unlimited"; label: qsTr("No") }
		RadioButton { 
			value: "limited"
			label: qsTr("Yes, show best")
			checked: true
			childrenOnSameRow: true
			IntegerField { name: "numShownModels"; defaultValue: 10; min: 1}
		}
	}  

    Section
    {
        title: qsTr("Model")

        VariablesForm
        {
            height: 200

            AvailableVariablesList
            {
                name: "availableTerms"
                title: qsTr("Components")
                source: ['covariates', 'factors']
                width: parent.width / 4
            }
            AssignedVariablesList
            {
                name: "modelTerms"
                title: qsTr("Model terms")
                width: parent.width * 5 / 9
                listViewType: "Interaction"

                ExtraControlColumn
                {
                    type: "CheckBox"
                    name: "isNuisance"
                    title: qsTr("Add to null model")
                    purpose: "nuisance"
                }
            }
        }

    }

    Section
    {
        title: qsTr("Statistics")

        Group
        {
            title: qsTr("Descriptives")
            CheckBox { name: "factorDescriptivesOpt"; label: qsTr("Factor descriptives") }
        }

        Group
        {
            title: qsTr("Performance Diagnostics")
            CheckBox
            {
                name: "confusionMatrixOpt";	label: qsTr("Confusion matrix")
                CheckBox { name: "confusionMatrixProportions";	label: qsTr("Proportions") }
            }
        }

        Group
        {
            title: qsTr("Regression Coefficients")
            CheckBox { name: "coeffEstimates";	label: qsTr("Estimates"); checked: true
                CheckBox
                {
                    name: "coeffEstimatesBootstrapping"; label: qsTr("From")
                    childrenOnSameRow: true
                    IntegerField
                    {
                        name: "coeffEstimatesBootstrappingReplicates"
                        defaultValue: 5000
                        fieldWidth: 50
                        min: 100
                        afterLabel: qsTr("bootstraps")
                    }
                }

            }
            CheckBox { name: "stdCoeff";		label: qsTr("Standardized coefficients")			}
            CheckBox { name: "oddsRatios";		label: qsTr("Odds ratios")						}
            CheckBox
            {
                name: "coeffCI";				label: qsTr("Confidence intervals")
                CIField {	name: "coeffCIInterval"; label: "Interval" }
                CheckBox {		name: "coeffCIOR";		label: qsTr("Odds ratio scale")		}
            }
            CheckBox { name: "robustSEOpt";		label: qsTr("Robust standard errors")		}
            CheckBox { name: "VovkSellkeMPR";	label: qsTr("Vovk-Sellke maximum p-ratio")	}
        }

        Group
        {
            title: qsTr("Performance Metrics")
            CheckBox { name: "AUC";			label: qsTr("AUC")					}
            CheckBox { name: "Sens";		label: qsTr("Sensitivity / Recall")	}
            CheckBox { name: "Spec";		label: qsTr("Specificity")			}
            CheckBox { name: "Prec";		label: qsTr("Precision")				}
            CheckBox { name: "Fmsr";		label: qsTr("F-measure")				}
            CheckBox { name: "BrierScr";	label: qsTr("Brier score")			}
            CheckBox { name: "Hmsr";		label: qsTr("H-measure")				}
        }

        Group
        {   title: qsTr("Residuals")
            CheckBox
            {
                name: "casewiseDiagnostics";	label: qsTr("Casewise diagnostics")
                RadioButtonGroup
                {
                    name: "casewiseDiagnosticsType"
                    RadioButton
                    {
                        value: "residualZ"; label: qsTr("Standard residual >"); checked: true
                        childrenOnSameRow: true
                        DoubleField { name: "casewiseDiagnosticsResidualZ"; defaultValue: 3	}
                    }
                    RadioButton
                    {
                        value: "cooksDistance";	label: qsTr("Cook's distance >")
                        childrenOnSameRow: true
                        DoubleField { name: "casewiseDiagnosticsCooksDistance";	defaultValue: 1	}
                    }
                    RadioButton { value: "allCases"; label: qsTr("All")										}
                }
            }
        }

    }

    Section
    {
        title: qsTr("Plots")

        Group
        {
            title: qsTr("Inferential Plots")
            CheckBox
            {
                name: "estimatesPlotsOpt"; label: qsTr("Display conditional estimates plots")
                CIField {	name: "estimatesPlotsCI";	label: qsTr("Confidence interval") }
                CheckBox {		name: "showPoints";			label: qsTr("Show data points")						}
            }
        }

        Group
        {
            title: qsTr("Residual Plots")
            CheckBox { name: "predictedPlotOpt";		label: qsTr("Predicted - residual plot")			}
            CheckBox { name: "predictorPlotsOpt";		label: qsTr("Predictor - residual plots")		}
            CheckBox { name: "squaredPearsonPlotOpt";	label: qsTr("Squared Pearson residuals plot")	}
        }

        RadioButtonGroup
        {
            name: "residualType"
            title: qsTr("Residual Type")
            RadioButton { value: "deviance";	label: qsTr("Deviance");	checked: true   }
            RadioButton { value: "pearson";		label: qsTr("Pearson")					}
        }
    }
    
    Section
	{
		title: qsTr("Advanced Options")
		
		RadioButtonGroup
		{
			name: "priorRegressionCoefficients"
			title: qsTr("Prior")

			RadioButton { value: "AIC";			label: qsTr("AIC")		}
			RadioButton { value: "BIC";			label: qsTr("BIC")		}
			RadioButton { value: "EB-global";	label: qsTr("EB-global")	}
			RadioButton { value: "EB-local";	label: qsTr("EB-local")	}
			RadioButton { value: "g-prior";		label: qsTr("g-prior")	}
			GridLayout
			{
				rowSpacing: Theme.rowGroupSpacing
				columnSpacing: 0
				Group
				{
					RadioButton { value: "hyper-g";			label: qsTr("Hyper-g!");			id: hyperg			}
					RadioButton { value: "hyper-g-laplace";	label: qsTr("Hyper-g-Laplace");		id: hyperglaplace	}
					RadioButton { value: "hyper-g-n";		label: qsTr("Hyper-g-n");			id: hypergn			}
				}
				DoubleField
				{
					name: "alpha";
					label: qsTr("alpha");
					enabled: hyperg.checked || hyperglaplace.checked || hypergn.checked
					defaultValue: 3.0
				}
				RadioButton { value: "JZS"; label: qsTr("JZS"); checked: true; id: jzs }
				DoubleField
				{
					name: "rScale"
					label: qsTr("r scale")
					enabled: jzs.checked
					fieldWidth: 50
					defaultValue: 0.354
					max: 100000
				}
			}
		}
			
		ColumnLayout
		{
			RadioButtonGroup
			{
				name: "modelPrior"
				title: qsTr("Model Prior")
				RadioButton
				{
					value: "beta.binomial"; label: qsTr("Beta binomial"); checked: true
					childrenOnSameRow: true
					childrenArea.columnSpacing: 1
					DoubleField { name: "betaBinomialParamA"; label: qsTr("a");  defaultValue: 1 }
					DoubleField { name: "betaBinomialParamB"; label: qsTr("b");  defaultValue: 1 }
				}
				RadioButton
				{
					value: "Bernoulli"; label: qsTr("Bernouilli")
					childrenOnSameRow: true
					DoubleField { name: "bernoulliParam"; label: qsTr("p"); defaultValue: 0.5; max: 1; decimals: 2 }
				}
				RadioButton { value: "uniform"; label: qsTr("Uniform") }
			}

			RadioButtonGroup
			{
				name: "samplingMethod"
				title: qsTr("Sampling Method")
				RadioButton
				{
					value: "BAS"; label: qsTr("BAS"); checked: true
					childrenOnSameRow: true
					IntegerField { name: "numberOfModels"; label: qsTr("No. models"); defaultValue: 0; max: 100000000 }
				}
				RadioButton
				{
					value: "MCMC"; label: qsTr("MCMC")
					childrenOnSameRow: true
					IntegerField { name: "iterationsMCMC"; label: qsTr("No. samples"); defaultValue: 0; max: 100000000 }
				}
			}

			Group
			{
				title: qsTr("Numerical Accuracy")
				IntegerField
				{
					name: "nSimForCRI"
					label: qsTr("No. samples for credible interval")
					defaultValue: 1000
					fieldWidth: 50
					min: 100
					max: 1000000
				}
			}
		}
	}

//    Section
//    {
//        title: qsTr("Statistics")

//        Group
//        {
//            title: qsTr("Descriptives")
//            CheckBox { name: "factorDescriptivesOpt"; label: qsTr("Factor descriptives") }
//        }

//        Group
//        {
//            title: qsTr("Performance Diagnostics")
//            CheckBox
//            {
//                name: "confusionMatrixOpt";	label: qsTr("Confusion matrix")
//                CheckBox { name: "confusionMatrixProportions";	label: qsTr("Proportions") }
//            }
//        }

//        Group
//        {
//            title: qsTr("Regression Coefficients")
//            CheckBox { name: "coeffEstimates";	label: qsTr("Estimates"); checked: true			}
//            CheckBox { name: "stdCoeff";		label: qsTr("Standardized coefficients")			}
//            CheckBox { name: "oddsRatios";		label: qsTr("Odds ratios")						}
//            CheckBox
//            {
//                name: "coeffCI";				label: qsTr("Confidence intervals")
//                PercentField {	name: "coeffCIInterval"; label: "Interval"; defaultValue: 95	}
//                CheckBox {		name: "coeffCIOR";		label: qsTr("Odds ratio scale")		}
//            }
//            CheckBox { name: "robustSEOpt";		label: qsTr("Robust standard errors")		}
//            CheckBox { name: "VovkSellkeMPR";	label: qsTr("Vovk-Sellke maximum p-ratio")	}
//        }

//        Group
//        {
//            title: qsTr("Performance metrics")
//            CheckBox { name: "AUC";			label: qsTr("AUC")					}
//            CheckBox { name: "Sens";		label: qsTr("Sensitivity / Recall")	}
//            CheckBox { name: "Spec";		label: qsTr("Specificity")			}
//            CheckBox { name: "Prec";		label: qsTr("Precision")				}
//            CheckBox { name: "Fmsr";		label: qsTr("F-measure")				}
//            CheckBox { name: "BrierScr";	label: qsTr("Brier score")			}
//            CheckBox { name: "Hmsr";		label: qsTr("H-measure")				}
//        }
//    }

//    Section
//    {
//        title: qsTr("Plots")

//        Group
//        {
//            title: qsTr("Inferential plots")
//            CheckBox
//            {
//                name: "estimatesPlotsOpt"; label: qsTr("Display conditional estimates plots")
//                PercentField {	name: "estimatesPlotsCI";	label: qsTr("Confidence interval"); defaultValue: 95 }
//                CheckBox {		name: "showPoints";			label: qsTr("Show data points")						}
//            }
//        }

//        Group
//        {
//            title: qsTr("Residual plots")
//            CheckBox { name: "predictedPlotOpt";		label: qsTr("Predicted - residual plot")			}
//            CheckBox { name: "predictorPlotsOpt";		label: qsTr("Predictor - residual plots")		}
//            CheckBox { name: "squaredPearsonPlotOpt";	label: qsTr("Squared Pearson residuals plot")	}
//        }

//        RadioButtonGroup
//        {
//            name: "residualType"
//            title: qsTr("Residual type")
//            RadioButton { value: "deviance";	label: qsTr("Deviance");	checked: true   }
//            RadioButton { value: "pearson";		label: qsTr("Pearson")					}
//        }
//    }
}
