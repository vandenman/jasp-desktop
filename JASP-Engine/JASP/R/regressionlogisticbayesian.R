#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
# regLogB: regressionLogisticBayesian
RegressionLogisticBayesian <- function(jaspResults, dataset, options) {

  dataset <- .regLogBayReadData(dataset, options)
  .regLogBayErrorHandling      (dataset, options)
 
  model <- .regLogBayTableModelComparison(jaspResults, dataset, options)
  
  .regLogBayTableSummary(jaspResults, options, model)
  
  return()
  
  # # STATE SYSTEM
  # # load state
  # state <- .retrieveState()
  # 
  # # init output variables
  # lrObj <- # glm object
  # modelSummary <- # fit/summary table
  # estimatesTable <- # parameter estimates table
  # confusionMatrix <- # confusion matrix table
  # perfMetrics <- # performance metrics of full model
  # estimatesPlots <- # plots for estimates
  # predictedPlot <- # predicted - residuals plot
  # predictorPlots <- # predictor - residuals plots
  # squaredPearsonPlot <- # squared pearson - predicted prob plot
  # factorDescriptives <- # factor descriptives table
  # NULL
  # 
  # # diff check
  # if (!is.null(state) && perform == "run") {
  #   diff <- .diff(options, state[["options"]])
  #   with(diff, { # with(diff, {}) makes us need "<<-" to assign to global env
  #     if (!any(dependent, covariates, factors, wlsWeights, modelTerms,
  #               includeIntercept, wlsWeights, method)) {
  #       lrObj <<- state[["lrObj"]]
  #       modelSummary <<- state[["modelSummary"]]
  # 
  #       if (!any(coeffEstimates, coeffCI, coeffCIInterval, coeffCIOR, stdCoeff,
  #                oddsRatios, VovkSellkeMPR, robustSEOpt)) {
  #         # estimates table can be reused
  #         estimatesTable <<- state[["estimatesTable"]]
  #       }
  # 
  #       if (!any(confusionMatrixOpt, confusionMatrixProportions)) {
  #         # confusionMatrix can be reused
  #         confusionMatrix <<- state[["confusionMatrix"]]
  #       }
  #       if (!any(AUC, Sens, Spec, Prec, Fmsr, BrierScr, Hmsr)) {
  #         # metrics table can be reused
  #         perfMetrics <<- state[["perfMetrics"]]
  #       }
  # 
  #       if (!any(estimatesPlotsOpt, estimatesPlotsCI, plotWidth, plotHeight,
  #                showPoints)) {
  #         # estimates plots can be reused
  #         estimatesPlots <<- state[["estimatesPlots"]]
  #       }
  # 
  #       if (!any(predictedPlotOpt, plotWidth, plotHeight, residualType)) {
  #         # predicted - residuals plot can be reused
  #         predictedPlot <<- state[["predictedPlot"]]
  #       }
  # 
  #       if (!any(predictorPlotsOpt, plotWidth, plotHeight, residualType)) {
  #         # predictor - residuals plots can be reused
  #         predictorPlots <<- state[["predictorPlots"]]
  #       }
  # 
  #       if (!any(squaredPearsonPlotOpt, plotWidth, plotHeight)) {
  #         # squared pearson plot can be reused
  #         squaredPearsonPlot <<- state[["squaredPearsonPlot"]]
  #       }
  # 
  #       if (!any(factorDescriptivesOpt)) {
  #         # descriptives table can be reused
  #         factorDescriptives <<- state[["factorDescriptives"]]
  #       }
  #     }
  #   })
  # } else if (!is.null(state)) {
  #   lrObj <- state[["lrObj"]]
  #   modelSummary <- state[["modelSummary"]]
  #   estimatesTable <- state[["estimatesTable"]]
  #   confusionMatrix <- state[["confusionMatrix"]]
  #   perfMetrics <- state[["perfMetrics"]]
  #   estimatesPlots <- state[["estimatesPlots"]]
  #   predictedPlot <- state[["predictedPlot"]]
  #   predictorPlots <- state[["predictorPlots"]]
  #   squaredPearsonPlot <- state[["squaredPearsonPlot"]]
  #   factorDescriptives <- state[["factorDescriptives"]]
  # }


  # RESULTS GENERATION
  # for each non-null result, generate results
  if (is.null(lrObj)) {
    lrObj <- .jaspGlm(dataset, options, perform, type = "binomial")
  }


  if (is.null(modelSummary)) {
    modelSummary <- .glmModelSummary(lrObj, options, perform, type = "binomial")
  }

  if (is.null(estimatesTable) && options[["coeffEstimates"]]) {
    estimatesTable <- .glmEstimatesTable(lrObj, options, perform,
                                         type = "binomial")
  }

  if (is.null(confusionMatrix) && options[["confusionMatrixOpt"]]) {
    confusionMatrix <- .glmConfusionMatrix(lrObj, options, perform,
                                           type = "binomial")
  }

  wantsPerfMetrics <- with(options, any(AUC, Sens, Spec, Prec, Fmsr, BrierScr,
                                        Hmsr))
  if (is.null(perfMetrics) && wantsPerfMetrics) {
    perfMetrics <- .glmPerformanceMetrics(lrObj, options, perform,
                                          type = "binomial")
  }

  perfDiagnostics <- list("confusionMatrix" = confusionMatrix,
                          "perfMetrics" = perfMetrics,
                          "title" = "Performance Diagnostics")

  if (is.null(estimatesPlots) && options[["estimatesPlotsOpt"]]) {
    estimatesPlots <- .glmEstimatesPlots(lrObj, options, perform,
                                         type = "binomial")
  }

  if (is.null(predictedPlot) && options[["predictedPlotOpt"]]) {
    predictedPlot <- .glmPredictedResidualsPlot(lrObj, options, perform,
                                                type = "binomial")
  }

  if (is.null(predictorPlots) && options[["predictorPlotsOpt"]]) {
    predictorPlots <- .glmPredictorResidualsPlots(lrObj, options, perform,
                                                  type = "binomial")
  }

  if (is.null(squaredPearsonPlot) && options[["squaredPearsonPlotOpt"]]) {
    squaredPearsonPlot <- .glmSquaredPearsonResidualsPlot(lrObj, options,
                                                          perform,
                                                          type = "binomial")
  }

  if(is.null(factorDescriptives) && options[["factorDescriptivesOpt"]]) {
    factorDescriptives <- .glmFactorDescriptives(dataset, options, perform,
                                                type = "binomial")

  }

  return()
}

.regLogBayReadData <- function(dataset, options) {
  
  if (!is.null(dataset))
    return(dataset)
  
  numericVars <- unlist(c(options[["covariates"]], options[["wlsWeights"]]))
  numericVars <- numericVars[numericVars != ""] 
  factorVars <- unlist(c(options[["dependent"]], options[["factors"]]))
  factorVars <- factorVars[factorVars != ""]

  dataset <- .readDataSetToEnd(
    columns  = c(numericVars, factorVars),
    # columns.as.numeric = numericVars,
    # columns.as.factor = factorVars,
    exclude.na.listwise = c(numericVars, factorVars)
  )
  for (var in .v(factorVars))
    dataset[[var]] <- factor(dataset[[var]])

  return(dataset)
}

.regLogBayErrorHandling <- function(dataset, options) {
  
  if (options[["dependent"]] != "") {
    .hasErrors(dataset, perform, type = "factorLevels",
               factorLevels.target = options[["dependent"]],
               factorLevels.amount = '!= 2',
               exitAnalysisIfErrors = TRUE)
  }
  
  if (!identical(options[["wlsWeights"]], "") || identical(options[["wlsWeights"]], list())) {
    .hasErrors(dataset, perform, type = "limits",
               limits.target = options[["wlsWeights"]],
               limits.min = 0, limits.max = Inf,
               exitAnalysisIfErrors = TRUE)
  }
  
  if (length(options[["covariates"]]) != 0) {
    .hasErrors(dataset, perform,
               type = c("observations", "infinity", "variance"),
               all.target = options[["covariates"]],
               observations.amount = "< 2",
               exitAnalysisIfErrors = TRUE)
  }
  
  # ncol > 1 => at least one predictor
  if (options[["dependent"]] != "" && ncol(dataset) > 1L && nrow(dataset) == 0L)
    .quitAnalysis("Dataset has no observations, check for missing values!")
}

.regLogBayFitBAS <- function(dataset, options) {

  options[["includeIntercept"]] <- TRUE # <- needed for .createGlmFormula
  formula <- .createGlmFormula(options)
  
  if (!identical(options[["wlsWeights"]], "") && !identical(options[["wlsWeights"]], list())) {
    idx <- colnames(dataset) == .v(options[["wlsWeights"]])
    wlsWeights <- dataset[, idx]
    dataset    <- dataset[, !idx, drop = FALSE]
  } else {
    wlsWeights <- rep.int(1L, nrow(dataset))
  }
  
  # TODO: BAS produces output for the predictors explain this
  for (var in .v(options[["factors"]]))
    levels(dataset[[var]]) <- paste0(" (", factor(dataset[[var]]), ")")
  
  model <- try(BAS::bas.glm(
    formula     = formula, 
    family      = stats::binomial(link = "logit"),
    data        = dataset,
    # weights     = wlsWeights,
    renormalize = TRUE
  ))
  if (!isTryError(model)) {
    
    # rename the variables from ".v(name) (level): .v(name) (level)" to "name (level) * name (level)".
    interactionSymbol <- "\u2009\u273B\u2009"
    namesx <- model[["namesx"]][-1L]
    newNamesx <- character(length(namesx))
    namesxSplit <- strsplit(namesx, ":")
    for (i in seq_along(namesxSplit)) {
      nmSplit <- strsplit(namesxSplit[[i]], " ")
      for (j in seq_along(nmSplit)) {
        isVariable <- !startsWith(nmSplit[[j]], "(")
        nmSplit[[j]][isVariable] <- .unv(nmSplit[[j]][isVariable])
        nmSplit[[j]] <- paste(nmSplit[[j]], collapse = " ")
      }
      namesxSplit[[i]] <- paste(unlist(nmSplit), collapse = interactionSymbol)
    }
    model[["newNamesx"]] <- c("Intercept", unlist(namesxSplit))
    model[["BFM"]] <- (model[["postprobs"]] / (1 - model[["postprobs"]])) / (model[["priorprobs"]] / (1 - model[["priorprobs"]]))
    
  }
  
  return(model)
}

.regLogBayTableModelComparison <- function(jaspResults, dataset, options) {

  if (is.null(jaspResults[["modelContainer"]]))
    jaspResults[["modelContainer"]] <- createJaspContainer(dependencies = c("dependent", "covariates", "factors", "modelTerms"))
  else if (!is.null(jaspResults[["modelContainer"]][["modelComparisonTable"]]))
    return(jaspResults[["modelContainer"]][["modelState"]]$object)

  modelComparisonTable <- .regLogBayTableModelComparisonMarkup(options)
  jaspResults[["modelContainer"]][["modelComparisonTable"]] <- modelComparisonTable

  results <- jaspResults[["modelContainer"]][["modelState"]]$object
  results["ready"] <- .regLogBayIsReady(options)
  if (results[["ready"]]) { 
    if (is.null(results[["model"]]))
      results[["model"]] <- .regLogBayFitBAS(dataset, options)

    if (isTryError(results[["model"]]))
      jaspResults[["modelContainer"]]$setError(.extractErrorMessage(results[["model"]]))
    else
      .regLogBayFillTableModelComparison(modelComparisonTable, results[["model"]], options)

    jaspResults[["modelContainer"]][["modelState"]] <- createJaspState(results)
  }
  return(results)
}

.regLogBayFillTableModelComparison <- function(jaspTable, model, options) {

  allTerms <- model[["newNamesx"]]
  modelNames <- character(length(model[["which"]]))
  modelNames[1L] <- "Null model" # TODO: including
  for (i in 2:length(modelNames)) {
    idx <- model[["which"]][[i]][-1]
    modelNames[i] <- paste(allTerms[idx], collapse = " + ")
  }

  idx <- if (options[["bayesFactorOrder"]] == "bestModelTop") which.max(model$logmarg) else 1L
  bf <- switch(
    options[["bayesFactorType"]],
    "BF10"    = exp(model$logmarg      - model$logmarg[idx]),
    "BF01"    = exp(model$logmarg[idx] - model$logmarg),
    "LogBF10" =     model$logmarg      - model$logmarg[idx]
  )

  table <- data.frame(
    Models         = modelNames,
    priorProbModel = model[["priorprobs"]],
    postProbModel  = model[["postprobs"]],
    BF             = bf,
    BFM            = model[["BFM"]],
    Deviance       = model[["deviance"]]
  )
  table <- table[order(table[, "BF"], decreasing = TRUE), ]

  jaspTable$setData(table)
  return(table)
  
}

.regLogBayTableSummary <- function(jaspResults, options, model) {
  # browser()
  # if (!options$summary)
  
  if (jaspResults[["modelContainer"]]$getError() || ! model[["ready"]])
    return()
  

  
  
}

# helper functions ----
.regLogBayCitations <- c(
  "Clyde2017"     = "Clyde, M. A. (2017). BAS: Bayesian Adaptive Sampling for Bayesian Model Averaging. (Version 1.5.3)[Computer software].", 
  "ClydeEtal2011" = "Clyde, M. A., Ghosh, J., & Littman, M. L. (2011). Bayesian adaptive sampling for variable selection and model averaging. Journal of Computational and Graphical Statistics, 20, 80-101."
)

.regLogBayIsReady <- function(options) {
  return(list(ready = all(
    options[["dependent"]] != "",
    length(options[["covariates"]]) > 0L || length(options[["factors"]]) > 0L,
    length(options[["modelTerms"]]) > 0L
  )))
}

.regLogBayTableModelComparisonMarkup <- function(options) {
    
    modelComparisonTable <- createJaspTable(title = "Model Comparison")
    modelComparisonTable$dependOn(c("bayesFactorType", "bayesFactorOrder"))#, "shownModels", "numShownModels"))
    modelComparisonTable$addCitation(.regLogBayCitations[c("Clyde2017", "ClydeEtal2011")])
  
    bf.title <- switch(
      options[["bayesFactorType"]],
      "BF10"  = "BF<sub>10</sub>",
      "BF01"  = "BF<sub>01</sub>",
      "LogBF10" = "Log(BF<sub>10</sub>)"
    )
  
    modelComparisonTable$addColumnInfo(name = "Models",         type = "string")
    modelComparisonTable$addColumnInfo(name = "priorProbModel", type = "number", title = "P(M)")
    modelComparisonTable$addColumnInfo(name = "postProbModel",  type = "number", title = "P(M|data)")
    modelComparisonTable$addColumnInfo(name = "BFM",            type = "number", title = "BF<sub>M</sub>")
    modelComparisonTable$addColumnInfo(name = "BF",             type = "number", title = bf.title)
    modelComparisonTable$addColumnInfo(name = "Deviance",       type = "number", format = "dp:3", title = "Deviance")
    return(modelComparisonTable)
  }
