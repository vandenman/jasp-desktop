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

  dataset <- .regLogBayReadData(dataset, options)
  .regLogBayErrorHandling      (dataset, options)
 
  model <- .regLogBayTableModelComparison(jaspResults, dataset, options)
  
  .regLogBayTableSummary(jaspResults, options, model)
  
  return()
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
    columns.as.numeric = numericVars,
    columns.as.factor = factorVars,
    exclude.na.listwise = c(numericVars, factorVars)
  )
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

  ready <- options[["dependent"]] != "" && !(length(options[["covariates"]]) == 0 || length(options[["factors"]]) == 0L)
  if (!ready)
    return(NULL)
  
  options[["includeIntercept"]] <- TRUE # <- needed for .createGlmFormula
  formula <- .createGlmFormula(options)
  
  if (!identical(options[["wlsWeights"]], "") && !identical(options[["wlsWeights"]], list())) {
    idx <- colnames(dataset) == .v(options[["wlsWeights"]])
    wlsWeights <- dataset[, idx]
    dataset    <- dataset[, !idx, drop = FALSE]
  } else {
    wlsWeights <- rep.int(1L, nrow(dataset))
  }
browser()
debugonce(BAS::bas.glm)
  model <- try(BAS::bas.glm(
    formula     = formula, 
    family      = stats::binomial(link = "logit"),
    data        = dataset,
    # weights     = wlsWeights,
    renormalize = TRUE
  ))
  if (!isTryError(model)) {
    
    namesx <- model[["namesx"]][-1L]
    vars  <- all.vars(model[["terms"]])[-1]
    
    
    
    # regex <- paste0(paste0(vars, collapse = "|"))
    # ll <- stringr::str_split_fixed(namesx, regex, n = Inf)
    # unique(gsub(":", "", ll)[ll != ""]))
    # regex2 <- paste0("^((?!", regex, ").)*$")
    # 
    
    browser()
    
    tt <- list(model = model$namesx, terms = 0)
    attr(tt[["terms"]], "intercept") <- FALSE
    sapply(model$namesx[-1], .formatTerm, glmModel = tt)
    
    model.frame(model$terms, dataset, contrasts)
    fromto <- attr(model[["terms"]], "term.labels")
    attr(model[["terms"]], "dataClasses")
    
    
    
    regex <- paste0(paste0(vars, collapse = "|"))
    regex2 <- paste0("^((?!", regex, ").)*$")
                    
    regex <- paste0("(?<=(", paste0(vars, collapse = "|"), "|\\:))")
    
    
    all <- unlist
    
    ll <- stringr::str_split(model$namesx, regex2)
    
    
    lookup <- matrix(NA, length(ll), 2L)
    for (i in seq_along(ll)) {
      
    }
    
    
    idx <- pmatch(fromto, model[["namesx"]])
    pmatch(model[["namesx"]], fromto)
    startsWith(model[["namesx"]][3], fromto[2])
    stringr::str_sub(model[["namesx"]][idx], nchar(fromto)+1)
    
    
    formula
    stringr::str_replace_all(model$namesx[-1L], fromto)
    
    model$namesx
    
  }
  
  return(model)
}

.regLogBayTableModelComparison <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["modelComparisonTable"]]))
    return()
  
  modelComparisonTable <- createJaspTable(title = "Model Comparison")
  jaspResults[["modelComparisonTable"]] <- modelComparisonTable
  
  modelComparisonTable$dependOnOptions(c("bayesFactorType", "bayesFactorOrder"))#, "shownModels", "numShownModels"))
  modelComparisonTable$addCitation(
    "Clyde, M. A. (2017). BAS: Bayesian Adaptive Sampling for Bayesian Model Averaging. (Version 1.5.3)[Computer software]."
  )
  modelComparisonTable$addCitation(
    "Clyde, M. A., Ghosh, J., & Littman, M. L. (2011). Bayesian adaptive sampling for variable selection and model averaging. 
    Journal of Computational and Graphical Statistics, 20, 80-101."
  )

  bf.title <- switch(
    options[["bayesFactorType"]],
    "BF10"  = "BF<sub>10</sub>",
    "BF01"  = "BF<sub>01</sub>",
    "LogBF10" = "Log(BF<sub>10</sub>)"
  )

  modelComparisonTable$addColumnInfo(name = "Models",         type = "string")
  modelComparisonTable$addColumnInfo(name = "priorProbModel", type = "number", format = "sf:4;dp:3", title = "P(M)")
  modelComparisonTable$addColumnInfo(name = "postProbModel",  type = "number", format = "sf:4;dp:3", title = "P(M|data)")
  modelComparisonTable$addColumnInfo(name = "BFM",            type = "number", format = "sf:4;dp:3", title = "BF<sub>M</sub>")
  modelComparisonTable$addColumnInfo(name = "BF",             type = "number", format = "sf:4;dp:3", title = bf.title)
  modelComparisonTable$addColumnInfo(name = "Deviance",       type = "number", format = "dp:3",      title = "Deviance")
  modelComparisonTable$setExpectedRows(1) 
  
  model <- jaspResults[["stateModel"]]$object
  if (is.null(model))
    model <- .regLogBayFitBAS(dataset, options)

  if (!isTryError(model)) {
    table <- .regLogBayFillTableModelComparison(modelComparisonTable, model, options)
    jaspResults[["stateModel"]] <- createJaspState(model)
    # jaspResults[["stateTable"]] <- createJaspState(model)
    return(model)
  } else {
    modelComparisonTable$setError(.extractErrorMessage(model))
    return(NULL)
  }
}

.regLogBayFillTableModelComparison <- function(jaspTable, model, options, stateTable) {
  
  allTerms <- .unvf(model[["namesx"]][-1L]) # drop "Intercept"
  modelsNames <- character(length(model[["which"]]))
  modelsNames[[1L]] <- "Null model" # TODO: including
  for (i in 2:length(modelsNames)) {
    idx <- model[["which"]][[i]][-1]
    modelsNames[i] <- paste(allTerms[idx], collapse = " + ")
  }

  idx <- if (options[["bayesFactorOrder"]] == "bestModelTop") which.max(model$logmarg) else 1L
  bf <- switch(
    options[["bayesFactorType"]],
    "BF10"    = exp(model$logmarg - model$logmarg[idx]),
    "BF01"    = exp(model$logmarg[idx] - model$logmarg),
    "LogBF10" = model$logmarg - model$logmarg[idx]
  )
  BFM <- (model[["postprobs"]] / (1 - model[["postprobs"]])) / (model[["priorprobs"]] / (1 - model[["priorprobs"]]))

  table <- data.frame(
    Models         = modelsNames,
    priorProbModel = model[["priorprobs"]],
    postProbModel  = model[["postprobs"]],
    BF             = bf,
    BFM            = BFM,
    Deviance       = model[["deviance"]]
  )
  table <- table[order(table[, "BF"], decreasing = TRUE), ]

  jaspTable$setData(table)
  return(table)
  
}

.regLogBayTableSummary <- function(jaspResults, options, model) {
  
  browser()
  
  
}