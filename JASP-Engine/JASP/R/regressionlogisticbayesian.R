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
  .regLogBayPosteriorSummary(jaspResults, options, model)
  
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
  
  # we make a list with bas_obj and other stuff, because Merlise indexes with $prior and adding things to the bas_obj
  # can cause name clashes
  bas_obj <- try(BAS::bas.glm(
    formula     = formula, 
    family      = stats::binomial(link = "logit"),
    data        = dataset,
    # weights     = wlsWeights,
    renormalize = TRUE
  ))
  model <- list(bas_obj = bas_obj)
  if (!isTryError(model[["bas_obj"]])) {
    
    # rename the variables from ".v(name) (level): .v(name) (level)" to "name (level) * name (level)".
    model[["newNamesx"]] <- .regLogBayRenameNamesx(model[["bas_obj"]][["namesx"]][-1L])
    model[["BFM"]] <- (bas_obj[["postprobs"]] / (1 - bas_obj[["postprobs"]])) / (bas_obj[["priorprobs"]] / (1 - bas_obj[["priorprobs"]]))
    
    # model[["nuisanceTerms"]] <- isNuisance
    # fix for prior probs all returning 1 with uniform and bernoulli 0.5 priors
    model[["priorprobs"]] <- bas_obj[["priorprobs"]] / sum(bas_obj[["priorprobs"]])
    model[["formula"]] <- formula
    model[["weights"]] <- wlsWeights
    
  }
  
  return(model)
}

# model comparison table ----
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
  bas_obj <- model[["bas_obj"]]
  which <- bas_obj[["which"]]
  modelNames <- character(length(which))
  modelNames[1L] <- "Null model" # TODO: including
  for (i in 2:length(modelNames)) {
    idx <- which[[i]][-1L]
    modelNames[i] <- paste(allTerms[idx], collapse = " + ")
  }
  
  logmarg <- bas_obj[["logmarg"]]
  idx <- if (options[["bayesFactorOrder"]] == "bestModelTop") which.max(logmarg) else 1L
  bf <- switch(
    options[["bayesFactorType"]],
    "BF10"    = exp(logmarg      - logmarg[idx]),
    "BF01"    = exp(logmarg[idx] - logmarg),
    "LogBF10" =     logmarg      - logmarg[idx]
  )
  
  table <- data.frame(
    Models         = modelNames,
    priorProbModel = bas_obj[["priorprobs"]],
    postProbModel  = bas_obj[["postprobs"]],
    BF             = bf,
    BFM            = model[["BFM"]],
    Deviance       = bas_obj[["deviance"]]
  )
  table <- table[order(table[, "BF"], decreasing = TRUE), ]
  
  jaspTable$setData(table)
  return()
}

.regLogBayTableModelComparisonMarkup <- function(options) {
  
  modelComparisonTable <- createJaspTable(title = "Model Comparison")
  modelComparisonTable$dependOn(c("bayesFactorType", "bayesFactorOrder", "shownModels", "numShownModels"))
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

# model summary table & plots ----
.regLogBayPosteriorSummary <- function(jaspResults, options, results) {
  
  if (!(options[["postSummaryTable"]] || options[["postSummaryPlot"]]))
    return()
  
  if (is.null(jaspResults[["modelContainer"]][["summaryContainer"]]))
    jaspResults[["modelContainer"]][["summaryContainer"]] <- createJaspContainer()#dependencies = c(""))

  summaryObj <- jaspResults[["modelContainer"]][["summaryState"]]$object
  if (is.null(summaryObj) && !jaspResults[["modelContainer"]]$getError() && results[["ready"]]) {

    summaryObj <- try(.regLogBayCalculatePosteriorSummary(results[["model"]][["bas_obj"]], options))

    if (isTryError(summaryObj)) {
      jaspResults[["modelContainer"]]$setError(.extractErrorMessage(summaryObj))
    } else {
      summaryObjState <- createJaspState(summaryObj)#, dependencies = c(""))
      jaspResults[["modelContainer"]][["summaryState"]] <- summaryObjState
    }
  }
  .regLogBaySummaryTable(jaspResults, options, results, summaryObj)
  .regLogBaySummaryPlot (jaspResults, options, results, summaryObj)
  
  return()
}

.regLogBayCalculatePosteriorSummary <- function(bas_obj, options) {
  
  # TODO: move computation of CRI elsewhere
  estimator <- switch(
    options[["summaryType"]],
    best   = "HPM",
    median = "MPM",
    "BMA"
  )
  
  footnote <- NULL
  # only need to recalculate this if bas_obj was remade (which implies bas_obj[["posteriorSummary"]] is NULL)
  coefBMA <- coef(bas_obj, estimator = "BMA")
  conf95BMA <- try(stats::confint(coefBMA, level = 0.95, nsim = options[["nSimForCRI"]]))
  if (isTryError(conf95BMA)) {
    conf95BMA <- cbind(NA, NA, coefBMA[["postmean"]])
    rownames(conf95BMA) <- coefBMA[["namesx"]]
    colnames(conf95BMA) <- c("2.5%", "97.5%", "beta")
    conf95BMA[is.nan(conf95BMA)] <- NA
    footnote <- "Parameters estimates and/ or credible intervals could not be calculated."
  }
  
  # check if results of table and plots should match
  criVal <- options[["posteriorSummaryPlotCredibleIntervalValue"]]
  if (estimator == "BMA" && isTRUE(all.equal(criVal, 0.95))) { # what we show under Marginal Posterior distributions
    coef <- coefBMA
    conf95 <- conf95BMA
  } else {
    coef <- coef(bas_obj, estimator = estimator)
    conf95 <- stats::confint(coef, level = criVal, nsim = options[["nSimForCRI"]])
  }
  
  probne0 <- coefBMA[["probne0"]]
  coefficients <- bas_obj[["namesx"]]
  if (estimator == "HPM") {
    loopIdx <- which(abs(coef[["postmean"]]) > sqrt(.Machine[["double.eps"]]))
  } else if (estimator == "MPM") {
    loopIdx <- which(abs(coef[["postmean"]]) > sqrt(.Machine[["double.eps"]]))
    probne0 <- bas_obj[["probne0"]]
  } else {
    loopIdx <- seq_along(coefficients)
  }
  
  return(list(coef = coef, loopIdx = loopIdx, coefficients = coefficients, probne0 = probne0,
              conf95 = conf95, coefBMA = coefBMA, conf95BMA = conf95BMA, footnote = footnote,
              nSimForCRI = options[["nSimForCRI"]]))
}

.regLogBaySummaryTable <- function(jaspResults, options, results, summaryObj) {

  if (!options[["postSummaryTable"]])
    return()
  
  summaryContainer <- jaspResults[["modelContainer"]][["summaryContainer"]]
  summaryTable <- .regLogBaySummaryTableMarkup(options)
  summaryContainer[["summaryTable"]] <- summaryTable
  
  if (summaryContainer$getError() || !results[["ready"]])
    return()
  
  # from regressionlinearbayesian
  bas_obj <- results[["model"]][["bas_obj"]]
  
  priorModelProbs <- bas_obj[["priorprobs"]]
  postModelProbs  <- bas_obj[["postprobs"]]
  priorModelProbs <- priorModelProbs / sum(priorModelProbs)
  postModelProbs  <- postModelProbs  / sum(postModelProbs)
  inclMat         <- BAS:::list2matrix.which(bas_obj)

  if (options[["effectsType"]] == "allModels") {
    
    priorInclProbs <- priorModelProbs %*% inclMat
    postInclProbs  <- postModelProbs  %*% inclMat
    BFincl         <- (postInclProbs / (1 - postInclProbs)) /(priorInclProbs / (1 - priorInclProbs))
    
  } else {
    
    terms <- attr(bas_obj$terms, "factors")[-1, , drop = FALSE]
    rownames(terms) <- .unvf(rownames(terms))
    colnames(terms) <- .unvf(colnames(terms))
    inclMat <- inclMat[, -1, drop = FALSE] # drop intercept
    terms <- rbind(terms, matrix(FALSE, nrow = ncol(terms) - nrow(terms), ncol = ncol(terms)))
    diag(terms) <- FALSE
    storage.mode(terms) <- "logical"
    storage.mode(inclMat) <- "logical"
    rownames(terms) <- colnames(terms)
    effectNames <- colnames(terms)
    
    tmp <- .BANOVAcomputMatchedInclusion(
      effectNames, inclMat, terms, priorModelProbs, postModelProbs
    )
    # 1 for the intercept
    priorInclProbs <- c(1, tmp[["priorInclProb"]])
    postInclProbs  <- c(1 ,tmp[["postInclProb"]])
    BFincl         <- c(1, tmp[["bfIncl"]])

  }

  # show BFinclusion for nuisance predictors as 1, rather than NaN
  priorInclIs1 <- is.nan(BFincl) & abs(1 - priorInclProbs) <= sqrt(.Machine[["double.eps"]])
  BFincl[priorInclIs1] <- 1

  summaryTable[["coefficient"]] <- results[["model"]][["newNamesx"]]
  summaryTable[["pInclPrior"]]  <- priorInclProbs
  summaryTable[["pInclPost"]]   <- postInclProbs
  summaryTable[["mean"]]        <- summaryObj[["coefBMA"]][["postmean"]]
  summaryTable[["sd"]]          <- summaryObj[["coefBMA"]][["postsd"]]
  summaryTable[["lowerCri"]]    <- summaryObj[["conf95"]][, 1L]
  summaryTable[["upperCri"]]    <- summaryObj[["conf95"]][, 2L]
  summaryTable[["BFincl"]]      <- .recodeBFtype(BFincl, oldBFtype = "BF10", newBFtype = options[["bayesFactorType"]])

  if (any(is.infinite(BFincl)))
    summaryTable$addFootnote(paste(
      "Infinite inclusion Bayes factors are a sign that the MCMC algorithm may need to run longer.",
      "Adjust the options under \"Advanced Options\" to run the algorithm longer."
    ))
  
  return()
}

.regLogBaySummaryTableMarkup <- function(options) {
  
  table <- createJaspTable(title = "Posterior Summaries of Coefficients")
  
  inclusionBfTitle <- switch(
    options[["bayesFactorType"]],
    "LogBF10" = "Log(BF<sub>incl</sub>)",
    "BF01"    = "BF<sub>excl</sub>",
    "BF10"    = "BF<sub>incl</sub>"
  )
  overtitle <- sprintf("%s%% Credible Interval", format(100*options[["posteriorSummaryPlotCredibleIntervalValue"]], digits = 3))
  
  table$position <- 1
  table$addColumnInfo(name = "coefficient", title = "Coefficient",    type = "string")
  table$addColumnInfo(name = "mean",        title = "Mean",           type = "number")
  table$addColumnInfo(name = "sd",          title = "SD",             type = "number")
  table$addColumnInfo(name = "pInclPrior",  title = "P(incl)",        type = "number")
  table$addColumnInfo(name = "pInclPost",   title = "P(incl|data)",   type = "number")
  table$addColumnInfo(name = "BFincl",      title = inclusionBfTitle, type = "number")
  table$addColumnInfo(name = "lowerCri",    title = "Lower",          type = "number", overtitle = overtitle)
  table$addColumnInfo(name = "upperCri",    title = "Upper",          type = "number", overtitle = overtitle)
  return(table)
  
}

.regLogBaySummaryPlot <- function(jaspResults, options, results, summaryObj) {
  
  if (!options[["postSummaryPlot"]])
    return()
  
  summaryContainer <- jaspResults[["modelContainer"]][["summaryContainer"]]
  title <- sprintf("Posterior Coefficients with %s%% Credible Interval", 
                   format(100*options[["posteriorSummaryPlotCredibleIntervalValue"]], digits = 3))

  plot <- createJaspPlot(title = title, position = 2)
  summaryContainer[["posteriorPlots"]] <- plot

  if (!results[["ready"]] || summaryContainer$getError())  
    return()

  df <- data.frame(
    mean   = summaryObj[["conf95"]][, 3L], 
    ciHigh = summaryObj[["conf95"]][, 1L], 
    ciLow  = summaryObj[["conf95"]][, 2L], 
    group  = results[["model"]][["newNamesx"]]
  )
  # add newlines before and after the interaction symbol to avoid the x-axis labels from overlapping
  df[["group"]] <- gsub("\u2009\u273B\u2009", "\n\u2009\u273B\u2009\n", df[["group"]])

  if (options[["omitIntercept"]])
    df <- df[-1L, ]

  yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(df$ciHigh, df$ciLow))
  graph <- ggplot2::ggplot(data = df, ggplot2::aes(x = group, y = mean, ymin = ciLow, ymax = ciHigh, group = group)) + 
    ggplot2::geom_point() + 
    ggplot2::geom_errorbar(width = 0.2) + 
    ggplot2::scale_y_continuous(breaks = yBreaks, limits = range(yBreaks)) +
    ggplot2::labs(x = NULL, y = expression(beta)) + 
    ggplot2::theme(axis.title.y = ggplot2::element_text(angle = 0, vjust = 0.5))
  graph <- JASPgraphs::themeJasp(graph)
  plot$plotObject <- graph
  return()

}

# helper functions ----
.regLogBayIsReady <- function(options) {
  return(list(ready = all(
    options[["dependent"]] != "",
    length(options[["covariates"]]) > 0L || length(options[["factors"]]) > 0L,
    length(options[["modelTerms"]]) > 0L
  )))
}

.regLogBayRenameNamesx <- function(namesx) {
  interactionSymbol <- "\u2009\u273B\u2009"
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
  return(c("Intercept", unlist(namesxSplit)))
}

# citations ----
.regLogBayCitations <- c(
  "Clyde2017"     = "Clyde, M. A. (2017). BAS: Bayesian Adaptive Sampling for Bayesian Model Averaging. (Version 1.5.3)[Computer software].", 
  "ClydeEtal2011" = "Clyde, M. A., Ghosh, J., & Littman, M. L. (2011). Bayesian adaptive sampling for variable selection and model averaging. Journal of Computational and Graphical Statistics, 20, 80-101."
)
