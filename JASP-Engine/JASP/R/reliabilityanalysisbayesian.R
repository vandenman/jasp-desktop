#
# Copyright (C) 2018 University of Amsterdam
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

ReliabilityAnalysisBayesian <- function(jaspResults, dataset, options, state = NULL) {
	
	# init analysis ----
	# do this once instead of in every subfunction
	options[["selectedEstimators"]] <- unlist(options[c("mcDonaldScale", "alphaScale", "gutmann2Scale", "gutmann6Scale", "glbScale")])
	options[["namesEstimators"]] <- list(
		tables = c("McDonald's \u03C9", "Cronbach's \u03B1", "Gutmann's \u03BB2", "Gutmann's \u03BB6", "Greatest Lower Bound"),
		plots = list(expression("McDonald's"~omega), expression("Cronbach\'s"~alpha), expression("Guttman's"~lambda[2]), expression("Guttman's"~lambda[6]),
								 "Greatest Lower Bound")
	)
	
	variables <- unlist(options[["variables"]])
	if (is.null(dataset)) {
		
		dataset <- .readDataSetToEnd(columns.as.numeric=variables, columns.as.factor=NULL, exclude.na.listwise=NULL)
		
	} else {
		
		dataset <- .vdf(dataset, columns.as.numeric=variables, columns.as.factor=NULL)
		
	}
	
	
	## Retrieve State
	resultsObj <- state[["resultsObj"]]
	
	# init results
	jaspResults$title <- "Bayesian Reliability Analysis"
	
	scaleTable <- getJaspTable("Scale Reliability Statistics")
	plots <- getJaspContainer("Posteriors Plots")
	jaspResults[["Scale"]] <- scaleTable
	jaspResults[["Posterior Plots"]] <- plots
	
	# execute analysis ----
	# should any sampling be done?
	if (!is.null(variables) && length(variables) >= 2 && is.null(resultsObj)) {
		
		# check for errors
		.hasErrors(dataset = dataset, perform = "run",
							 type = c("infinity", "variance", "observations"),
							 observations.amount = " < 3",
							 exitAnalysisIfErrors = TRUE)
		
		# manage user expectations
		.reliabalityBayesianScaleTable(scaleTable, NULL, options, "running")
		jaspResults$send()
		
		# do actual work
		resultsObj <- .reliabilityBayesianResults(dataset, options, variables)
		
	}
	
	# create output ----
	.reliabalityBayesianScaleTable(scaleTable, resultsObj, options)
	jaspResults$send()
	.reliabalityBayesianPosteriorPlot(jaspResults, resultsObj, options)
	
	
	# exit analysis ----
	
	# make statekey & state
	state <- list(
		options = options,
		resultsObj = resultsObj
	)
	
	defaults <- c("variables", "reverseScaledItems", "missingValues")
	attr(state, "key") <- list(
		resultsObj = defaults
	)
	
	return(state)
	
}

.reliabilityBayesianResults <- function(dataset, options, variables) {
	
	dataset <- as.matrix(dataset)
	if (length(options[["reverseScaledItems"]]) > 0) {
		nvar <- length(variables)
		key <- rep(1, nvar)
		key[match(.v(unlist(options[["reverseScaledItems"]])), nvar)] <- -1
		dataset <- dataset %*% diag(key, nvar, nvar)
	}
	resultsObj <- list(
		relyFit = bayesrel::brel(dataset, freq = FALSE, n.iter = 5e2),#options[["nIter"]]),
		footnote = .reliabilityCheckLoadings(dataset, variables)
	)
	
	return(resultsObj)
	
}

.reliabilityCheckLoadings <- function(dataset, variables) {
	
	footnote <- NULL
	prin <- psych::principal(dataset)
	idx <- prin[["loadings"]] > 0
	sidx <- sum(idx)
	if (sidx == 1) {
		footnote <- sprintf("The following item correlated negatively with the scale: %s", variables[idx])
	} else if (sidx > 1) {
		footnote <- sprintf("The following items correlated negatively with the scale: %s", paste0(variables[idx], collapse = ", "))
	}
	return(footnote)
}

.reliabalityBayesianScaleTable <- function(scaleTable, resultsObj, options, status = "complete") {
	
	# print(sprintf("status %s", status))
	# if (is.null(scaleTable[["statistic"]])) {
	# 	print("here0")
	# 	scaleTable$addColumnInfo(name = "statistic", title = "Statistic",      type="string")
	# 	scaleTable$addColumnInfo(name = "postMean",  title = "Posterior Mean", type="number", format="sf:4")
	# 	scaleTable$addColumnInfo(name = "lower",     title = "Lower",          type="number", format="sf:4")
	# 	scaleTable$addColumnInfo(name = "upper",     title = "Upper",          type="number", format="sf:4")
	# }
	relyFit <- resultsObj[["relyFit"]]
	
	opts <- options[["namesEstimators"]][["tables"]]
	if (!is.null(relyFit)) {
		allData <- data.frame(
			statistic = opts,
			postMean  = unname(unlist(relyFit$bay$est)),
			lower     = unname(unlist(relyFit$bay$cred$low)),
			upper     = unname(unlist(relyFit$bay$cred$up)),
			stringsAsFactors = FALSE
		) 
	} else {
		allData <- data.frame(
			statistic = opts,
			postMean  = ".",
			lower     = ".",
			upper     = ".", 
			stringsAsFactors = FALSE
		)    
	}
	# allData <- allData[options[["selectedEstimators"]], ]
	# cnms <- colnames(allData)
	# for (nm in cnms)
	# 	scaleTable[[nm]] <- allData[[nm]]
	scaleTable$setData(allData[options[["selectedEstimators"]], ])
	if (!is.null(resultsObj[["footnotes"]]))
		scaleTable$addFootnote(resultsObj[["footnotes"]])
	scaleTable$setColFormats(c("", rep("sf:4", 3)))
	scaleTable$status <- status
	return(NULL)
}

.reliabalityBayesianPosteriorPlot <- function(plots, resultsObj, options) {
	
	if (!options[["plotPosterior"]])
		return()
	
	relyFit <- resultsObj[["relyFit"]]
	indices <- which(options[["selectedEstimators"]])
	nmsLabs <- options[["namesEstimators"]][["plots"]]
	nmsObjs <- options[["namesEstimators"]][["tables"]]
	for (i in indices) {
		
		p <- .reliabalityBayesianMakeSinglePosteriorPlot(relyFit, i, nmsLabs[[i]])
		plots[[nmsObjs[i]]] <- getJaspPlot(plot = p, title = nmsObjs[i], width = 480, height = 320)
		
	}
	
	return(NULL)
}

.reliabalityBayesianMakeSinglePosteriorPlot <- function(relyFit, i, nms) {
	
	d <- stats::density(relyFit$bay$samp[[i]], from = 0, to = 1, n = 2^11)
	datDens <- data.frame(x = d$x, y = d$y)
	# max height posterior is at 90% of plot area; remained is for credible interval
	ymax <- max(d$y) / .9 
	yBreaks <- JASPgraphs::getPrettyAxisBreaks(c(0, ymax))
	ymax <- max(yBreaks)
	datCri <- data.frame(xmin = relyFit$bay$cred$low[[i]], xmax = relyFit$bay$cred$up[[i]], y = .925 * ymax)
	datCri$x <- (datCri$xmin + datCri$xmax) / 2
	datTxt <- data.frame(x = c(datCri$xmin, datCri$xmax), 
											 y = 0.985 * ymax, 
											 label = format(c(datCri$xmin, datCri$xmax), digits = 3, scientific = -2))
	g <- ggplot2::ggplot(data = datDens, mapping = ggplot2::aes(x = x, y = y)) +
		ggplot2::geom_line() + 
		ggplot2::geom_errorbarh(data = datCri, mapping = ggplot2::aes(xmin = xmin, xmax = xmax, y = y, x = x), inherit.aes = FALSE) + 
		ggplot2::geom_text(data = datTxt, mapping = ggplot2::aes(x = x, y = y, label = label), inherit.aes = FALSE) + 
		ggplot2::scale_y_continuous(name = "Density", breaks = yBreaks, limits = range(yBreaks)) + 
		ggplot2::scale_x_continuous(name = nms)
	# g
	return(JASPgraphs::themeJasp(g))
	
}
