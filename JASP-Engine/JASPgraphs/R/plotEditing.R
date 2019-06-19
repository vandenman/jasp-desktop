fromJSON  <- function(x) jsonlite::fromJSON(x, TRUE, FALSE, FALSE)
toJSON    <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null")

#' @importFrom ggplot2 layer_scales is.ggplot

# possible optimization!
# layer_scales <- function(plot, i = 1L, j = 1L) {
#   if (is.ggplot(plot)) {
#     print("Inefficient!")
#     return(ggplot2::layer_scales(plot, i, j))
#   }
#   # definition from ggplot2::layer_scales but without ggplot_build
#   layout <- plot$layout$layout
#   selected <- layout[layout$ROW == i & layout$COL == j, , drop = FALSE]
#   return(list(x = plot$layout$panel_scales_x[[selected$SCALE_X]], y = b$layout$panel_scales_y[[selected$SCALE_Y]]))
# }

# all axis types in ggplot
AxisTypes <- c(
  "ScaleContinuous",
  "ScaleContinuousDate",
  "ScaleContinuousDatetime",
  "ScaleDiscrete"
)

getAxisType <- function(x, asCharacter = TRUE) {
  UseMethod("getAxisType", x)
}

getAxisType.list <- function(x, asCharacter = TRUE) {

  scaleX <- match(class(x[[1L]]), AxisTypes)
  scaleY <- match(class(x[[2L]]), AxisTypes)
  scaleX <- scaleX[!is.na(scaleX)]
  scaleY <- scaleY[!is.na(scaleY)]
  if (asCharacter) {
    scaleX <- AxisTypes[scaleX]
    scaleY <- AxisTypes[scaleY]
  }
  return(c("x" = scaleX, "y" = scaleY))
}

getAxisType.gg <- function(x, asCharacter = TRUE) {
  return(getAxisType.list(layer_scales(g, i = 1L, j = 1L), asCharacter))
}

evenly_spaced <- function(x) {
  by <- x[2L] - x[1L]
  return(all((x[-length(x)] - x[-1L] - by) <= .Machine[["double.eps"]]))
}

getAxisInfo <- function(x) {
  UseMethod("getAxisInfo", x)
}

getAxisInfo.ScaleContinuousPosition <- function(x) {

  info <- x[["break_info"]]()
  return(list(
    limits   = info[["range"]],
    labels   = info[["labels"]],
    breaks   = info[["major_source"]]
  ))
}

getAxisInfo.ScaleDiscretePosition <- function(x) {

  return(list(
    labels = x[["get_labels"]](),
    shown  = x[["get_limits"]]()
  ))

}

internalUpdateAxis <- function(currentAxis, newSettings) {
  UseMethod("internalUpdateAxis", currentAxis)
}

getPanelInfo <- function(x) {

  opts <- x$layout$coord$labels(x$layout$panel_params)[[1L]]
  idx1 <- startsWith(names(opts), "x")
  nms <- substring(names(opts[idx]), 3)
  nms2keep <- c("range", "labels", "major_source")
  idx2 <- nms %in% nms2keep

  out <- list(
    x = list(
      type     = "c",
      settings = opts[idx1 & idx2]
    ),
    y = list(
      type     = "c",
      settings = opts[!idx1 & idx2]
    )
  )
  names(out[["x"]][["settings"]]) <- names(out[["y"]][["settings"]]) <- nms2keep

  return(out)
}

internalUpdateAxis.ScaleContinuousPosition <- function(currentAxis, newSettings) {

  # newSettings only contains not modified settings!
  if (!is.null(newSettings[["limits"]]))
    currentAxis[["limits"]] <- newSettings[["limits"]]

  if (!is.null(newSettings[["labels"]]))
    currentAxis[["labels"]] <- c(newSettings[["labels"]])

  # if (!is.null(newSettings[["fromByTo"]]) || !is.null(newSettings[["breaks"]])) {
  if (!is.null(newSettings[["breaks"]])) {
    # if (!is.null(newSettings[["breaks"]])) {
      breaks <- sort(newSettings[["breaks"]])
    # } else {
      # fromToBy <- newSettings[["fromToBy"]]
      # breaks <- seq(fromToBy[1L], fromToBy[2L], fromToBy[3L])
    # }
    currentAxis[["breaks"]] <- breaks
  } else if (!is.null(newSettings[["labels"]]) && is.waive(currentAxis[["breaks"]])) {
    breaks <- currentAxis[["get_breaks"]]()
    currentAxis[["breaks"]] <- breaks[!is.na(breaks)]
  }
  return(currentAxis)
}

internalUpdateAxis.ScaleDiscretePosition <- function(currentAxis, newSettings) {

  # newSettings only contains not modified settings!
  if (!is.null(newSettings[["limits"]]))
    currentAxis[["limits"]] <- newSettings[["limits"]]

  if (!is.null(newSettings[["labels"]]))
    currentAxis[["labels"]] <- newSettings[["labels"]]

  return(currentAxis)
}

# preProcessOptions <-

# updateAxis <- function(g, xory = c("x", "y"), from, to, by, labels = NULL, setLimits = TRUE) {
#
#   # error handling for (to - from) / by should happen elsewhere!
#   xory <- match.arg(xory)
#   currentAxis <- layer_scales(g, i = 1L, j = 1L)[[xory]]
#   return(internalUpdateAxis(currentAxis, g, from, to, by, labels, setLimits))
#
# }

validateOptions <- function(newOptions, oldOptions) {

  if (is.character(newOptions)) {
    newOptions <- fromJSON(newOptions)
  } else if (!is.list(newOptions)) {
    stop("options should be an R list or a json string!")
  }

  if (newOptions[["xAxis"]][["type"]] != oldOptions[["xAxis"]][["type"]] ||
      newOptions[["yAxis"]][["type"]] != oldOptions[["yAxis"]][["type"]]) {
    stop("The axis type in the new options list does not match the graph!")
  }

  return(newOptions)
}

# unifyBreaksAndFromToBy(options, breaksAreNew) {
#   # unify breaks and fromToBy? make newOptions global?
#   if (options[["xAxis"]][["type"]] == "ScaleContinuous") {
#     if (options[["xAxis"]][["breaks"]] == oldOptions[["xAxis"]][["breaks"]]) {
#
#     }
#   }
#   if (options[["xAxis"]][["type"]] == "ScaleYContinuous") {
#     if (options[["xAxis"]][["breaks"]] == oldOptions[["xAxis"]][["breaks"]]) {
#
#     }
#   }
# }

#' @export
plotEditingOptions <- function(graph, currentAxes = NULL, asJSON = FALSE) {

  if (is.null(currentAxes))
    currentAxes <- layer_scales(graph)
  axisTypes <- getAxisType(currentAxes)
  optsForEditor <- list(
    xAxis = list(type = axisTypes[["x"]], settings = getAxisInfo(currentAxes[["x"]])),
    yAxis = list(type = axisTypes[["y"]], settings = getAxisInfo(currentAxes[["y"]])),
    geoms = NULL
  )

  if (asJSON)
    optsForEditor <- toJSON(optsForEditor)
  return(optsForEditor)

}

optionsDiff <- function(lst1, lst2) {

  lst1[["xAxis"]][["settings"]] <- lst1[["xAxis"]][["settings"]][
    !unlist(mapply(identical, lst1[["xAxis"]][["settings"]], lst2[["xAxis"]][["settings"]],
                   SIMPLIFY = FALSE, USE.NAMES = FALSE))
  ]

  lst1[["yAxis"]][["settings"]] <- lst1[["yAxis"]][["settings"]][
    !unlist(mapply(identical, lst1[["yAxis"]][["settings"]], lst2[["yAxis"]][["settings"]],
                   SIMPLIFY = FALSE, USE.NAMES = FALSE))
  ]

  return(lst1)
}

#' @export
plotEditing <- function(graph, newOptions) {

  if (!is.ggplot(graph))
    stop("graph should be a ggplot2")

  # ggbuild     <- ggplot_build(graph)
  # axisInfo    <- ggbuild$
  currentAxis <- layer_scales(graph)
  oldOptions  <- plotEditingOptions(graph, currentAxis)
  newOptions  <- validateOptions(newOptions, oldOptions)
  diffOptions <- optionsDiff(newOptions, oldOptions)

  if (length(diffOptions[["xAxis"]][["settings"]]) > 0L)
    graph <- graph + internalUpdateAxis(currentAxis[["x"]], diffOptions[["xAxis"]][["settings"]])

  if (length(diffOptions[["yAxis"]][["settings"]]) > 0L)
    graph <- graph + internalUpdateAxis(currentAxis[["y"]], diffOptions[["yAxis"]][["settings"]])

  return(graph)

}
