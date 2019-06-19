fromJSON  <- function(x) jsonlite::fromJSON(x, TRUE, FALSE, FALSE)
toJSON    <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null")

#' @importFrom ggplot2 layer_scales is.ggplot ggplot_build

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

getAxisType <- function(x) {
  UseMethod("getAxisType", x)
}

getAxisType.list <- function(x) {
  # for output of layer_scales(plot)
  scaleX <- match(class(x[[1L]]), AxisTypes)
  scaleY <- match(class(x[[2L]]), AxisTypes)
  scaleX <- scaleX[!is.na(scaleX)]
  scaleY <- scaleY[!is.na(scaleY)]
  scaleX <- AxisTypes[scaleX]
  scaleY <- AxisTypes[scaleY]
  return(c("x" = scaleX, "y" = scaleY))
}


getAxisType.ggplot_built <- function(x) {
  return(c(
    "x" = class(x[["layout"]][["panel_scales_x"]][[1L]])[[2L]],
    "y" = class(x[["layout"]][["panel_scales_y"]][[1L]])[[2L]]
  ))
}

getAxisType.ggplot <- function(x) {
  return(getAxisType.list(layer_scales(x, i = 1L, j = 1L)))
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

#' @export
plotEditingOptions <- function(graph, asJSON = FALSE) {
  UseMethod("plotEditingOptions", graph)
}

plotEditingOptions.gg <- function(graph, asJSON = FALSE) {
  return(plotEditingOptions.ggplot(graph, asJSON))
}

plotEditingOptions.ggplot <- function(graph, asJSON = FALSE) {

  ggbuild <- ggplot_build(graph)
  return(plotEditingOptions.ggplot_built(ggbuild, asJSON))

}

plotEditingOptions.ggplot_built <- function(ggbuild, asJSON) {

  opts <- ggbuild$layout$coord$labels(ggbuild$layout$panel_params)[[1L]]
  idx1 <- startsWith(names(opts), "x")
  nms <- substring(names(opts[idx1]), 3L)
  nms2keep <- c("range",  "labels", "major_source")
  nms2give <- c("limits", "labels", "breaks")
  idx2 <- nms %in% nms2keep

  axisTypes <- getAxisType(ggbuild)

  out <- list(
    xAxis = list(
      type     = axisTypes[["x"]],
      settings = opts[idx1 & idx2]
    ),
    yAxis = list(
      type     = axisTypes[["y"]],
      settings = opts[!idx1 & idx2]
    )
  )
  names(out[["xAxis"]][["settings"]]) <- names(out[["yAxis"]][["settings"]]) <- nms2give

  if (asJSON)
    out <- toJSON(out)

  return(out)
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

  ggbuild     <- ggplot_build(graph)
  oldOptions  <- plotEditingOptions(ggbuild)
  newOptions  <- validateOptions(newOptions, oldOptions)
  diffOptions <- optionsDiff(newOptions, oldOptions)

  currentAxis <- layer_scales(graph) # <- could be ggbuild

  if (length(diffOptions[["xAxis"]][["settings"]]) > 0L)
    graph <- graph + internalUpdateAxis(currentAxis[["x"]], diffOptions[["xAxis"]][["settings"]])

  if (length(diffOptions[["yAxis"]][["settings"]]) > 0L)
    graph <- graph + internalUpdateAxis(currentAxis[["y"]], diffOptions[["yAxis"]][["settings"]])

  return(graph)

}
