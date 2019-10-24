#Some functions that act as a bridge between R and JASP. If JASP isn't running then all columnNames are expected to not be encoded

encodeColumnName <- function(columnNames)
{
  if (is.null(columnNames) || !exists('.encodeColumnName'))
    return(columnNames)

  encoded <- c()
  for (columnName in columnNames)
    if(is.character(columnName))  encoded[length(encoded)+1] <- .encodeColumnName(columnName)
    else                          encoded[length(encoded)+1] <- columnName

  return(encoded)
}

decodeColumnName <- function(columnName)
{
  if (is.null(columnNames) || !exists('.decodeColumnName'))
    return(columnNames)

  decoded <- c()
  for(columnName in columnNames)
    if(is.character(columnName))  decoded[length(decoded)+1] <- .decodeColumnName(columnName)
    else                          decoded[length(decoded)+1] <- columnName

  return(decoded)
}

encodeAllColummnNames <- function(texts)
{
  if (is.null(texts) || !exists('.encodeAllColumnNames'))
    return(texts)

  encoded <- c()
  for(text in texts)
    if(is.character(text))  encoded[length(encoded)+1] <- .encodeAllColumnNames(text)
    else                    encoded[length(encoded)+1] <- text

  return(encoded)
}

decodeAllColumnNames <- function(texts)
{

  if (is.null(texts) || !exists('.decodeAllColumnNames'))
    return(texts)

  decoded <- c()
  for(text in texts)
  if(is.character(text))  decoded[length(decoded)+1] <- .decodeAllColumnNames(text)
  else                    decoded[length(decoded)+1] <- text

  return(decoded)
}

# decodeSVGfile <- function(path) {
# 
#   con <- file(path)
#   on.exit(close(con))
#   string <- readLines(con)
#   string2 <- decodeAllColumnNames(string)
#   saveRDS(string2, file = "~/jaspDeleteable/string2.RDS")
#   writeLines(string2, con)
# 
#   return(invisible(TRUE))
#   
# }

decodeplot <- function(x, ...) {
  UseMethod("decodeplot", x)
}

decodeplot.JASPgraphsPlot <- function(x) {
  for (i in seq_along(x$subplots))
    x$subplots[[i]] <- decodeplot(x$subplots[[i]], returnGrob = FALSE)
  return(x)
}

decodeplot.gg <- function(x, returnGrob = TRUE) {
  save(x, file = "~/jaspDeleteable/decodeplot.gg")
  labels <- x[["labels"]]
  for (i in seq_along(labels))
    if (!is.null(labels[[i]]))
      labels[[i]] <- decodeAllColumnNames(labels[[i]])
  x[["labels"]] <- labels
  if (returnGrob)
    return(decodeplot.gTree(ggplot2::ggplotGrob(x)))
  else
    return(x)
}

decodeplot.recordedplot <- function(x) {
  decodeplot.gTree(grid::grid.grabExpr(gridGraphics::grid.echo(x)))
}

decodeplot.gtable <- function(x) rapply(x, f = decodeAllColumnNames, classes = "character", how = "replace")
decodeplot.grob   <- function(x) rapply(x, f = decodeAllColumnNames, classes = "character", how = "replace")
decodeplot.gTree  <- function(x) rapply(x, f = decodeAllColumnNames, classes = "character", how = "replace")
decodeplot.gDesc  <- function(x) rapply(x, f = decodeAllColumnNames, classes = "character", how = "replace")

decodeplot.qgraph <- function(x) {
  stop("qgraph is not implemented yet!")
}

decodeSVGfile <- function(path, width, height, ppi) {
  
  # file.copy(path, "~/jaspDeleteable/svgtest.svg", overwrite = TRUE)
  # save(width, height, ppi, file = "~/jaspDeleteable/svgtest.Rdata")

  svgPlot <- xml2::read_xml(path)
  
  # all encountered text elements where always subelemnts of g
  children <- xml2::xml_children(svgPlot)
  idx <- xml2::xml_name(children) == "g"
  text <- xml2::xml_children(children[idx])
  
  # rename potential column names
  old <- xml2::xml_text(text)
  new <- old
  new[9:11] <- c("conta", "contbb", "contccc")
  # new[13] <- "contNormal"
  # new[14] <- "contGamma"
  # new <- decodeAllColumnNames(old)
  diff <- which(old != new)
  
  # set the column names
  xml2::xml_text(text)[diff] <- new[diff]

  # convert a string of pixels to numeric  
  px2numeric <- function(x) as.numeric(substr(x, 1L, nchar(x) - 2L))
  
  # open dummy svg file with dimensions identical to path to recompute size of text.
  f <- tempfile()
  svglite::svglite(file = f, width = width, height = height)
  graphics::plot.new()
  on.exit({
    grDevices::dev.off()
    if (file.exists(f)) file.remove(f)
  })

  # for each textual change we do some rescaling
  for (i in diff) {
    attr <- xml2::xml_attrs(text[i])[[1L]]
    
    oldTextLength <- px2numeric(attr["textLength"])
    style <- strsplit(attr["style"], ":|;")[[1L]]
    font   <- px2numeric(style[2L])
    family <- trimws(style[4L])
    # newTextLength <- strwidth(new[i], font = font, family = family) * 2 * ppi
    oo <- strwidth(old[i], font = font, family = family) * ppi
    r <- oldTextLength / oo
    newTextLength <- strwidth(new[i], font = font, family = family) * ppi * r

    if ("transform" %in% names(attr)) {
      
      # TODO: this assumes a rotation of -90. Instead, other rotations should be distributed over both x and y.
      xy <- strsplit(attr["transform"], "translate\\(|,|\\)")[[1]]
      xy2 <- as.numeric(xy[xy != ""][1:2])
      center <- xy2[2] -  0.5 * oldTextLength
      xy2[2] <- center + 0.5 * newTextLength
      attr["transform"] <- paste0("translate(", xy2[1L], ',', xy2[2L], ")", xy[length(xy)], ")")
      
    } else {

      xpos <- as.numeric(attr["x"])
      center <- xpos + 0.5 * oldTextLength
      newxpos <- center - 0.5 * newTextLength
      attr["x"] <- newxpos

    }
    attr["textLength"] <- newTextLength
    xml2::xml_attrs(text[i]) <- attr
  }
  
  # save to file
  # xml2::write_xml(svgPlot, file = "~/jaspDeleteable/svgtest_after.svg")
  xml2::write_xml(svgPlot, file = path)
  
}

# path <- "~/jaspDeleteable/svgtest.svg"
# load("~/jaspDeleteable/svgtest.Rdata")
# debugonce(decodeSVGfile)
# decodeSVGfile("~/jaspDeleteable/svgtest.svg", width, height, ppi)
