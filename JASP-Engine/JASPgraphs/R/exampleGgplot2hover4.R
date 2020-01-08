#rm(list = ls())
library(ggplot2)
library(grid)
library(gtable)

is.zeroGrob <- function(x) inherits(x, "zeroGrob")

# functions for alternative solution
isUnitNull <- function(x) endsWith(as.character(x), "null")
getUnitValue <- function(x) sapply(x, `[[`, 1L)

computeUnit <- function(u, all, type = c("width", "height")) {
  type <- match.arg(type)
  if (isUnitNull(u)) {
    
    notNull <- !isUnitNull(all)# != "null"
    unew <- unit(1, "npc") - sum(all[notNull])
    if (sum(!notNull) > 1L) {
      valU <- getUnitValue(u)
      valAll <- getUnitValue(all[!notNull])
      prop <- valU / sum(valAll)
      unew <- prop * unew
    }
  } else {
    unew <- u
  }
  
  if (type == "width") {
    ans <- grid::convertWidth(unew, "npc")
  } else {
    ans <- grid::convertHeight(unew, "npc")
  }
  return(ans)
}

convertObj <- function(obj, target) {
  return(list(
    name   = obj$name,
    #x      = grid::convertX(obj$x,           target), 
    #y      = grid::convertY(obj$y,           target), 
    #width  = grid::convertWidth(obj$width,   target), 
    #height = grid::convertHeight(obj$height, target),
    x0     = as.double(grid::convertX(obj$x0,          target)), 
    x1     = as.double(grid::convertX(obj$x1,          target)), 
    y0     = as.double(grid::convertY(obj$y0,          target)), 
    y1     = as.double(grid::convertY(obj$y1,          target))
  ))
}

getCornersInPixels <- function(obj, pngWidth, pngHeight) {
  getUnitValue(obj[-(1:4)]) * c(pngWidth, pngWidth, pngHeight, pngHeight)
}

grid.show.layout.modified <- function(l, table, newpage = TRUE, vp.ex = 0.8, bg = "light grey", 
                                      cell.border = "blue", cell.fill = "light blue", cell.label = TRUE, 
                                      label.col = "blue", unit.col = "red", vp = NULL, targetUnit = "native", 
                                      drawNew = TRUE, ...) {
  if (!grid:::is.layout(l)) 
    stop("'l' must be a layout")
  if (newpage) 
    grid.newpage()
  if (!is.null(vp)) 
    grid::pushViewport(vp)
  grid.rect(gp = gpar(col = NULL, fill = bg))
  vp.mid <- grid::viewport(0.5, 0.5, vp.ex, vp.ex, layout = l)
  grid::pushViewport(vp.mid)
  grid.rect(gp = gpar(fill = "white"))
  gp.red <- gpar(col = unit.col)
  objs <- matrix(list(), l$nrow, l$ncol)
  oldWW <- NULL
  oldHH <- NULL
  totalHeight <- unit(1, "npc")
  prevI <- 1
  for (i in 1L:l$nrow) for (j in 1L:l$ncol) {
    
    
    vp.inner <- grid::viewport(layout.pos.row = i, layout.pos.col = j)
    grid::pushViewport(vp.inner)
    grid.rect(gp = gpar(col = cell.border, fill = cell.fill))
    
    if(length(table[i,j])>0)
    {
      if(!(is.zeroGrob(table[i,j]$grobs[[1]])))
      {
        grid.rect(gp = gpar(col = cell.border, fill = cell.fill))
        if (cell.label) 
          grid.text(paste0("(", i, ", ", j, ")"), gp = gpar(col = label.col))
        if (j == 1) 
          grid.text(format(l$heights[i, top = FALSE], ...), 
                    gp = gp.red, just = c("right", "centre"), x = unit(-0.05, 
                                                                       "inches"), y = unit(0.5, "npc"), rot = 0)
        if (i == l$nrow) 
          grid.text(format(l$widths[j, top = FALSE], ...), 
                    gp = gp.red, just = c("centre", "top"), x = unit(0.5, 
                                                                     "npc"), y = unit(-0.05, "inches"), rot = 0)
        if (j == l$ncol) 
          grid.text(format(l$heights[i, top = FALSE], ...), 
                    gp = gp.red, just = c("left", "centre"), x = unit(1, 
                                                                      "npc") + unit(0.05, "inches"), y = unit(0.5, 
                                                                                                              "npc"), rot = 0)
        if (i == 1) 
          grid.text(format(l$widths[j, top = FALSE], ...), 
                    gp = gp.red, just = c("centre", "bottom"), x = unit(0.5, 
                                                                        "npc"), y = unit(1, "npc") + unit(0.05, "inches"), 
                    rot = 0)
      }
    }
    popViewport()
    
    hh <- computeUnit(l$height[i, top = FALSE], l$height, "height")
    ww <- computeUnit(l$width[j, top = FALSE], l$width, "width")
    if (j == 1L)
      totalWidth <- unit(0, "npc")
    if (i != prevI)
      totalHeight <- totalHeight - oldHH[length(oldHH)]
    
    x <- totalWidth + 0.5 * ww
    y <- totalHeight - 0.5 * hh
    x0 <- x - 0.5 * ww
    x1 <- x + 0.5 * ww
    y0 <- y - 0.5 * hh
    y1 <- y + 0.5 * hh
    if (drawNew) {
      if(length(table[i,j])>0)
      {
          if(!(is.zeroGrob(gt[i,j]$grobs[[1]])))
          {
            grid.points(x, y, gp = gpar(cex = .75, fill = scales::alpha("orange", .5), col = "orange"))
            grid.points(x = unit.c(x0, x0, x1, x1), y = unit.c(y0, y1, y0, y1), gp = gpar(cex = .75, fill = scales::alpha("purple", .5), col = "purple"))
            grid.rect(x = x,
                      y = y,
                      width = ww, height = hh,
                      gp = gpar(col = "green", fill = "transparent")
            )
          }
      }
    }
    totalWidth  <- totalWidth + ww
    oldWW <- if (length(oldWW) == 0L) ww else grid::unit.c(oldWW, ww)
    oldHH <- if (length(oldHH) == 0L) hh else grid::unit.c(oldHH, hh)
    prevI <- i
    obj <- list(#x = x, y = y, width = ww, height = hh,
                x0 = x0, x1 = x1, y0 = y0, y1 = y1)
    if(length(table[i,j])>0)
    {
      if(!(is.zeroGrob(gt[i,j]$grobs[[1]])))
      {
        objs[[i, j]] <- convertObj(obj, targetUnit)
      }
    }
  }
  grid::popViewport()
  if (!is.null(vp)) 
    grid::popViewport()
  return(objs)
}

grid.show.layout.modified.2 <- function(l, table, targetUnit = "native", debug = FALSE, newpage = FALSE, ...) {
  if (!grid:::is.layout(l)) 
    stop("'l' must be a layout")
  if (newpage) 
    grid.newpage()
  vp.mid <- grid::viewport(0.5, 0.5, layout = l)
  #grid::pushViewport(vp.mid)
  objs <- matrix(list(), l$nrow, l$ncol)
  
  oldWW <- NULL
  oldHH <- NULL
  totalHeight <- unit(1, "npc")
  prevI <- 1
  for (i in 1L:l$nrow) for (j in 1L:l$ncol) {
    
    
    
    hh <- computeUnit(l$height[i, top = FALSE], l$height, "height")
    ww <- computeUnit(l$width[j, top = FALSE], l$width, "width")
    if (j == 1L)
      totalWidth <- unit(0, "npc")
    if (i != prevI)
      totalHeight <- totalHeight - oldHH[length(oldHH)]
    
    x <- totalWidth + 0.5 * ww
    y <- totalHeight - 0.5 * hh
    x0 <- x - 0.5 * ww
    x1 <- x + 0.5 * ww
    y0 <- y - 0.5 * hh
    y1 <- y + 0.5 * hh
    if (debug) {
      if(length(table[i,j])>0)
      {
        if(!(is.zeroGrob(table[i,j]$grobs[[1]])))
        {
      grid.points(x, y, gp = gpar(cex = .75, fill = scales::alpha("orange", .5), col = "orange"))
      grid.points(x = unit.c(x0, x0, x1, x1), y = unit.c(y0, y1, y0, y1), gp = gpar(cex = .75, fill = scales::alpha("purple", .5), col = "purple"))
      grid.rect(x = x,
                y = y,
                width = ww, height = hh,
                gp = gpar(col = "green", fill = "transparent")
      )
        }
      }
    }
    
    if(length(table[i,j])>0)
    {
      if(!(is.zeroGrob(table[i,j]$grobs[[1]])))
      {
        nameOfElem <- ""
        
        if(grepl("axis.title.y", table[i,j][[1]][[1]]))
        {
          nameOfElem <- "yAxisTitle"
        }
        else if(grepl("axis.title.x", table[i,j][[1]][[1]]))
        {
          nameOfElem <- "xAxisTitle"
        }
        else if(grepl("plot.title", table[i,j][[1]][[1]]))
        {
          nameOfElem <- "plotTitle"
        }
        else if(grepl("absoluteGrob", table[i,j][[1]][[1]]))
        {
          if(grepl("axis-l",table[i,j][["layout"]][["name"]]))
          {
            nameOfElem <- "yAxisTicks"
          }
          else
          {
            nameOfElem <- "xAxisTicks"
          }
        }
        else
        {
          nameOfElem <- "dataVisual"
        }
      }
    }
    
    totalWidth  <- totalWidth + ww
    oldWW <- if (length(oldWW) == 0L) ww else grid::unit.c(oldWW, ww)
    oldHH <- if (length(oldHH) == 0L) hh else grid::unit.c(oldHH, hh)
    prevI <- i
    
    if(length(table[i,j])>0)
    {
      if(!(is.zeroGrob(table[i,j]$grobs[[1]])))
      {
    obj <- list(name = nameOfElem,# x = x, y = y, width = ww, height = hh,
                x0 = x0, x1 = x1, y0 = y0, y1 = y1)
    
        objs[[i, j]] <- convertObj(obj, targetUnit)
      }
    }
    
  }
  #grid::popViewport()
  return(objs)
}


createCoords <- function(gt)
{
  e1 <- grid.show.layout.modified.2(gtable:::gtable_layout(gt), gt, newpage = FALSE, vp.ex = 1, targetUnit = "cm")
    sendList <- list()
    nameList <- list()
    
    for(i in 1:12)
    {
      for(j in 1:9)
      {
        if(!(is.null(e1[[i,j]])))
        {
          sendList <- c(sendList,e1[i,j])
        }
      }
    }
  return(sendList)
}
