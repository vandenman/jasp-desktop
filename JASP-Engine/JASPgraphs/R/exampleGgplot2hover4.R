#rm(list = ls())

is.zeroGrob <- function(x) inherits(x, "zeroGrob")

# functions for alternative solution
isUnitNull <- function(x) endsWith(as.character(x), "null")
getUnitValue <- function(x) sapply(x, `[[`, 1L)

computeUnit <- function(u, all, type = c("width", "height")) {
  browser()
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
    ans <- grid::convertWidth(unew, "npc", valueOnly = TRUE)
  } else {
    ans <- grid::convertHeight(unew, "npc", valueOnly = TRUE)
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

grid.show.layout.modified.2 <- function(l, table, targetUnit = "native", debug = FALSE, newpage = FALSE, ...) {
  #browser()
  if (!grid:::is.layout(l)) 
    stop("'l' must be a layout")
  if (newpage) 
    grid.newpage()
  #COMMENTED vp.mid <- grid::viewport(0.5, 0.5, layout = l)
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
  e1 <- grid.show.layout.modified.2(gtable:::gtable_layout(gt), gt, newpage = FALSE, vp.ex = 1, targetUnit = "npc") #npc is needed to make sure we get normalized coordinates
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
