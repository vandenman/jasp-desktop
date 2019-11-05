context("Plot Editing")

data("mtcars")
ensureCharacter            <- JASPgraphs:::ensureCharacter
ensureCharacter.character  <- JASPgraphs:::ensureCharacter.character
ensureCharacter.expression <- JASPgraphs:::ensureCharacter.expression
ensureCharacter.NULL       <- JASPgraphs:::ensureCharacter.NULL
ensureCharacter.default    <- JASPgraphs:::ensureCharacter.default

test_that("manipulating continuous axes works", {

  g1a <- ggplot(mtcars, aes(x = mpg, y = disp)) + geom_line()

  opts1a <- plotEditingOptions(g1a)

  opts1b <- opts1a
  opts1b$xAxis$settings$breaks <- seq(10, 20, 2)
  opts1b$xAxis$settings$labels <- as.character(seq(10, 20, 2))
  opts1b$xAxis$settings$title  <- ensureCharacter("HOOOOI")
  opts1b$xAxis$settings$expand[c(2, 4)] <- c(5, 10)

  g1b <- plotEditing(g1a, opts1b)
  expect_equal(plotEditingOptions(g1b), opts1b)

  opts1c <- opts1a
  opts1c$yAxis$settings$breaks <- seq(50, 400, 50)
  opts1c$yAxis$settings$labels <- as.character(seq(50, 400, 50))

  g1c <- plotEditing(g1a, opts1c)
  expect_equal(plotEditingOptions(g1c), opts1c)
  
})

test_that("axes titles with uncommon axis labels return character", {
  
  g1a <- ggplot(mtcars, aes(x = mpg, y = disp)) + geom_line() + 
    labs(x = expression(alpha^2), y = NULL)
  
  opts1a <- plotEditingOptions(g1a)
  
  expect_identical(
    opts1a[["xAxis"]][["settings"]][["title"]],
    list(value = "alpha^2", type = "expression"), 
    label = "plotEditingOptions returns correct type for xAxis (expression)"
  )
  
  expect_identical(
    opts1a[["yAxis"]][["settings"]][["title"]],
    list(value = "", type = "NULL"), 
    label = "plotEditingOptions returns correct type for yAxis (NULL)"
  )
  
  opts1b <- opts1a
  opts1b$xAxis$settings$title <- ensureCharacter(NULL)
  opts1b$yAxis$settings$title <- ensureCharacter(expression(beta^2))

  g1b <- plotEditing(g1a, opts1b)
  # debugonce(JASPgraphs:::plotEditingOptions.ggplot_built)
  expect_equal(plotEditingOptions(g1b), opts1b)
  
  opts1c <- opts1a
  opts1c$yAxis$settings$breaks <- seq(50, 400, 50)
  opts1c$yAxis$settings$labels <- as.character(seq(50, 400, 50))
  
  g1c <- plotEditing(g1a, opts1c)
  expect_equal(plotEditingOptions(g1c), opts1c)
  
})

test_that("manipulating discrete axes works", {

  g2a <- ggplot(mtcars, aes(x = mpg, y = factor(cyl))) + geom_point()

  opts2a <- plotEditingOptions(g2a)

  opts2b <- opts2a
  opts2b$xAxis$settings$breaks <- seq(10, 20, 2)
  opts2b$xAxis$settings$labels <- as.character(seq(10, 20, 2))
  opts2b$xAxis$settings$title  <- "HOOOOI"

  g2b <- plotEditing(g2a, opts2b)
  expect_equal(plotEditingOptions(g2b), opts2b)

  opts2c <- opts2a
  opts2c$yAxis$settings$shown  <- c("4", "6")
  opts2c$yAxis$settings$labels <- c("vier", "zes")
  opts2c$yAxis$settings$title <- "YOYOYO"

  g2c <- plotEditing(g2a, opts2c)
  expect_equal(plotEditingOptions(g2c), opts2c)

  opts2d <- opts2c
  opts2d$xAxis$settings$breaks <- seq(15, 25, 5)
  opts2d$xAxis$settings$labels <- as.character(seq(15, 25, 5))

  g2d <- plotEditing(g2a, opts2d)
  expect_equal(plotEditingOptions(g2d), opts2d)

})
