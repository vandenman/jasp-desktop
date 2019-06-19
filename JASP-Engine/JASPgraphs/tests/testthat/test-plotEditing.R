test_that("manipulating continuous axes works", {

  data("mtcars")
  g1a <- ggplot(mtcars, aes(x = mpg, y = disp)) + geom_line()

  opts1a <- plotEditingOptions(g1a)

  opts1b <- opts1a
  opts1b$xAxis$settings$breaks <- seq(10, 20, 2)
  opts1b$xAxis$settings$labels <- seq(10, 20, 2)

  g1b <- plotEditing(g1a, opts1b)
  expect_equal(plotEditingOptions(g1b), opts1b)

  opts1c <- opts1a
  opts1c$xAxis$settings$limits <- c(15, 25) # <- this fails!
  opts1c$yAxis$settings$breaks <- seq(10, 20, 2)
  opts1c$yAxis$settings$labels <- seq(10, 20, 2)

  debugonce(plotEditing)
  g1c <- plotEditing(g1a, opts1c)
  expect_equal(plotEditingOptions(g1c), opts1c)


})

debugonce(ggplot_build)
ggplot_build(g1a)

# limits are calculated here:
# layout$setup_panel_params
# <ggproto method>
#   <Wrapper function>
#   function (...)
#     f(..., self = self)
#
# <Inner function (f)>
#   function (self)
#   {
#     self$coord$modify_scales(self$panel_scales_x, self$panel_scales_y)
#     scales_x <- self$panel_scales_x[self$layout$SCALE_X]
#     scales_y <- self$panel_scales_y[self$layout$SCALE_Y]
#     setup_panel_params <- function(scale_x, scale_y) {
#       self$coord$setup_panel_params(scale_x, scale_y, params = self$coord_params)
#     }
#     self$panel_params <- Map(setup_panel_params, scales_x, scales_y)
#     invisible()
#   }
# layout$coord$setup_panel_params
# <ggproto method>
#   <Wrapper function>
#   function (...)
#     f(..., self = self)
#
# <Inner function (f)>
#   function (self, scale_x, scale_y, params = list())
#   {
#     train_cartesian <- function(scale, limits, name) {
#       range <- scale_range(scale, limits, self$expand)
#       out <- scale$break_info(range)
#       out$arrange <- scale$axis_order()
#       names(out) <- paste(name, names(out), sep = ".")
#       out
#     }
#     c(train_cartesian(scale_x, self$limits$x, "x"), train_cartesian(scale_y,
#                                                                     self$limits$y, "y"))
#   }

# look at
# ggplot2:::expand_default
# ggplot2:::`%|W|%`
# scales:::expand_range
