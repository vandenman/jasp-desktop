context("Reliability Analysis")

# does not test
# - missing values exclusion

test_that("Main table results match", {
  options <- jasptools::analysisOptions("ReliabilityAnalysis")
  options$variables <- c("contcor1", "contcor2", "contNormal")
  options$reverseScaledItems <- "contcor2"
  options$alphaScale <- TRUE
  options$averageInterItemCor <- TRUE
  options$confAlpha <- TRUE
  options$glbScale <- TRUE
  options$gutmannScale <- TRUE
  options$mcDonaldScale <- TRUE
  options$meanScale <- TRUE
  options$sdScale <- TRUE
  results <- jasptools::run("ReliabilityAnalysis", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["reliabilityScale"]][["data"]]
  expect_equal_tables(table,
    list("scale", -0.757822989578577, -0.0677657928415725, 0.667932535083157,
         0.622700230679449, -0.175972651899464, -0.02217061461, 0.144515070286093,
         -1.45211881901153, -0.235388804018903)
  )
})

test_that("Item Statistics table matches", {
  options <- jasptools::analysisOptions("ReliabilityAnalysis")
  options$variables <- c("contcor1", "contcor2", "contNormal")
  options$alphaItem <- TRUE
  options$confAlpha <- TRUE
  options$gutmannItem <- TRUE
  options$itemRestCor <- TRUE
  options$mcDonaldItem <- TRUE
  options$meanItem <- TRUE
  options$sdItem <- TRUE
  results <- jasptools::run("ReliabilityAnalysis", "test.csv", options, view=FALSE, quiet=TRUE)
  table <- results[["results"]][["reliabilityItemsObj"]][["reliabilityItems"]][["data"]]
  expect_equal_tables(table,
    list("contcor1", 0.0618194975467092, 0.0319398198963565, 0.061902485553013,
         0.560156128034403, 0.05254867287, 1.01183864387684, "contcor2",
         0.277152727398941, 0.161031927910319, 0.27739448681683, 0.442807451055322,
         0.06968807084, 1.0041493380131, "contNormal", 0.79299280264282,
         0.657010063712354, 0.793006727117146, 0.106272823965938, -0.18874858754,
         1.05841360919316)
  )
})

test_that("ICC table matches", {
	options <- jasptools::analysisOptions("ReliabilityAnalysis")
	options$variables <- c("contcor1", "contcor2", "contNormal")
	options$ICCScale <- TRUE
	results <- jasptools::run("ReliabilityAnalysis", "test.csv", options, view=FALSE, quiet=TRUE)
	table <- results[["results"]][["reliabilityICC"]][["data"]]
	expect_equal_tables(table,
						list("Single raters absolute", "ICC1", 0.270736580074761, 2.11373986138993,
							 99, 200, 4.15511641435793e-06, 0.146823863872799, 0.400742095405812,
							 "Single random raters", "ICC2", 0.273772630694275, 2.15072767362195,
							 99, 198, 2.66481826493225e-06, 0.15092759374215, 0.402827483048287,
							 "Single fixed raters", "ICC3", 0.277235165519258, 2.15072767362195,
							 99, 198, 2.66481826493225e-06, 0.152965433244052, 0.407143912164826,
							 "Average raters absolute", "ICC1k", 0.526904886326726, 2.11373986138993,
							 99, 200, 4.15511641435793e-06, 0.340488049544983, 0.66735322593966,
							 "Average random raters", "ICC2k", 0.53072301830183, 2.15072767362195,
							 99, 198, 2.66481826493225e-06, 0.347798115780762, 0.669276507325944,
							 "Average fixed raters", "ICC3k", 0.535041087598067, 2.15072767362195,
							 99, 198, 2.66481826493225e-06, 0.351394022078838, 0.67322930800452)
	)
})

