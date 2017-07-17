library(nullnetr)
context("Plot_preferences")

# Dummy nullnet object for unit testing + load files for controlling plot order
s.1 <- list()
s.1$obs.interactions <- read.csv(system.file("testdata", "obs_data.csv",
                                             package = "nullnetr"))
s.1$rand.data <- read.csv(system.file("testdata", "sim_data.csv",
                                      package = "nullnetr"))
s.1$n.iterations <- 100
class(s.1) <- "nullnet"
ro <- read.csv(system.file("testdata", "df1_res_order.csv", package = "nullnetr"))
ro.err1 <- read.csv(system.file("testdata", "df1_res_order_error.csv",
                                package = "nullnetr"))


# Check error messages: i) significance level is outside 0-1, ii) invalid
#  'bar.type' value, & iii) resource names match when a 'res.order' data frame
#  is specified.
test_that("Basic error warnings",{
  expect_error(plot_preferences(s.1, node = "A", signif.level = 1.1))
  expect_error(plot_preferences(s.1, node = "A", bar.type = "height"))
  expect_error(plot_preferences(s.1, node = "A", res.order = ro[-1, ]))
  expect_error(plot_preferences(s.1, node = "A", res.order = ro.err1))
})


test_that("Check appearance of plot",{
  pl1 <- function() plot_preferences(s.1, node = "A", signif.level = 0.95,
                                     bar.colours = c("#998EC3", "#F7F7F7",
                                     "#F1A340"), bar.type = "counts")
  pl2 <- function() plot_preferences(s.1, node = "B", signif.level = 0.95,
                                     bar.colours = c("#998EC3", "#F7F7F7",
                                     "#F1A340"), bar.type = "counts")
  pl3 <- function() plot_preferences(s.1, node = "A", bar.colours = c("#998EC3",
                                     "#F7F7F7", "#F1A340"), bar.type = "SES")
  pl4 <- function() plot_preferences(s.1, node = "B", bar.colours = c("#998EC3",
                                     "#F7F7F7", "#F1A340"), bar.type = "SES")
  # Use the shiny app via vdiffr::manage_cases(".") to create new cases
  vdiffr::expect_doppelganger("Plot preferences A 0.95", pl1)
  vdiffr::expect_doppelganger("Plot preferences B 0.95", pl2)
  vdiffr::expect_doppelganger("Plot preferences A SES", pl3)
  vdiffr::expect_doppelganger("Plot preferences B SES", pl4)
})
