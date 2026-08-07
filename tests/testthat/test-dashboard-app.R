test_that("dashboard overview and programme tabs render", {
  app <- new_dashboard_app()
  on.exit(app$stop(), add = TRUE)

  expect_identical(app$get_value(input = "tab"), "overview")
  expect_false(is.null(app$wait_for_value(output = "totalPlot", timeout = 60000)))
  expect_false(is.null(app$wait_for_value(output = "uniquePlot", timeout = 60000)))

  app$set_inputs(tab = "programme")
  expect_identical(app$get_value(input = "tab"), "programme")
  expect_false(is.null(app$wait_for_value(output = "programmeUniquePlot", timeout = 60000)))
  expect_false(is.null(app$wait_for_value(output = "programmeFacultyPlot", timeout = 60000)))
})

test_that("dashboard specialised tabs render representative outputs", {
  app <- new_dashboard_app()
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(tab = "velocity")
  expect_false(is.null(app$wait_for_value(output = "velocityUniquePlot", timeout = 60000)))

  # Assert on outputs the tab shows by default. The studio plots below these sit inside
  # a conditionalPanel keyed on baseProgramme == "<...> Studio Participant", and no such
  # programme exists in all.csv or tags_selection.csv, so those panels stay hidden and
  # their outputs are never computed.
  app$set_inputs(tab = "unleash")
  expect_false(is.null(app$wait_for_value(output = "unleashUniquePlot", timeout = 60000)))
  expect_false(is.null(app$wait_for_value(output = "unleashFacultyPlot", timeout = 60000)))

  app$set_inputs(tab = "createmaker")
  expect_false(is.null(app$wait_for_value(output = "createmakerUniquePlot", timeout = 60000)))

  app$set_inputs(tab = "curricula")
  expect_false(is.null(app$wait_for_value(output = "curriculaUniquePlot", timeout = 60000)))
})

test_that("dashboard journey tab renders with a real destination", {
  app <- new_dashboard_app()
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(tab = "journey")
  app$set_inputs(baseYear = "2015")
  app$set_inputs(baseDestination = "2015 CIE Participant")
  app$click("updateTotal")

  journey_total <- paste(app$wait_for_value(output = "journeyTotal", timeout = 60000), collapse = " ")
  expect_match(journey_total, "Total", fixed = TRUE)
  expect_false(is.null(app$wait_for_value(output = "journeyBarChart", timeout = 60000)))
  expect_false(is.null(app$wait_for_value(output = "journeyEventHeatmap", timeout = 60000)))
})