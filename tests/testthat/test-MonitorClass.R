# Tests for MonitorS4 class

test_that("MonitorS4 class exists", {
  expect_true(isClass("MonitorS4"))
})

test_that("createMonitor creates valid object", {
  # Skip if no test data available
  test_file <- system.file("extdata", "Monitor36.txt", package = "RMB")
  skip_if(!file.exists(test_file) && !file.exists("../../test/Monitor36.txt"),
          "No test data available")
  
  # Use local test file if package data not available
  if (!file.exists(test_file)) {
    test_file <- "../../test/Monitor36.txt"
  }
  
  monitor <- createMonitor(test_file, verbose = FALSE)
  
  expect_s4_class(monitor, "MonitorS4")
  expect_true(length(monitor@monitor_name) > 0)
})

test_that("processMonitor extracts assays", {
  test_file <- "../../test/Monitor36.txt"
  skip_if(!file.exists(test_file), "No test data available")
  
  monitor <- createMonitor(test_file, verbose = FALSE)
  monitor <- processMonitor(monitor, verbose = FALSE)
  
  expect_true(!is.null(monitor@assays$mt))
  expect_true(!is.null(monitor@assays$pn))
  expect_true(nrow(monitor@assays$mt) > 0)
  expect_true(ncol(monitor@assays$mt) > 0)
})

test_that("removeDeadFlies filters correctly", {
  test_file <- "../../test/Monitor36.txt"
  skip_if(!file.exists(test_file), "No test data available")
  
  monitor <- createMonitor(test_file, verbose = FALSE)
  monitor <- processMonitor(monitor, verbose = FALSE)
  
  n_before <- ncol(monitor@assays$mt)
  monitor <- removeDeadFlies(monitor, min_movement = 10, verbose = FALSE)
  n_after <- ncol(monitor@assays$mt)
  
  # Should have same or fewer channels
  expect_true(n_after <= n_before)
})

test_that("mergeMonitors combines multiple monitors", {
  test_files <- list.files("../../test", pattern = "Monitor.*\\.txt$", full.names = TRUE)
  skip_if(length(test_files) < 2, "Need at least 2 monitors for merge test")
  
  monitor_list <- lapply(test_files[1:2], function(f) {
    m <- createMonitor(f, verbose = FALSE)
    processMonitor(m, verbose = FALSE)
  })
  
  merged <- mergeMonitors(monitor_list, verbose = FALSE)
  
  # Should have combined channels
  expect_true(ncol(merged@assays$mt) > ncol(monitor_list[[1]]@assays$mt))
})
