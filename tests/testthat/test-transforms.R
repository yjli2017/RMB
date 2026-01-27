# Tests for data transformation functions

test_that("activityToAwake converts activity correctly", {
  # Create test data: 10 timepoints, 2 flies
  # Fly 1: awake (some activity)
  # Fly 2: asleep (5+ consecutive zeros)
  test_data <- data.frame(
    fly1 = c(1, 0, 2, 1, 0, 0, 0, 1, 2, 1),
    fly2 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )
  
  result <- activityToAwake(test_data, threshold = 5)
  
  # Fly 1 should be mostly awake (has activity)
  expect_true(sum(result$fly1) > 0)
  
  # Fly 2 should be all asleep (5+ consecutive zeros)
  expect_equal(sum(result$fly2), 0)
})

test_that("awakeToSleep flips binary correctly", {
  test_data <- data.frame(
    fly1 = c(1, 1, 0, 0, 1),
    fly2 = c(0, 0, 1, 1, 0)
  )
  
  result <- awakeToSleep(test_data)
  
  expect_equal(result$fly1, c(0, 0, 1, 1, 0))
  expect_equal(result$fly2, c(1, 1, 0, 0, 1))
})

test_that("positionFrequency counts positions correctly", {
  test_data <- data.frame(
    fly1 = c(1, 1, 2, 3, 15),
    fly2 = c(1, 1, 1, 1, 1)
  )
  
  result <- positionFrequency(test_data, positions = 1:15)
  
  # Fly 1: 2 at position 1, 1 at 2, 1 at 3, 1 at 15
  expect_equal(result[1, "fly1"], 2)
  expect_equal(result[2, "fly1"], 1)
  expect_equal(result[3, "fly1"], 1)
  expect_equal(result[15, "fly1"], 1)
  
  # Fly 2: all 5 at position 1
  expect_equal(result[1, "fly2"], 5)
  expect_equal(sum(result[2:15, "fly2"]), 0)
})

test_that("flipBinary handles all cases", {
  test_data <- data.frame(
    col = c(0, 1, 0, 1, 5)  # 5 should remain unchanged
  )
  
  result <- flipBinary(test_data)
  
  expect_equal(result$col, c(1, 0, 1, 0, 5))
})

test_that("maskByAwake multiplies correctly", {
  position <- data.frame(
    fly1 = c(5, 5, 5, 5),
    fly2 = c(10, 10, 10, 10)
  )
  
  awake <- data.frame(
    fly1 = c(1, 1, 0, 0),
    fly2 = c(0, 1, 1, 0)
  )
  
  result <- maskByAwake(position, awake)
  
  expect_equal(result$fly1, c(5, 5, 0, 0))
  expect_equal(result$fly2, c(0, 10, 10, 0))
})
