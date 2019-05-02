context("Testing Check functions")

test_that("check_prob with probability between 0 and 1", {
  
  expect_true(check_prob(0.5))
  expect_true(check_prob(0.7))
  expect_true(check_prob(1))
})

test_that("check_prob with probability with length > 1", {
  
  expect_error(check_prob(c(0.5,0.7,1)))
  expect_error(check_prob(seq(0.1,0.3,by=0.1)))
  expect_error(check_prob(1:2))
})

test_that("check_prob with probability > 1", {
  
  expect_error(check_prob(1.3))
  expect_error(check_prob(9))
  expect_error(check_prob(1.1))
})

test_that("check_prob gives a value with length of 1", {
  
  expect_length(check_prob(0.5),1)
})




test_that("check_trials with trials > 0 and trial is a whole number", {
  
  expect_true(check_trials(1))
  expect_true(check_trials(4.0))
})

test_that("check_trials with trials with length > 1", {
  
  expect_error(check_trials(1:5))
  expect_error(check_trials(1:2))
  expect_error(check_trials(c(1,2)))
})

test_that("check_trials with trials < 0 or trials is not a whole number", {
  
  expect_error(check_trials(-1))
  expect_error(check_trials(-2.3))
  expect_error(check_trials(1.1))
})

test_that("check_trials gives a logical value", {
  
  expect_type(check_trials(4),"logical")
})



test_that("check_success with success <= trials and success > 0", {
  
  expect_true(check_success(success = 1,trials = 2))
  expect_true(check_success(success = 2, trials = 5))
  expect_true(check_success(success = 5, trials = 9))
})

test_that("check_success with success <= trials but success < 0", {
  
  expect_error(check_success(success = -1,trials = 2))
  expect_error(check_success(success = -3, trials = 1))
})

test_that("check_success with success > trials", {
  
  expect_error(check_success(success = 3,trials = 2))
  expect_error(check_success(success = 2, trials = 1))
})

test_that("check_success with sucess is not a whole number", {
  
  expect_error(check_success(success = 3.3,trials = 4))
  expect_error(check_success(success = 2.1, trials = 3))
})

test_that("check_success gives a logical value", {
  
  expect_type(check_success(3,5),"logical")
})
