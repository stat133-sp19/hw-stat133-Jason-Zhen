context("Testing Aux functions")

test_that("aux_mean gives expected value and has expected length and type", {

  expect_equal(aux_mean(10,0.5),5)
  expect_equal(aux_mean(10,0.3),3)
  expect_length(aux_mean(40,0.1),1)
  expect_type(aux_mean(40,0.1),"double")
})

test_that("aux_variance gives expected value and has expected length and type", {

  expect_equal(aux_variance(10,0.5),2.5)
  expect_equal(aux_variance(10,0.3),2.1)
  expect_length(aux_variance(40,0.1),1)
  expect_type(aux_variance(40,0.1),"double")
})

test_that("aux_mode gives expected value and has expected length and type", {

  expect_equal(aux_mode(10,0.5),5)
  expect_equal(aux_mode(10,0.3),3)
  expect_length(aux_mode(40,0.1),1)
  expect_type(aux_mode(40,0.1),"double")
})

test_that("aux_skewness gives expected value and has expected length and type", {

  expect_equal(aux_skewness(10,0.5),0)
  expect_length(aux_skewness(40,0.1),1)
  expect_type(aux_skewness(40,0.1),"double")
})

test_that("aux_kurtosis gives expected value and has expected length and type", {

  expect_equal(aux_kurtosis(10,0.5),-0.2)
  expect_length(aux_kurtosis(40,0.1),1)
  expect_type(aux_kurtosis(40,0.1),"double")
})
