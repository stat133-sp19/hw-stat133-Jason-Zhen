context("Testing Binomial functions")

test_that("bin_choose works for values where n > k", {
  
  expect_equal(bin_choose(10,3),120)
  expect_equal(bin_choose(6,3),20)
  expect_equal(bin_choose(4,1),4)
})

test_that("bin_choose errors for values where n < k", {
  
  expect_error(bin_choose(3, 5))
})

test_that("bin_choose has length 1", {
  
  expect_length(bin_choose(5, 3),1)
})


test_that("bin_probability errors for invalid arguments, trials < 0, success > trials, prob < 0", {
  
  expect_error(bin_probability(-1, 5, 0.3))
  expect_error(bin_probability(5, 3 , 0.3))
  expect_error(bin_probability(3, 5, -0.1))
})

test_that("bin_probability works as expected", {
  
  expect_equal(bin_probability(3, 5, 0.3),0.1323)
  expect_equal(bin_probability(2, 4 , 0.7),0.2646)
})

test_that("bin_probability has length 1", {
  
  expect_length(bin_probability(1, 5, 0.3),1)
})



test_that("bin_distribution errors for invalid arguments, trials < 0, prob < 0, trials is not a whole number", {
  
  expect_error(bin_distribution(-1, 0.3))
  expect_error(bin_distribution(5, -0.3))
  expect_error(bin_distribution(5.5, -0.3))
})

test_that("bin_distribution is a bindis with length 2", {
  
  expect_s3_class(bin_distribution(3, 0.3), "bindis")
  expect_length(bin_distribution(3,0.3),2)
})





test_that("bin_cumulative errors for invalid arguments, trials < 0, prob < 0, trials is not a whole number", {
  
  expect_error(bin_cumulative(-1, 0.3))
  expect_error(bin_cumulative(5, -0.3))
  expect_error(bin_cumulative(5.5, -0.3))
})

test_that("bin_cumulative is a bincum with length 3", {
  
  expect_s3_class(bin_cumulative(3, 0.3), "bincum")
  expect_length(bin_cumulative(3,0.3),3)
})

