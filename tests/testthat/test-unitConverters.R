test_that("simple unit converters work", {

  s <- seq(1, 100, .005)

  expect_equal(wt2ppm(s), s*10000)
  expect_equal(ppm2wt(s), s/10000)

})
