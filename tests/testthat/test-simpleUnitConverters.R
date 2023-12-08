test_that("wt-ppm converters work", {

  s <- seq(1, 100, .005)

  # wt% <> ppm
  expect_equal(wt2ppm(s), s*10000)
  expect_equal(ppm2wt(s), s/10000)
})

test_that("cm-inch converters work", {

  s <- seq(1, 100, .005)

  # cm <> inch
  expect_equal(cm2in(s), s/2.54)
  expect_equal(in2cm(s), s*2.54)
})
