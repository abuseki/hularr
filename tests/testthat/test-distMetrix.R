test_that("dist.euclidean works", {
  expect_equal(
    dist.euclidean(c(0, 0), c(1, 1)),
    sqrt(2)
  )

  expect_equal(
    dist.euclidean(c(0, 0, 0), c(1, 1, 1)),
    sqrt(3)
  )
})


test_that("dist.manhattan works", {
  expect_equal(
    dist.manhattan(c(0, 0), c(1, 1)),
    2
  )

  expect_equal(
    dist.manhattan(c(0, 0, 0), c(1, 1, 1)),
    3
  )
})


test_that("dist.chebyshev works", {
  expect_equal(
    dist.chebyshev(c(6, 3), c(2, 8)),
    5
  )

  x <- runif(3, 1, 50)
  y <- runif(3, 1, 90)
  dxy <- abs(x-y)

  expect_equal(dist.chebyshev(x, y), max(dxy))

})
