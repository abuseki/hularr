test_that("polyarea works", {

  p <- matrix(c(0,2,0, 0,0,2), ncol=2)
  expect_equal(polyarea(p), 2)

  p <- matrix(c(0,2,2,0, 0,0,2,2), ncol=2)
  expect_equal(polyarea(p), 4)


  p <- matrix(c(0,1,1,2,2,3,3,0, 0,0,1,1,0,0,2,2), ncol = 2)
  expect_equal(polyarea(p), (3*2)-1)
})
