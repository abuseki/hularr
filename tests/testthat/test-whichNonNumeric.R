test_that("multiplication works", {
  # basic
  t <- c(1:5, 'X', 7:10, ' ', 12:15, 'bad', NA, NA)

  expect_equal(which.nonnumeric(t), c(6, 11, 16))

  # using a data.frame
  t <- data.frame(
    sampleName= c(LETTERS[1:10]),
    V1= c(1, 'bad too', 3:10),
    V2= c(1:2, 'XXX', 4:10),
    V3= c(1:8, NA, ' ')
  )
  expect_equal(unlist(lapply(t[-1], which.nonnumeric), use.names = FALSE), c(2, 3, 10))
})
