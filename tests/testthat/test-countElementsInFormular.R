test_that("element counting in a formular works works works", {

  fmlas <- list(
    'Mg2(SiO4)',
    'Mg3Al2(SiO4)3',
    'KAl2(Si3Al)O10(OH)2',
    'KLi1.5Al1.5(Si3Al)O10F2'
  )
  cnts <- list(
    c(Mg= 2, O=4, Si=1),
    c(Al=2, Mg=3, O=12, Si=3),
    c(Al=3, H=2, K=1, O=12, Si=3),
    c(Al=2.5, F=2, K=1, Li=1.5, O=10, Si=3)
  )

  expect_true(all(unlist(
    lapply(seq_along(fmlas), function(i) {
      countElementsInFormula(fmlas[[i]]) == cnts[[i]]
    })
  )))

})
