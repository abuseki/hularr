test_that("element counting in a formula works works works", {

  fmlas <- list(
    'Mg2(SiO4)',
    'Mg3Al2(SiO4)3',
    'KAl2(Si3Al)O10(OH)2',
    'KLi1.5Al1.5(Si3Al)O10F2',
    'NaMg3Al6(Si6O18)(BO3)3(OH)3(OH)',
    'Mg2(SiO3|(OH)2)2',

    'Zx5Y(C(Dx(DxX)4)7)9'
  )
  cnts <- list(
    c(Mg= 2, O=4, Si=1),
    c(Al=2, Mg=3, O=12, Si=3),
    c(Al=3, H=2, K=1, O=12, Si=3),
    c(Al=2.5, F=2, K=1, Li=1.5, O=10, Si=3),
    c(Na=1, Mg=3, Al=6, Si=6, O=31, B=3, H=4),
    c(Mg=2, Si=2, O= 10, H=4),

    c(Zx=5, Y=1, C=9, Dx= 315, X=252)
  )

  lapply(seq_along(fmlas), function(i) {
    neif <- countElementsInFormula(fmlas[[i]])
    expect_equal(neif, cnts[[i]][order(names(cnts[[i]]))])
  })
})
