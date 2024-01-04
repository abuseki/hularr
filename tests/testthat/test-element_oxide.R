test_that("element-oxide works", {

  # elOxTable_121 is a subset of elOxTable
  expect_true(all(elOxTable_121$Oxide %in% elOxTable$Oxide))
  expect_true(all(table(elOxTable_121$Element) == 1))
  expect_true(
    length(unique(elOxTable_121$Element)) == length(unique(elOxTable$Element))
  )

  # eloxFactor
  ## sampled and manually calculated...
  exps <- c(
    TiO= georefdatar::aw("Ti") / sum(georefdatar::aw("Ti"), georefdatar::aw("O")),
    SiO2= georefdatar::aw("Si") / sum(georefdatar::aw("Si"), 2*georefdatar::aw("O")),
    K2O= 2*georefdatar::aw("K") / sum(2*georefdatar::aw("K"), georefdatar::aw("O")),
    Al2O3= 2*georefdatar::aw("Al") / sum(2*georefdatar::aw("Al"), 3*georefdatar::aw("O")),
    Fe2O3= 2*georefdatar::aw("Fe") / sum(2*georefdatar::aw("Fe"), 3*georefdatar::aw("O")),
    WO3= georefdatar::aw("W") / sum(georefdatar::aw("W"), 3*georefdatar::aw("O")),
    V2O3= 2*georefdatar::aw("V") / sum(2*georefdatar::aw("V"), 3*georefdatar::aw("O")),
    CuO= georefdatar::aw("Cu") / sum(georefdatar::aw("Cu"), georefdatar::aw("O")),
    PbO= georefdatar::aw("Pb") / sum(georefdatar::aw("Pb"), georefdatar::aw("O")),
    As2O5= 2*georefdatar::aw("As") / sum(2*georefdatar::aw("As"), 5*georefdatar::aw("O"))
  )
  lapply(names(exps), function(ox) {
    expect_equal(eloxFactor(ox), signif(exps[ox], 5),  ignore_attr = TRUE)
  })

  # el2ox
  expect_error(el2ox("X", 0, NULL, NULL))

  # ask or an oxide explicitly
  expect_equal(el2ox("Si", 1, "SiO2"), 1/eloxFactor("SiO2"), ignore_attr= TRUE)

  # more than one result for an oxide
  expect_equal(
    el2ox("U", 1),
    1/ sapply(c("UO3", "U2O5", "U2O3", "UO2"), eloxFactor),
    ignore_attr= TRUE
  )
  ## Using eot_121 only one uranium oxide
  expect_equal(el2ox("U", 1, LUT = elOxTable_121),
    1/ eloxFactor("UO2"),
    ignore_attr= TRUE
  )

  # ask for an non existing oxide
  expect_equal(
    el2ox("Ar", 666, "Ar666O666"),
    666 / eloxFactor("Ar666O666")
  )


  # ox2el
  expect_equal(ox2el("SiO2", 1), 0.46744)
})
