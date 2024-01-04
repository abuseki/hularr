test_that("test mineral formula recalculation works", {

  ##############################################################################
  # Garnet analysis from [Gregor Markl, "Minerale und Gesteine", 2015,
  # Springer Berlin Heidelberg]
  grt <- c(SiO2= 37.30 , Al2O3= 21.10, FeO= 29.90,
           MnO= 1.14, MgO= 0.9, CaO=9.02
  )
  mrc <- minRecalc(grt, 12)

  # sum of ox %wt
  expect_equal(round(mrc$sum_ox_wt, 2), 99.36)

  # sum of cation atoms, need tolerance here, since atom weight might differ
  expect_equal(round(mrc$sum_cat_atms, 2), 8.00, tolerance = 0.01)

  #formula units
  expect_equal(
    round(mrc$recalc$AtomUnits, 2),
    c(3, 2, 2.01, 0.08, 0.11, 0.79),
    tolerance = 0.01
    )

  ##############################################################################
  # Garnet analysis from [Okrusch, Martin / Matthes, Siegfried, "Mineralogie:
  # Eine Einführung in die spezielle Mineralogie, Petrologie und
  # Lagerstättenkunde", 2014, Springer Spektrum]
  grt <- c(SiO2= 37.1 , Al2O3= 20.3, Fe2O3= 0.5, FeO= 33,
           MnO= 5.45, MgO= 2.28, CaO= 0.99
  )
  mrc <- minRecalc(grt, 12)

  # sum of ox %wt
  expect_equal(round(mrc$sum_ox_wt, 2), 99.62)

  # sum of cation atoms, need tolerance here, since atom weight might differ
  expect_equal(round(mrc$sum_cat_atms, 2), 7.99, tolerance = 0.001)

  #formula units
  expect_equal(
    round(mrc$recalc$AtomUnits, 3),
    c(3.022, 1.949, 0.031, 2.248, 0.376, 0.277, 0.087),
    tolerance = 0.1
  )

  ##############################################################################
  # Another garnet, this time from
  # (https://geowiki.geo.lmu.de/wiki/Berechnung_von_Mineralformeln)
  grt <- c(SiO2= 36.3, Al2O3= 22.03, FeO= 29.7, MnO= 1.25, MgO= 2.13, CaO= 8.54)
  mrc <- minRecalc(grt, 12)

  # sum of ox %wt
  expect_equal(round(mrc$sum_ox_wt, 2), 99.95)

  # sum of cation atoms, need tolerance here, since atom weight might differ
  expect_equal(round(mrc$sum_cat_atms, 2), 8.05, tolerance = 0.001)

  #formula units
  expect_equal(
    round(mrc$recalc$AtomUnits, 3),
    c(2.908, 2.080, 1.990, 0.085, 0.254, 0.733),
    tolerance = 0.001
  )

  ##############################################################################
  # Lepdolite, (https://www.handbookofmineralogy.org/pdfs/lepidolite.pdf)
  # Chemistry (1), water and fluorine ignored so one oxygen apfu less
  lpd <- c(SiO2=48.58, Al2O3=28.93, FeO=0.04, MnO=0.92, Li2O=3.70,
           Na2O=0.87, K2O=10.02, Rb2O=0.91, Cs2O=0.16
  )
  mrc <- minRecalc(ox_wt = lpd, 11)
  #formula units
  exps <- c(K= 0.85, Na= 0.11, Rb= 0.04, Al= 1.5+0.77, Li= 0.99, Mn= 0.05, Si= 3.23)
  lapply(names(exps), function(n) {
    apfu <- round(mrc$recalc[mrc$recalc$Cation == n, "AtomUnits"], 2)
    expect_equal(apfu, exps[[n]], tolerance = 0.02)
  })

  ##############################################################################
  # Lepdolite, (https://www.handbookofmineralogy.org/pdfs/lepidolite.pdf)
  # Chemistry (2), water and fluorine ignored so one oxygen apfu less
  lpd <- c(SiO2= 51.45, Al2O3= 22.62, Fe2O3= 0.16, FeO= 0.04, MnO= 0.51,
           MgO= 0.53, CaO= 0.20, Li2O= 5.42, Na2O= 0.26, K2O= 9.09, Rb2O= 1.69,
           Cs2O= 0.94
  )
  mrc <- minRecalc(ox_wt = lpd, 11)
  #formula units
  exps <- c(K= 0.79, Rb= 0.07, Na= 0.03, Cs= 0.03, Ca= 0.01, Li= 1.48,
            Al= 1.3+0.51, Mg= 0.05, Mn= 0.03, Si= 3.49
  )
  lapply(names(exps), function(n) {
    apfu <- round(mrc$recalc[mrc$recalc$Cation == n, "AtomUnits"], 2)
    expect_equal(apfu, exps[[n]], tolerance = 0.02)
  })

  ##############################################################################
  # Feldspar from https://sarahlambart.com/teaching/mineralogy-06-01.pdf, p14
  fsp <- c(SiO2= 68.20, Al2O3=19.19, Na2O=10.20, K2O=2.32)
  mrc <- minRecalc(fsp, 8)
  #formula units
  expect_equal(
    round(mrc$recalc$AtomUnits, 4),
    c(2.9997, 1.0001, 0.8699, 0.1311),
    tolerance = 0.002
  )

  ##############################################################################
  # Opx from https://sarahlambart.com/teaching/mineralogy-06-01.pdf, p18
  opx <- c(MgO= 25.69, FeO= 19.62, SiO2= 54.69)
  mrc <- minRecalc(opx, 3)
  expect_equal(
    round(mrc$recalc$AtomUnits, 4),
    c(0.7, 0.3, 1),
    tolerance = 0.001
  )

  ##############################################################################
  # Cpx from https://sarahlambart.com/teaching/mineralogy-06-01.pdf, p22
  px <- c(Na2O= 14.12, Fe2O3=21.83, Al2O3=9.29, SiO2= 54.76)
  mrc <- minRecalc(px, 6)
  expect_equal(
    round(mrc$recalc$AtomUnits, 4),
    c(1, 0.6, 0.4, 2),
    tolerance = 0.001
  )

  ##############################################################################
  # Px from https://sarahlambart.com/teaching/mineralogy-06-01.pdf, p28
  px <- c(SiO2= 56.64, Na2O= 4.38, Al2O3= 7.21, MgO= 13.30, CaO= 18.46)
  mrc <- minRecalc(px, 6)
  expect_equal(
    round(mrc$recalc$AtomUnits, 1),
    c(2, 0.3, 0.3, 0.7, 0.7)
  )

})
