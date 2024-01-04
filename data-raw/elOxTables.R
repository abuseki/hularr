## code to prepare `elOxTable` data set goes here

# elements with known atomic weights
es <- georefdatar::IUPAC_StdAW[ which(!is.na(georefdatar::IUPAC_StdAW$`abrStdAW::Value`)), "Symbol"]

# these elements and their oxidation state
oxss <- georefdatar::pte[georefdatar::pte$Symbol %in% es, c("Symbol", "OxidationStates")]

# build list of oxides
oxs <-unlist(apply(oxss, 1, function(x) {
  l <- unlist(lapply(strsplit(x[["OxidationStates"]], split=",", fixed = T), as.numeric))

  # filter out positive oxidation states -- we want to build oxides
  i <- which(l>0)

  lapply(l[i], function(os) {
    lcm <- numbers::LCM(os, 2)
    nCat <- lcm/os
    nOx <- lcm/2
    paste0(x[["Symbol"]], if(nCat>1) nCat, "O", if(nOx>1) nOx)
  })
}))

# build table of conversion factors
df <- do.call(rbind, lapply(oxs, function(o) {
  neif <- hularr::countElementsInFormula(o)
  idEl <- which(names(neif) != 'O')
  idOx <- which(names(neif) == 'O')
  elSy <- names(neif)[idEl]
  cf <- signif(georefdatar::aw(elSy)* neif[idEl] / hularr::mass(o), 5)

  data.frame(o, elSy, neif[idEl], neif[idOx], cf)
}))
colnames(df) <- c('Oxide', 'Element', 'nEl', 'nOx', 'Factor')

# reduce and order upper table to our needs
elOxTable <- df[order(df$Element, rev(df$Oxide)), c('Element', 'Oxide', 'Factor')]

# reset rownames
rownames(elOxTable) <- NULL


usethis::use_data(elOxTable, overwrite = TRUE)


################################################################################
# build a simplified elOxTable without polyvalent entries of the elements
################################################################################

# Selection of oxides of polyvalent elements
commonOxs <- c("As2O3", "Au2O3", "Bi2O3", "Br2O5", "CO2", "Ce2O3", "Cl2O",
               "Co2O3", "Cr2O3", "CuO", "Eu2O3", "FeO", "GeO2", "HgO", "I2O5",
               "IrO2", "MnO", "N2O3", "Nb2O5", "NiO", "OsO2", "P2O5", "Pa2O5",
               "PbO", "PdO", "PtO2", "Re2O7", "SO2", "Sb2O3", "SeO2", "SiO2",
               "Sm2O3", "SnO2", "TeO2", "TiO2", "Tl2O3", "UO2", "V2O3", "Yb2O3"
)
els <- gsub("\\d?O\\d?$", "", commonOxs)
names(commonOxs) <-  els

# build new table from upper table
eot <- elOxTable
eott <- table(eot$Element)
eotu <- eot[eot$Element %in% names(which(eott==1)), ]
eotu <- rbind(eotu, eot[eot$Oxide %in% commonOxs, ])
eotu <- eotu[order(eotu$Element),]
# reset rownames
rownames(eotu) <- NULL

elOxTable_121 <- eotu

usethis::use_data(elOxTable_121, overwrite = TRUE)
