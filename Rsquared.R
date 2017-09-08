#--------------------------------------------
#
# R-squared tabel
#
#--------------------------------------------

source("TCRE+SSPnonCO2.R")

CCmatNOcosts <- f.CCmatrix(N,s.seed)
#------------ Pearson
CCdataNOcostsP = data.table(CCmatNOcosts[[1]])

#kwadrateren
CCdataNOcostsP <- CCdataNOcostsP * CCdataNOcostsP

#Ttarget ervoor plakken
CCdataNOcostsP = cbind(as.character(seq(1, 4, by = 0.1)), CCdataNOcostsP)


# naar latex (gooi nog wel de eerste kolom weg!)
xtable(CCdataNOcostsP)

#---------- Spearman
CCdataNOcostsS = data.table(CCmatNOcosts[[2]])

#kwadrateren
CCdataNOcostsS <- CCdataNOcostsS * CCdataNOcostsS

#Ttarget ervoor plakken
CCdataNOcostsS = cbind(as.character(seq(1, 4, by = 0.1)), CCdataNOcostsS)


# naar latex (gooi nog wel de eerste kolom weg!)
xtable(CCdataNOcostsS)


#------------- Costs --------------------------
source("kostenbakjes.R")
source("kostenSSP.R")

CCmat <- f.costs.CCmatrix(N,s.seed)
#------------ Pearson
CCdataP = data.table(CCmat[[1]])

#kwadrateren
CCdataP <- CCdataP * CCdataP

#Ttarget ervoor plakken
CCdataP = cbind(as.character(seq(1.4, 3.4, by = 0.1)), CCdataP)


# naar latex (gooi nog wel de eerste kolom weg!)
xtable(CCdataP)


#---------- Spearman
CCdataS = data.table(CCmat[[2]])

#kwadrateren
CCdataS <- CCdataS * CCdataS

#Ttarget ervoor plakken
CCdataS = cbind(as.character(seq(1.4, 3.4, by = 0.1)), CCdataS)


# naar latex (gooi nog wel de eerste kolom weg!)
xtable(CCdataS)
