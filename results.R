#-------------------------------------------
# 
# Dummymodel met vrij onzekere waarden
# Plaatjes
#
#-------------------------------------------
# Punten zijn ingelezen met http://arohatgi.info/WebPlotDigitizer/app/
# 

if (dir.exists("~/disks/y/ontwapps/Timer/Users/Stijn/Model/modelTt")) {     #Den Haag
  setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/modelTt")
} else if (dir.exists("~/Documenten/Stage PBL/modelTt")) {    #thuis
  setwd("~/Documenten/Stage PBL/modelTt")
}


source("TCRE-T2010HV.R")
source("TCRE+nonCO2.R")
source("TCRE+depnonCO2.R")


#--------- maak data per Ttarget ----------------

f.dataframe <- function(N,Ttarget,f.seed) {
  # maak samples
  cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
  # reken resultaten uit
  sample_en_result <- f.cumuCO2result(N,Ttarget,cumuvstemp.sample)
  return(sample_en_result)
}

N <- 10000
s.seed <- 21
data1 <- f.dataframe(N,1,s.seed)
data1.5 <- f.dataframe(N,1.5,s.seed)
data2 <- f.dataframe(N,2,s.seed)
data2.5 <- f.dataframe(N,2.5,s.seed)
data3 <- f.dataframe(N,3,s.seed)
data4 <- f.dataframe(N,4,s.seed)


#--------- bundel resultaten van het carbon budget voor verschillende Ttarget --------------

CO2.results <- NULL
for (i in seq(1, 4, by = 0.1)) {
  data <- f.dataframe(N,i,s.seed)
  CO2.results <- cbind(CO2.results, data$cumuCO2result)
}
colnames(CO2.results) <- as.character(seq(1, 4, by = 0.1))
CO2.results = data.table(CO2.results)


CO2.results <- NULL
for (i in c(1.5,2,3)) {
  data <- f.dataframe(N,i,s.seed)
  CO2.results <- cbind(CO2.results, data$cumuCO2result)
}
colnames(CO2.results) <- as.character(c(1.5,2,3))
CO2.results = data.table(CO2.results)


# maak histogrammen van result
hist(CO2.results)

#--------- bundel resultaten van de kosten voor verschillende Ttarget --------------

costs.results <- NULL
for (i in seq(1, 4, by = 0.1)) {
  data <- f.dataframe(N,i,s.seed)
  costs.results <- cbind(costs.results, data$costs)
}
colnames(costs.results) <- as.character(seq(1, 4, by = 0.1))
costs.results = data.table(costs.results)