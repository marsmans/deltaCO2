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
source("TCRE+denonCO2skewed.R")
source("TCRE+SSPnonCO2.R")
source("kostenbakjes.R")
source("kostenbakjesDeterministisch.R")
source("kostenSSP.R")

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
data2010 <- f.dataframe(N,T2010mean,s.seed)
data1 <- f.dataframe(N,1,s.seed)
data1.5 <- f.dataframe(N,1.5,s.seed)
data2 <- f.dataframe(N,2,s.seed)
data2.4 <- f.dataframe(N,2.4,s.seed)
data2.5 <- f.dataframe(N,2.5,s.seed)
data3 <- f.dataframe(N,3,s.seed)
data4 <- f.dataframe(N,4,s.seed)


#--------- bundel resultaten van het carbon budget voor verschillende Ttarget --------------

CO2.results <- NULL
nonCO2.sample <- NULL
for (i in seq(1, 4, by = 0.1)) {
  data <- f.dataframe(N,i,s.seed)
  nonCO2.sample <- cbind(nonCO2.sample, data$nonCO2*(data$cumuCO2result + a.nonCO2))
  CO2.results <- cbind(CO2.results, data$cumuCO2result)
}
colnames(nonCO2.sample) <- as.character(seq(1, 4, by = 0.1))
colnames(CO2.results) <- as.character(seq(1, 4, by = 0.1))
nonCO2.sample = data.table(nonCO2.sample)
CO2.results = data.table(CO2.results)


CO2.results1.523 <- NULL
T2010.sample <- NULL
TCRE.sample <- NULL
nonCO2.sample <- NULL
for (i in c(1.5,2,3)) {
  data <- f.dataframe(N,i,s.seed)
  T2010.sample <- cbind(T2010.sample, data$T2010)
  TCRE.sample <- cbind(TCRE.sample, data$TCRE)
  # constant:
  #nonCO2.sample <- cbind(nonCO2.sample, data$nonCO2)
  # niet skewed:
  nonCO2.sample <- cbind(nonCO2.sample, data$nonCO2*(data$cumuCO2result + a.nonCO2))
  # skewed:
  #nonCO2.sample <- cbind(nonCO2.sample, data$TCRnonCO2*data$cumuCO2result + data$nonCO2int)
  CO2.results1.523 <- cbind(CO2.results1.523, data$cumuCO2result)
}
colnames(CO2.results1.523) <- as.character(c(1.5,2,3))
colnames(T2010.sample) <- as.character(c(1.5,2,3))
colnames(TCRE.sample) <- as.character(c(1.5,2,3))
colnames(nonCO2.sample) <- as.character(c(1.5,2,3))
CO2.results1.523 = data.table(CO2.results1.523)
T2010.sample = data.table(T2010.sample)
TCRE.sample = data.table(TCRE.sample)
nonCO2.sample = data.table(nonCO2.sample)


summary(CO2.results1.523)

# maak histogrammen van result
hist(CO2.results1.523)

# histogrammen van samples
hist(T2010.sample)
hist(TCRE.sample)
hist(nonCO2.sample)



#-------------- resultaten kostenbakjes ---------------------

Ttarget <- 1.3
sample_kosten <- f.dataframe.kosten(N,Ttarget,s.seed)

remove <- c(-1)

waarZitMin1 <- which(sample_kosten$kosten.result %in% remove)

kosten.result_zonderbuitenbeentjes <- sample_kosten$kosten.result [! sample_kosten$kosten.result %in% remove]

hist(kosten.result_zonderbuitenbeentjes, breaks = "Scott", main = Ttarget, xlab = "kosten (% baseline GDP)")


#------------- resultaten kostenbakjes lineair tussen bakjes -----

source("kostenbakjesDeterministisch.R")

f.dataframekosten <- function(N,Ttarget,f.seed) {
  cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
  
  sample_en_result.deltaCO2 <- f.cumuCO2result(N,Ttarget,cumuvstemp.sample)
  
  kosten.result <- model.costs(sample_en_result.deltaCO2$cumuCO2result)
  
  sample_en_result.kosten <- data.frame(sample_en_result.deltaCO2,kosten.result)
  
  # verwijder resultaten buiten bakjes
  remove <- c(-1)
  aantalMin1 <- vector(mode="numeric", length=0)
  waarZitMin1 <- which(sample_en_result.kosten$kosten.result %in% remove)
  aantalMin1 <- c(aantalMin1, length(waarZitMin1))
  
  if (!identical(waarZitMin1, integer(0))) {
    sample_en_result.kosten <- sample_en_result.kosten[-waarZitMin1,]
  }
  
  return(sample_en_result.kosten)
}

#----- kostenbakjes ---

source("kostenbakjes.R")

f.dataframekosten <- function(N,Ttarget,f.seed) {
  cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
  
  sample_en_result.deltaCO2 <- f.cumuCO2result(N,Ttarget,cumuvstemp.sample)
  
  kosten.result <- model.costs(sample_en_result.deltaCO2$cumuCO2result)
  
  # herleid costsensitivity
  cs <-kosten.result/sample_en_result.deltaCO2$cumuCO2result
  
  sample_en_result.kosten <- data.frame(sample_en_result.deltaCO2,cs,kosten.result)
  
  # verwijder resultaten buiten bakjes
  #remove <- c(-1)
  #aantalMin1 <- vector(mode="numeric", length=0)
  #waarZitMin1 <- which(sample_en_result.kosten$kosten.result %in% remove)
  #aantalMin1 <- c(aantalMin1, length(waarZitMin1))
  
  #if (!identical(waarZitMin1, integer(0))) {
  #  sample_en_result.kosten <- sample_en_result.kosten[-waarZitMin1,]
  #}
  
  return(sample_en_result.kosten)
}


#------------- bundel resultaten kosten ---------------------

deltaCO2 <- NULL
cs <- NULL
costsIPCC <- NULL
for (i in seq(1, 4, by = 0.1)) {
  data <- f.dataframekosten(N,i,s.seed)
  print(length(data$cumuCO2result))
  # print(length(data$cs))
  deltaCO2 <- cbind(deltaCO2, data$cumuCO2result)
  cs <- cbind(cs, data$cs)
  costsIPCC <- cbind(costsIPCC, data$kosten.result)
}
colnames(deltaCO2) <- as.character(seq(1, 4, by = 0.1))
colnames(cs) <- as.character(seq(1, 4, by = 0.1))
colnames(costsIPCC) <- as.character(seq(1, 4, by = 0.1))
deltaCO2 = data.table(deltaCO2)
cs = data.table(cs)
costsIPCC = data.table(costsIPCC)


# maak veel histogrammen:
hist(deltaCO2, breaks = "Scott")
hist(cs, breaks = "Scott")
hist(costsIPCC, breaks = "Scott")

# maak veel scatterplots
# lukt nog niet :(
par(mfrow=c(2,2))
plot(costsIPCC$`1.3`~cs$`1.3`)
plot(costsIPCC$`2`~cs$`2`)
plot(costsIPCC$`3`~cs$`3`)
plot(costsIPCC$`3.4`~cs$`3.4`)

plot(costsIPCC$`1.3`~deltaCO2$`1.3`)
plot(costsIPCC$`2`~deltaCO2$`2`)
plot(costsIPCC$`3`~deltaCO2$`3`)
plot(costsIPCC$`3.4`~deltaCO2$`3.4`)
par(mfrow=c(1,1))


# oud:

deltaCO2.results <- NULL
costsIPCC.result <- NULL
for (i in seq(1, 4, by = 0.1)) {
  data <- f.dataframe.kosten(N,i,s.seed)
  deltaCO2.results <- cbind(deltaCO2.results, data$cumuCO2result)
  costsIPCC.result <- cbind(costsIPCC.result, data$kosten.result)
}
colnames(deltaCO2.results) <- as.character(seq(1, 4, by = 0.1))
colnames(costsIPCC.result) <- as.character(seq(1, 4, by = 0.1))
deltaCO2.results = data.table(deltaCO2.results)
costsIPCC.result = data.table(costsIPCC.result)



#--------- bundel resultaten van de kosten voor verschillende Ttarget --------------

costs.results <- NULL
for (i in seq(1, 4, by = 0.1)) {
  data <- f.dataframe(N,i,s.seed)
  costs.results <- cbind(costs.results, data$costs)
}
colnames(costs.results) <- as.character(seq(1, 4, by = 0.1))
costs.results = data.table(costs.results)