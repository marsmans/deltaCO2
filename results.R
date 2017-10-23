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
source("kostenbakjesAR5tran01toCosts.R")

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
source("kostenSSP.R")
source("kostenSSPandereindex.R")

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


#----- kostenbakjes met [0,1]transformatie -----

source("kostenbakjesAR5tran01toCosts.R")
source("kostenSSPanIndex01trans.R")

f.dataframekosten <- function(N,Ttarget,f.seed) {
  cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
  
  sample_en_result.deltaCO2 <- f.cumuCO2result(N,Ttarget,cumuvstemp.sample)
  
  kosten.result <- model.costs(sample_en_result.deltaCO2$cumuCO2result)
  
  # herleid costsensitivity
  #cs <-kosten.result/sample_en_result.deltaCO2$cumuCO2result
  
  sample_en_result.kosten <- data.frame(sample_en_result.deltaCO2,sampletrans01,kosten.result)
  
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

# met cs
deltaCO2 <- NULL
cs <- NULL
costs <- NULL
for (i in seq(1, 4, by = 0.1)) {
  data <- f.dataframekosten(N,i,s.seed)
  print(length(data$cumuCO2result))
  # print(length(data$cs))
  deltaCO2 <- cbind(deltaCO2, data$cumuCO2result)
  cs <- cbind(cs, data$cs)
  costs <- cbind(costs, data$kosten.result)
}
colnames(deltaCO2) <- as.character(seq(1, 4, by = 0.1))
colnames(cs) <- as.character(seq(1, 4, by = 0.1))
colnames(costs) <- as.character(seq(1, 4, by = 0.1))
deltaCO2 = data.table(deltaCO2)
cs = data.table(cs)
costs = data.table(costs)

# met sampletrans01
deltaCO2 <- NULL
trans01 <- NULL
costs <- NULL
for (i in seq(1, 4, by = 0.1)) {
  data <- f.dataframekosten(N,i,s.seed)
  print(length(data$cumuCO2result))
  # print(length(data$cs))
  deltaCO2 <- cbind(deltaCO2, data$cumuCO2result)
  trans01 <- cbind(trans01, data$sampletrans01)
  costs <- cbind(costs, data$kosten.result)
}
colnames(deltaCO2) <- as.character(seq(1, 4, by = 0.1))
colnames(trans01) <- as.character(seq(1, 4, by = 0.1))
colnames(costs) <- as.character(seq(1, 4, by = 0.1))
deltaCO2 = data.table(deltaCO2)
trans01 = data.table(trans01)
costs = data.table(costs)



# maak veel histogrammen:
hist(deltaCO2, breaks = "Scott")
hist(cs, breaks = "Scott")
hist(costs, breaks = "Scott")
hist(trans01, breaks = "Scott")

# maak veel scatterplots
# lukt nog niet :(

# 4 scatterplots costs vs cs
par(mfrow=c(2,2))
plot(costs$`1.3`~cs$`1.3`, ylab = "Costs", xlim = c(-0.5,100))
plot(costs$`2`~cs$`2`, ylab = "Costs")
plot(costs$`3`~cs$`3`, ylab = "Costs")
plot(costs$`3.4`~cs$`3.4`, ylab = "Costs")

# 4 scatterplots costs vs deltaCO2
par(mfrow=c(2,2))
plot(costs$`1.4`~deltaCO2$`1.4`, ylab = "Costs")
plot(costs$`2`~deltaCO2$`2`, ylab = "Costs")
plot(costs$`3`~deltaCO2$`3`, ylab = "Costs")
plot(costs$`3.4`~deltaCO2$`3.4`, ylab = "Costs")
par(mfrow=c(1,1))

# 4 scatterplots costs vs trans01
par(mfrow=c(2,2))
plot(costs$`1.4`~trans01$`1.4`, ylab = "Costs")
plot(costs$`2`~trans01$`2`, ylab = "Costs")
plot(costs$`3`~trans01$`3`, ylab = "Costs")
plot(costs$`3.4`~trans01$`3.4`, ylab = "Costs")
par(mfrow=c(1,1))


# scatterplots ggplot
data1.5 <- f.dataframekosten(N,1.5,s.seed)
data2 <- f.dataframekosten(N,2,s.seed)
data3 <- f.dataframekosten(N,3,s.seed)
colnames(data1.5) <- paste("1.5:",colnames(data1.5))
colnames(data2) <- paste("2:",colnames(data2))
colnames(data3) <- paste("3:",colnames(data3))

data1.523 <- cbind(data1.5,data2,data3)
data1.523 <- data.table(data1.523)

T20101.5 <- ggplot(data = data1.523)
T20101.5 <- T20101.5 + geom_point(aes(x = data1.523$`1.5: T2010`, y = data1.523$`1.5: kosten.result`))
T20101.5 <- T20101.5 + theme_bw()
T20101.5 <- T20101.5 + labs(x = expression(Temperature~relative~to~p.i.~( degree*C)), y = "Mitigation Costs (%GDP)")
T20101.5 <- T20101.5 + coord_cartesian(ylim = c(0, 6.5))
T20101.5

T20102 <- ggplot(data = data1.523)
T20102 <- T20102 + geom_point(aes(x = data1.523$`2: T2010`, y = data1.523$`2: kosten.result`))
T20102 <- T20102 + theme_bw()
T20102 <- T20102 + labs(x = expression(Temperature~relative~to~p.i.~( degree*C)), y = "Mitigation Costs (%GDP)")
T20102 <- T20102 + coord_cartesian(ylim = c(0, 6.5))
T20102

T20103 <- ggplot(data = data1.523)
T20103 <- T20103 + geom_point(aes(x = data1.523$`3: T2010`, y = data1.523$`3: kosten.result`))
T20103 <- T20103 + theme_bw()
T20103 <- T20103 + labs(x = expression(Temperature~relative~to~p.i.~( degree*C)), y = "Mitigation Costs (%GDP)")
T20103 <- T20103 + coord_cartesian(ylim = c(0, 6.5))
T20103

TCRE1.5 <- ggplot(data = data1.523)
TCRE1.5 <- TCRE1.5 + geom_point(aes(x = data1.523$`1.5: TCRE`, y = data1.523$`1.5: kosten.result`))
TCRE1.5 <- TCRE1.5 + theme_bw()
TCRE1.5 <- TCRE1.5 + labs(x = expression(TCRE~( degree*C/TtCO[2])), y = "Mitigation Costs (%GDP)")
TCRE1.5 <- TCRE1.5 + coord_cartesian(ylim = c(0, 6.5))
TCRE1.5

TCRE2 <- ggplot(data = data1.523)
TCRE2 <- TCRE2 + geom_point(aes(x = data1.523$`2: TCRE`, y = data1.523$`2: kosten.result`))
TCRE2 <- TCRE2 + theme_bw()
TCRE2 <- TCRE2 + labs(x = expression(TCRE~( degree*C/TtCO[2])), y = "Mitigation Costs (%GDP)")
TCRE2 <- TCRE2 + coord_cartesian(ylim = c(0, 6.5))
TCRE2

TCRE3 <- ggplot(data = data1.523)
TCRE3 <- TCRE3 + geom_point(aes(x = data1.523$`3: TCRE`, y = data1.523$`3: kosten.result`))
TCRE3 <- TCRE3 + theme_bw()
TCRE3 <- TCRE3 + labs(x = expression(TCRE~( degree*C/TtCO[2])), y = "Mitigation Costs (%GDP)")
TCRE3 <- TCRE3 + coord_cartesian(ylim = c(0, 6.5))
TCRE3

FnCO21.5 <- ggplot(data = data1.523)
FnCO21.5 <- FnCO21.5 + geom_point(aes(x = data1.523$`1.5: nonCO2`, y = data1.523$`1.5: kosten.result`))
FnCO21.5 <- FnCO21.5 + theme_bw()
FnCO21.5 <- FnCO21.5 + labs(x = expression(Temperature~relative~to~p.i.~( degree*C)), y = "Mitigation Costs (%GDP)")
FnCO21.5 <- FnCO21.5 + coord_cartesian(ylim = c(0, 6.5))
FnCO21.5

FnCO22 <- ggplot(data = data1.523)
FnCO22 <- FnCO22 + geom_point(aes(x = data1.523$`1.5: nonCO2`, y = data1.523$`2: kosten.result`))
FnCO22 <- FnCO22 + theme_bw()
FnCO22 <- FnCO22 + labs(x = expression(Temperature~relative~to~p.i.~( degree*C)), y = "Mitigation Costs (%GDP)")
FnCO22 <- FnCO22 + coord_cartesian(ylim = c(0, 6.5))
FnCO22

FnCO23 <- ggplot(data = data1.523)
FnCO23 <- FnCO23 + geom_point(aes(x = data1.523$`1.5: nonCO2`, y = data1.523$`3: kosten.result`))
FnCO23 <- FnCO23 + theme_bw()
FnCO23 <- FnCO23 + labs(x = expression(Temperature~relative~to~p.i.~( degree*C)), y = "Mitigation Costs (%GDP)")
FnCO23 <- FnCO23 + coord_cartesian(ylim = c(0, 6.5))
FnCO23

t1.5 <- ggplot(data = data1.523)
t1.5 <- FnCO21.5 + geom_point(aes(x = data1.523$`1.5: sampletrans01`, y = data1.523$`1.5: kosten.result`))
t1.5 <- FnCO21.5 + theme_bw()
t1.5 <- FnCO21.5 + labs(x = "t", y = "Mitigation Costs (%GDP)")
t1.5 <- FnCO21.5 + coord_cartesian(ylim = c(0, 6.5))
t1.5

t2 <- ggplot(data = data1.523)
t2 <- t2 + geom_point(aes(x = data1.523$`1.5: sampletrans01`, y = data1.523$`2: kosten.result`))
t2 <- t2 + theme_bw()
t2 <- t2 + labs(x = "t", y = "Mitigation Costs (%GDP)")
t2 <- t2 + coord_cartesian(ylim = c(0, 6.5))
t2

t3 <- ggplot(data = data1.523)
t3 <- t3 + geom_point(aes(x = data1.523$`1.5: sampletrans01`, y = data1.523$`3: kosten.result`))
t3 <- t3 + theme_bw()
t3 <- t3 + labs(x = "t", y = "Mitigation Costs (%GDP)")
t3 <- t3 + coord_cartesian(ylim = c(0, 6.5))
t3

source("multiplot.R")
# 3x4
multiplot(T20101.5, T20102, T20103, TCRE1.5, TCRE2, TCRE3, FnCO21.5, FnCO22, FnCO23, t1.5, t2, t3, layout = matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), nrow=3))
# 4x3
multiplot(T20101.5, T20102, T20103, TCRE1.5, TCRE2, TCRE3, FnCO21.5, FnCO22, FnCO23, t1.5, t2, t3, layout = matrix(c(1,4,7,10,2,5,8,11,3,6,9,12), nrow=4))




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