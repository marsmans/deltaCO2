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

# Den Haag
setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/modelTt")
# Thuis
#setwd("~/Documenten/Stage PBL/modelTt")

source("TCRE.R")
#source("costs.R")


source("TCRE+depnonCO2.R")
source("TCRE+SSPnonCO2.R")

#------------ grafieken ------------------

# plaatje van TCRE en hoe de lijnen zijn gefit
par(mfrow=c(2,2))
plot(cumuvstempLL,xlim=c(0,9), ylim=c(0,5))
abline(fLL)
points(cumuvstempUL,xlim=c(0,9))
abline(fUL)
abline(intercept, slope)







# plaatje van costs en hoe de lijnen zijn gefit (lineaire geval)
par(mfrow=c(1,1))
plot(costsLL,xlim=c(0,9),ylim=c(0,8))
abline(gLL)
points(costsUL)
abline(gUL)
abline(intercept_mean, slope_mean)

# plaatje van CO2.results
CO2.results <- data.table(CO2.results)
CO2.results2 <-gather(CO2.results,temp,cumuCO2,as.character(seq(1, 4, by = 0.1)))
par(mfrow=c(1,1))
plot(CO2.results2$temp~CO2.results2$cumuCO2,xlim=c(0,9), ylim=c(0,4), xlab = "cumu CO2 (2010-2100) (TtCO2)", ylab = "temperature change relative to pi (*C)")


# plaatje van nonCO2 sample
unlnonCO2 <- unlist(nonCO2.sample)
unlCO2.result <- unlist(CO2.results)

plot(unlnonCO2~unlCO2.result, main = "nonCO2 sample", xlim = c(-0.3,7), ylim = c(-0.35,0.35), xlab = "cumu CO2 (2010-2100) (TtCO2)", ylab = "temperature change relative to pi (*C)")


# plaatje van CO2.results met nonCO2sample
O2.results <- data.table(CO2.results)
CO2.results2 <-gather(CO2.results,temp,cumuCO2,as.character(seq(1, 4, by = 0.1)))
par(mfrow=c(1,1))
plot(CO2.results2$temp~CO2.results2$cumuCO2,xlim=c(-0.3,8), ylim=c(-0.35,4), xlab = "cumu CO2 (2010-2100) (TtCO2)", ylab = "temperature change relative to pi (*C)")
points(unlnonCO2~unlCO2.result)


# plaatje van non-CO2 ricos
cumuCO22010forupperboundsfixed <- (nonCO2temp$cumuCO2 - cumuCO22010.rico)
plot(nonCO2.upperbounds~cumuCO22010forupperboundsfixed, xlim=c(0,8), ylim=c(0,0.3),xlab = "cumuCO2 (2010-2100) (TtCO2)", ylab = "Temperature change relative to pi (*C)")
abline(a=nonCO22010max,b=nonCO2ricos[1])
abline(a=nonCO22010max,b=nonCO2ricos[2])
abline(a=nonCO22010max,b=nonCO2ricos[3])
abline(a=nonCO22010max,b=nonCO2ricos[4])
abline(a=nonCO22010max,b=nonCO2ricos[5])
abline(a=nonCO22010max,b=nonCO2ricos[6])
# gemiddelde (zonder rechte lijn)
abline(a=nonCO22010max,b=TCRnonCO2max, col = "blue")
abline(a=nonCO22010max,b=TCRnonCO2max2, col = "green")


# plaatje van SSPdata curves om bakjes te corrigeren
plot(kostenSSP.indexed$Cost.Estimate..ktrillion.~kostenSSP.indexed$Cum.CO2, xlab = "delta CO2 (Tt)", ylab = "indexed costs", pch = 16, col = "blue", xlim = c(0,6))
points(kostenSSP.indexed$MAC.Costs..ktrillion.~kostenSSP.indexed$Cum.CO2, pch = 17, col = "green" )
points(kostenSSP.indexed$Consumption.Loss..ktrillion.~kostenSSP.indexed$Cum.CO2, pch = 18, col = "red" )
curve(143.02*exp(-1.415*x), add = T)
curve(13.373*exp(-0.804*x), add = T)
curve(36.598*exp(-1.054*x), add = T)
# text(locator(), labels = c("143.02*exp(-1.415*x)", "13.373*exp(-0.804*x)", "36.598*exp(-1.054*x)"))
text(locator(), labels = c("y1", "y2", "y3"))

# plaatje nieuw index
plot(kostenSSP.indexed$Cost.Estimate..ktrillion.~kostenSSP.indexed$Cum.CO2, xlab = "delta CO2 (Tt)", ylab = "indexed costs", pch = 16, col = "blue", xlim = c(0,6))
points(kostenSSP.indexed$MAC.Costs..ktrillion.~kostenSSP.indexed$Cum.CO2, pch = 17, col = "green" )
points(kostenSSP.indexed$Consumption.Loss..ktrillion.~kostenSSP.indexed$Cum.CO2, pch = 18, col = "red" )
curve(2.7593*exp(-1.884*x), add = T)
curve(3.6792*exp(-0.968*x), add = T)
curve(28.84*exp(-0.986*x), add = T)


kostenSSP.ind.gather <- gather(kostenSSP.indexed, variable, value)
kostenSSP.ind.gather$cumuCO2 <- kostenSSP.indexed$Cum.CO2
kostenSSP.ind.gather <- data.table(kostenSSP.ind.gather)

s <- ggplot(kostenSSP.ind.gather[variable %in% c('Cost.Estimate..ktrillion.','MAC.Costs..ktrillion.','Consumption.Loss..ktrillion.')])
s = s + geom_point(aes(x=cumuCO2,y=value),stat="identity")
s = s + theme_bw()
#s = s + scale_color_manual(values=c("Cost.Estimate..ktrillion."="blue","MAC.Costs..ktrillion."="green", "Consumption.Loss..ktrillion."="red"),
#s = s + scale_shape_manual(values=c("tempStijging_door_F_nonCO2"=1,"tempStijging_door_F_nonCO2.met.avg"=4), #if you want shapes
#                           labels=c("Carbon price total costs","Area under MAC curve","Consumption loss"))
s = s + guides(col=guide_legend(title=NULL))
s = s + theme(legend.justification=c(0.9,0.65), legend.position=c(0.9,0.65), 
              legend.text = element_text(size = 12))
s = s + labs(x = expression(Cumulative~carbon~emissions~(2010-2100)~(TtCO[2])), y = expression(Cost~index~(1.3~TtCO[2]==1)))
s = s + coord_cartesian(xlim = c(0,5)) #, ylim = c(0,10))
s = s + stat_function(fun=function(x)2.7593*exp(-1.884*x), geom="line", aes(colour="minimum"))
s = s + stat_function(fun=function(x)3.6792*exp(-0.968*x), geom="line", aes(colour="median"))
s = s + stat_function(fun=function(x)28.84*exp(-0.986*x), geom="line", aes(colour="maximum"))
#s = s + scale_colour_manual("Function", value=c("blue","red"), breaks=c("min","med"))
s



# plaatje van deltaCO2 vs kosten, oud
deltaCO2.results <- data.table(deltaCO2.results)
costsIPCC.result <- data.table(costsIPCC.result)

deltaCO2.results2 <-gather(deltaCO2.results,temp,cumuCO2,as.character(seq(1, 4, by = 0.1)))
costsIPCC.result2 <- gather(costsIPCC.result,temp,kosten,as.character(seq(1, 4, by = 0.1)))

par(mfrow=c(1,1))
plot(costsIPCC.result2$kosten~deltaCO2.results2$cumuCO2,xlim=c(0,7), ylim=c(-1.2,4.5), xlab = "cumu CO2 (2010-2100) (TtCO2)", ylab = "abatement costs (%GDP)")


# nieuwe versie
# plaatje van deltaCO2 vs kosten
deltaCO2 <- data.table(deltaCO2)
costs <- data.table(costs)

deltaCO22 <-gather(deltaCO2,temp,cumuCO2,as.character(seq(1, 4, by = 0.1)))
costs2 <- gather(costs,temp,kosten,as.character(seq(1, 4, by = 0.1)))

deltaCO2Costs2 <- cbind(deltaCO22, kosten = costs2$kosten)

par(mfrow=c(1,1))
plot(costs2$kosten~deltaCO22$cumuCO2,xlim=c(-1,7), ylim=c(-0.5,12), xlab = "cumu CO2 (2010-2100) (TtCO2)", ylab = "abatement costs (%GDP)")

p = ggplot(deltaCO2Costs2,aes(x=cumuCO2,y=kosten))
p = p + geom_point(alpha = 0.03)
p = p + theme_bw()
p = p + labs(x = "deltaCO2", y = "Costs") 
p


# plaatje van Ttarget vs kosten
deltaCO2 <- data.table(deltaCO2)
costsIPCC <- data.table(costsIPCC)

deltaCO22 <-gather(deltaCO2,temp,cumuCO2,as.character(seq(1, 4, by = 0.1)))
costsIPCC2 <- gather(costsIPCC,temp,kosten,as.character(seq(1, 4, by = 0.1)))

par(mfrow=c(1,1))
plot(costs2$kosten~costs2$temp,xlim=c(-1,7), ylim=c(-0.5,4.5), xlab = "cumu CO2 (2010-2100) (TtCO2)", ylab = "abatement costs (%GDP)")

p = ggplot(costs2,aes(x=temp,y=kosten))
p = p + geom_point(alpha = 0.03)
p = p + geom_smooth(method = lm)
p = p + theme_bw()
p = p + labs(x = "Ttarget", y = "Costs") 
p = p + coord_cartesian(xlim = c(0,32), ylim = c(0, 13))
p


#----------- histogrammen --------------

N <- 10000
f.seed <- 21

# maak samples
cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
p.sample <- f.p.sample(N, f.seed)


# histogram van input
par(mfrow=c(2,2))
hist(cumuvstemp.sample$T2010, breaks = "Scott", main = "Histogram of T2010", xlab = "degrees Celsius, relative to pre-industrial")
hist(cumuvstemp.sample$TCRE, breaks = "Scott", main = "Histogram of TCRE", xlab = "TCRE (degrees Celsius/TtCO2)")
hist(cumuvstemp.sample$CO22010, breaks = "Scott", main = "Histogram of CO22010", xlab = "Tt cumuCO2, relative to pre-industrial")
par(mfrow=c(1,1))

# costs
par(mfrow=c(2,2))
hist(costs.sample$costs.slope, breaks = "Scott", main = "Histogram of costs.slope", xlab = "cost respons to emissions (%pt/tCO2)")
hist(costs.sample$baselineCO2, breaks = "Scott", main = "Histogram of baseline CO2", xlab = "Baseline CO2")
par(mfrow=c(1,1))

# costs nonlineair
hist(p.sample, breaks = "Scott", main = "Histogram of p", xlab = "response parameter")



#maak resultaten
#sample_en_result <- f.cumuCO2result(N,1.5,cumuvstemp.sample)
#costs.sample_en_result <- data.frame(sample_en_result,f.costsresult(N,sample_en_result[,5],costs.sample))


# functies die een histogram van de cumuCO2 resultaten maken gegeven een dataframe
hist.cumuCO2result <- function(f.data) {
  hist(f.data$cumuCO2result, breaks = "Scott", main = paste("CO2result, Ttarget = ", f.data$f.Ttarget[1]) , xlab = "cumuCO2 (Tt)")
}

hist.costs <- function(f.data) {
  hist(f.data$costs, breaks = "Scott", main = paste("Costs, Ttarget = ", f.data$f.Ttarget[1]) , xlab = "Costs")
}

hist.T2010 <- function(f.data) {
  hist(f.data$T2010, breaks = "Scott", main="T2010 sample", xlab = "T2010")
}

hist.TCRE <- function(f.data) {
  hist(f.data$TCRE, breaks = "Scott", main="TCRE sample", xlab = "TCRE")
}

hist.nonCO2 <- function(f.data) {
  hist(f.data$nonCO2, breaks = "Scott", main= "nonCO2 sample", xlab = "nonCO2")
}


hist.slope <- function(f.data) {
  plot(f.data$costs.slope, main=paste(f.data$f.Ttarget[1]), sub = "costs.slope", xlab = "costs.slope", ylab = "costs")
}

hist.p <- function(f.data) {
  plot(f.data$p.sample, main=paste(f.data$f.Ttarget[1]), sub = "p", xlab = "p", ylab = "costs")
}


hist.cs <- function(f.data) {
  plot(f.data$cs, main=paste(f.data$f.Ttarget[1]), sub = "cs", xlab = "cs", ylab = "costs")
}




# scatterplots maken van een dataframe
scatter.TCRE <- function(f.data) {
  plot(f.data$costs~f.data$TCRE, main=paste(f.data$f.Ttarget[1]), sub = "TCRE", xlab = "TCRE", ylab = "costs", xlim=c(0.2,1), ylim=c(-6,4))
}

scatter.slope <- function(f.data) {
  plot(f.data$costs~f.data$costs.slope, main=paste(f.data$f.Ttarget[1]), sub = "costs.slope", xlab = "costs.slope", ylab = "costs", xlim=c(-1.2,-0.3),ylim=c(-6,4))
}

scatter.p <- function(f.data) {
  plot(f.data$costs~f.data$p.sample, main=paste(f.data$f.Ttarget[1]), sub = "p", xlab = "p", ylab = "costs", xlim=c(0.75,1),ylim=c(0,4))
}

scatter.cumuCO2result <- function(f.data) {
  plot(f.data$costs~f.data$cumuCO2result, main=paste(f.data$f.Ttarget[1]), sub = "cumuCO2result", xlab = "cumuCO2result", ylab = "costs", xlim=c(0,7),ylim=c(-8,8))
}


#----------- main plot -----------------
# krijgt een CC matrix
CCmat <- f.costs.CCmatrix(N,s.seed)
CCdata = data.table(CCmat)
# maak er een 'werkbaarder' format van
CC <-gather(CCdata,variable,value,c('T2010','TCRE','nonCO2','cumuCO2result','costs.slope'))
CC=data.table(CC)
CC$temp <- as.character(seq(1, 4, by = 0.1))


#----------- plot van CC waarden (lin) met costs.slope --------------


# plotting (probeersel) staven naast elkaar
p = ggplot(CC[variable %in% c('T2010','TCRE','costs.slope')])
p = p + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="dodge")
p = p + theme_bw()# + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("cumuCO2result"="dark blue","costs.slope"="dark red","T2010"="black","TCRE"="green"))
p
ggsave(paste("CC_GE_lin.png"),p)

# plotting (probeersel) staven op elkaar
# eerst alle getallen positief maken (door te kwadrateren)
CC$value <- CC$value*CC$value

p = ggplot(CC[variable %in% c('T2010','TCRE','nonCO2','costs.slope')])
p = p + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="dodge")
p = p + theme_bw()# + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("cumuCO2result"="dark blue","costs.slope"="dark red","T2010"="black","TCRE"="green","nonCO2"="blue"))
p = p + ggtitle("CC values of GE models with linear relation and fitted cost graph and bounded TCRE")
p
ggsave(paste("CC_GE_lin_stacked_squared_withcor.png"),p)

# plotting (probeersel) staven op elkaar
# eerst alle getallen positief maken (door absolute waarde te nemen)
CC$value <- abs(CC$value)

p = ggplot(CC[variable %in% c('T2010','CO22010','TCRE','costs.slope')])
p = p + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="fill")
p = p + theme_bw()# + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("CO22010"="grey","cumuCO2result"="dark blue","costs.slope"="dark red","T2010"="black","TCRE"="green"))
p
ggsave(paste("CC_GE_lin_stacked_absolute.png"),p)


#----------- plot van CC waarden (lin) zonder costs.slope --------------

#==========> deze moeten we hebben voor cc tussen deltaCO2 en input
# MET NONCO2

source("TCRE+nonCO2.R")
source("TCRE+depnonCO2.R")
source("TCRE+depnonCO2+SCC.R")
source("TCRE+SSPnonCO2.R")

# krijgt een CC matrix
CCmatNOcosts <- f.CCmatrix(N,s.seed)
CCdataNOcosts = data.table(CCmatNOcosts[[1]])
# maak er een 'werkbaarder' format van
CCNOcosts <-gather(CCdataNOcosts,variable,value,c('T2010','TCRE','nonCO2'))
CCNOcosts=data.table(CCNOcosts)
CCNOcosts$temp <- as.character(seq(1, 4, by = 0.1))

# plotting (probeersel) staven op elkaar
# eerst alle getallen positief maken (door te kwadrateren)
CCNOcosts$value <- CCNOcosts$value*CCNOcosts$value

q = ggplot(CCNOcosts[variable %in% c('T2010','TCRE','nonCO2')])
q = q + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="fill")
q = q + theme_bw()# + theme(axis.text.x=element_text(size=12))
q = q + scale_fill_manual(values=c("CO22010"="grey","cumuCO2result"="dark blue","T2010"="black","TCRE"="green","nonCO2"="blue"))
q = q + ggtitle("CC values of T2010, TCRE and nonCO2")
q

ggsave(paste("CC_T2010_TCRE_nonCO2.png"),q)



# met nonCO2 = nonCO2int + deltaCO2 * TCRnonCO2

source("TCRE+denonCO2skewed.R")

# krijgt een CC matrix
CCmatNOcosts <- f.CCmatrix(N,s.seed)
CCdataNOcosts = data.table(CCmatNOcosts)
# maak er een 'werkbaarder' format van
CCNOcosts <-gather(CCdataNOcosts,variable,value,c('T2010','TCRE','TCRnonCO2','nonCO2int'))
CCNOcosts=data.table(CCNOcosts)
CCNOcosts$temp <- as.character(seq(1, 4, by = 0.1))

# plotting (probeersel) staven op elkaar
# eerst alle getallen positief maken (door te kwadrateren)
CCNOcosts$value <- CCNOcosts$value*CCNOcosts$value

q = ggplot(CCNOcosts[variable %in% c('T2010','TCRE','TCRnonCO2','nonCO2int')])
q = q + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="fill")
q = q + theme_bw()# + theme(axis.text.x=element_text(size=12))
q = q + scale_fill_manual(values=c("CO22010"="grey","cumuCO2result"="dark blue","T2010"="black","TCRE"="green","TCRnonCO2"="blue","nonCO2int"="pink"))
q = q + ggtitle("CC values of T2010 (HV), TCRE (bounded) and nonCO2 (bounded and depending)")
q
ggsave(paste("CC_T2010_TCRE_nonCO2.png"),q)


# ZONDER non CO2

source("TCRE-T2010HV.R")

# krijgt een CC matrix
CCmatNOcosts <- f.CCmatrix(N,s.seed)
CCdataNOcosts = data.table(CCmatNOcosts)
# maak er een 'werkbaarder' format van
CCNOcosts <-gather(CCdataNOcosts,variable,value,c('T2010','TCRE'))
CCNOcosts=data.table(CCNOcosts)
CCNOcosts$temp <- as.character(seq(1, 4, by = 0.1))

# plotting (probeersel) staven op elkaar
# eerst alle getallen positief maken (door te kwadrateren)
CCNOcosts$value <- CCNOcosts$value*CCNOcosts$value

q = ggplot(CCNOcosts[variable %in% c('T2010','TCRE')])
q = q + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="fill")
q = q + theme_bw()# + theme(axis.text.x=element_text(size=12))
q = q + scale_fill_manual(values=c("cumuCO2result"="dark blue","T2010"="black","TCRE"="green"))
q = q + ggtitle("CC values of T2010 (HV) and TCRE (bounded)")
q
ggsave(paste("CC_T2010_TCRE.png"),q)




#==========> deze moeten we hebben voor CC van inputparameters en kosten
#----------- plot van CC waarden met nonCO2 en kostenbakjes IPCC en kostenbakjes SSP --------------


source("TCRE+SSPnonCO2.R")

source("kostenbakjes.R")
source("kostenbakjesDeterministisch.R")

source("kostenSSP.R")
source("kostenSSPandereindex.R")
source("kostenbakjesAR5tran01toCosts.R")


# krijgt een CC matrix
s.seed <- 21
CCmat <- f.costs.CCmatrix(N,s.seed)
CCdata = data.table(CCmat[[2]])
# maak er een 'werkbaarder' format van
CC <-gather(CCdata,variable,value,c('T2010','TCRE','nonCO2','cumuCO2result','cs'))
CC=data.table(CC)
CC$Ttarget <- as.character(seq(1.4, 3.4, by = 0.1))

# plotting (probeersel) staven naast elkaar
p = ggplot(CC[variable %in% c('T2010','TCRE','nonCO2','cumuCO2result','cs')])
p = p + geom_bar(aes(x=Ttarget,y=value,fill=variable),stat="identity",position="dodge") #position="dodge"
p = p + theme_bw()# + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("cumuCO2result"="dark blue","cs"="dark red","T2010"="black","TCRE"="green", "nonCO2"="blue"))
p = p + ggtitle("CC values (Pearson rank) for costs, SSP-data")
p
# ggsave(paste("CC_GE_lin.png"),p)


# plotting (probeersel) staven op elkaar
# eerst alle getallen positief maken (door te kwadrateren)
CC$value <- CC$value*CC$value

q = ggplot(CC[variable %in% c('T2010','TCRE','nonCO2','cs')]) # c('cumuCO2result','cs')]) # 'cumuCO2result' 
q = q + geom_bar(aes(x=Ttarget,y=value,fill=variable),stat="identity",position="fill")
q = q + theme_bw()# + theme(axis.text.x=element_text(size=12))
q = q + scale_fill_manual(values=c("cumuCO2result"="dark blue","cs"="dark red","T2010"="black","TCRE"="green","nonCO2"="blue"))
q = q + ggtitle("CC values costs, SSP-data")
q

# ggsave(paste("CC.costs_T2010_TCRE_nonCO2_deltaCO2.png"),q)

#---------- probeersel met sampletrans01

source("kostenbakjesAR5tran01toCosts.R")
source("kostenSSPanIndex01trans.R")

# krijgt een CC matrix
s.seed <- 21
CCmat <- f.costs.CCmatrix(N,s.seed)
CCdata = data.table(CCmat[[2]])
# maak er een 'werkbaarder' format van
CC <-gather(CCdata,variable,value,c('T2010','TCRE','nonCO2','cumuCO2result','sampletrans01'))
CC=data.table(CC)
CC$Ttarget <- as.character(seq(1.5, 4, by = 0.1))

# plotting (probeersel) staven naast elkaar
p = ggplot(CC[variable %in% c('T2010','TCRE','nonCO2','cumuCO2result','sampletrans01')])
p = p + geom_bar(aes(x=Ttarget,y=value,fill=variable),stat="identity",position="dodge") #position="dodge"
p = p + theme_bw()# + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("cumuCO2result"="dark blue","cs"="dark red","T2010"="black","TCRE"="green", "nonCO2"="blue", "sampletrans01"="orange"))
p = p + ggtitle("SRCC for costs, AR5-data")
p
# ggsave(paste("CC_GE_lin.png"),p)


# plotting (probeersel) staven op elkaar
# eerst alle getallen positief maken (door te kwadrateren)
CC$value <- CC$value*CC$value

q = ggplot(CC[variable %in% c('T2010','TCRE','nonCO2','sampletrans01')]) # c('cumuCO2result','cs')]) # 'cumuCO2result' 
q = q + geom_bar(aes(x=Ttarget,y=value,fill=variable),stat="identity",position="fill")
q = q + theme_bw()# + theme(axis.text.x=element_text(size=12))
q = q + scale_fill_manual(values=c("cumuCO2result"="dark blue","cs"="dark red","T2010"="black","TCRE"="green", "nonCO2"="blue", "sampletrans01"="orange"),
                          name="Parameter",
                          labels=c("TFnonCO2", "t","T2010","TCRE"))
q = q + ggtitle("SRCC, SSP-data")
q = q + labs(x = "T2100", y = expression(r[S]^{2}))
q

# ggsave(paste("CC.costs_T2010_TCRE_nonCO2_deltaCO2.png"),q)


# ------- plot van CC waarden tussen cs en andere input
source("kostenbakjes.R")
source("kostenSSP.R")

# krijgt een CC matrix
s.seed <- 21
CCmatcs <- f.cs.CCmatrix(N,s.seed)
CCdatacs = data.table(CCmatcs[[2]])
# maak er een 'werkbaarder' format van
CC <-gather(CCdatacs,variable,value,c('T2010','TCRE','nonCO2','cumuCO2result','kosten.result'))
CC=data.table(CC)
CC$Ttarget <- as.character(seq(1.4, 3.4, by = 0.1))

# plotting (probeersel) staven naast elkaar
p = ggplot(CC[variable %in% c('T2010','TCRE','nonCO2','cumuCO2result','kosten.result')])
p = p + geom_bar(aes(x=Ttarget,y=value,fill=variable),stat="identity",position="dodge") #position="dodge"
p = p + theme_bw()# + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("cumuCO2result"="dark blue","cs"="dark red","T2010"="black","TCRE"="green", "nonCO2"="blue", "kosten.result"="yellow"))
p = p + ggtitle("CC values (Spearman rank) for CF, AR5-data")
p


#----------- plot van CC waarden (non-lin) --------------
# krijgt een CC matrix
CCmat <- f.costs.CCmatrix(N,s.seed)
CCdata = data.table(CCmat)
# maak er een 'werkbaarder' format van
CC <-gather(CCdata,variable,value,c('T2010','TCRE','CO22010','cumuCO2result','p.sample'))
CC=data.table(CC)
CC$temp <- as.character(seq(1, 4, by = 0.1))

# plotting (probeersel) staven naast elkaar
p = ggplot(CC[variable %in% c('T2010','CO22010','TCRE','p.sample')])
p = p + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="dodge")
p = p + theme_bw()# + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("CO22010"="grey","cumuCO2result"="dark blue","p.sample"="dark red","T2010"="black","TCRE"="green"))
p
ggsave(paste("CC_GE_nonlin.png"),p)

# plotting (probeersel) staven op elkaar
# eerst alle getallen positief maken (door te kwadrateren)
CC$value <- CC$value*CC$value


p = ggplot(CC[variable %in% c('p.sample','TCRE','T2010','CO22010')])
p = p + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="fill", order = c('p.sample','TCRE','T2010','CO22010'))
p = p + theme_bw()# + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("p.sample"="dark red","TCRE"="green","CO22010"="grey","T2010"="black"))
p
ggsave(paste("CC_GE_lin_stacked_squared.png"),p)

# plotting (probeersel) staven op elkaar
# eerst alle getallen positief maken (door absolute waarde te nemen)
CC$value <- abs(CC$value)

p = ggplot(arrange(CC[variable %in% c('T2010','CO22010','TCRE','p.sample')],variable,c('p.sample','TCRE','T2010','CO22010')))
p = p + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="fill")
p = p + theme_bw()# + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("CO22010"="grey","cumuCO2result"="dark blue","p.sample"="dark red","T2010"="black","TCRE"="green"))
p
ggsave(paste("CC_GE_lin_stacked_absolute.png"),p)


# maak er een werkbaarder format van
CC <-gather(CC,variable,value,c('T2010','TCRE','CO22010','cumuCO2result','p.sample'))
CC=data.table(CC)

# plotting (probeersel)
p = ggplot(data=CC[variable %in% c()])
p = p + geom_bar(aes(x=temp,y=value,fill=variable),stat="identity",position="dodge")
p = p + theme_bw() + theme(axis.text.x=element_text(size=12))
p = p + scale_fill_manual(values=c("CO22010"="grey","cumuCO2result"="dark blue","p.sample"="dark red","T2010"="black","TCRE"="green"))
ggsave(paste("Fig1.png"),p,width=12,height=12,dpi=300)



#--------- boxplots van costs -----------------------

# probeersel
b = ggplot(results)
b = b + geom_boxplot(aes)
b
