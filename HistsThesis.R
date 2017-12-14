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


# maak data
data1.5 <- f.dataframekosten(N,1.5,s.seed)
data2 <- f.dataframekosten(N,2,s.seed)
data3 <- f.dataframekosten(N,3,s.seed)
data4 <- f.dataframekosten(N,4,s.seed)


# bij hoeveel zijn de kosten 0?
hoeveel0 <- function(data){
  waarZit0 <- which(data$kosten.result %in% 0)
  return(length(waarZit0))
}


data1.5.test <- subset(data1.5, select = -f.Ttarget)
data2.test <- subset(data2, select = -f.Ttarget)
data3.test <- subset(data3, select = -f.Ttarget)
data4.test <- subset(data, select = -f.Ttarget)

# Deze is goed volgens mij!:
data1.5.test <- melt(data1.5.test, id.vars = 'kosten.result', variable.name = 'parameter')
data1.5.test$Ttarget <- 1.5
data1.5.test <- data.table(data1.5.test)

data2.test <- melt(data2.test, id.vars = 'kosten.result', variable.name = 'parameter')
data2.test$Ttarget <- 2
data2.test <- data.table(data2.test)

data3.test <- melt(data3.test, id.vars = 'kosten.result', variable.name = 'parameter')
data3.test$Ttarget <- 3
data3.test <- data.table(data3.test)

data4.test <- melt(data4.test, id.vars = 'kosten.result', variable.name = 'parameter')
data4.test$Ttarget <- 4
data4.test <- data.table(data4.test)

data1.523.test <- rbind(data1.5.test,data2.test,data3.test)
data1.523.test <- data.table(data1.523.test)

data1.5234.test <- rbind(data1.5.test,data2.test,data3.test, data4.test)
data1.5234.test <- data.table(data1.5234.test)

plabels <- c(T2010 = "T2010", TCRE = "TCRE", nonCO2 = "TFnonCO2,2010", cumuCO2result = "deltaCO2", sampletrans01 = "t")


# histogrammen van carbonbudget UA

hcb <- ggplot(data1.523.test[parameter %in% c('cumuCO2result')], aes(value))
hcb = hcb + geom_histogram(bins = 50)
hcb = hcb + facet_grid(. ~ Ttarget, labeller=labeller(parameter = plabels), scales="free", space="free")
hcb = hcb + theme_bw()
hcb = hcb + labs(x = expression(Cumulative~CO[2]~emissions~(2010-2100)~(TtCO[2])))
hcb

# 1.5+2+3+4
hcb <- ggplot(data1.5234.test[parameter %in% c('cumuCO2result')], aes(value))
hcb = hcb + geom_histogram(bins = 50)
hcb = hcb + facet_grid(parameter ~ Ttarget, labeller=labeller(parameter = plabels), scales="free", space="free")
hcb = hcb + theme_bw()
hcb = hcb + labs(x = expression(Cumulative~CO[2]~emissions~(2010-2100)~(TtCO[2])))
hcb


# histogrammen van mitigationcosts UA
source("kostenbakjesAR5tran01toCosts.R")
costs1.5 <- data.frame(costs = data1.5$kosten.result)
costs1.5$Ttarget <- 1.5

costs2 <- data.frame(costs = data2$kosten.result)
costs2$Ttarget <- 2

costs3 <- data.frame(costs = data3$kosten.result)
costs3$Ttarget <- 3

#costs4 <- data.frame(costs = data4$kosten.result)
#costs4$Ttarget <- 4

costsAR5 <- rbind(costs1.5, costs2, costs3)#, costs4)
costsAR5$database <- "AR5"

source("kostenSSPanIndex01trans.R")
costs1.5 <- data.frame(costs = data1.5$kosten.result)
costs1.5$Ttarget <- 1.5

costs2 <- data.frame(costs = data2$kosten.result)
costs2$Ttarget <- 2

costs3 <- data.frame(costs = data3$kosten.result)
costs3$Ttarget <- 3

costs4 <- data.frame(costs = data4$kosten.result)
costs4$Ttarget <- 4

costsSSP <- rbind(costs1.5, costs2, costs3, costs4)
costsSSP$database <- "SSP"

costsAR5SSP <- rbind(costsAR5,costsSSP)


hcb <- ggplot(costsAR5SSP, aes(costs))
hcb = hcb + geom_histogram(bins = 50)
hcb = hcb + facet_grid(database ~ Ttarget, scales = "free") #labeller=labeller(parameter = plabels) , scales="free", space="free"
hcb = hcb + theme_bw()
hcb = hcb + labs(x = "Mitigation Costs")
hcb




# histogram t

hcb <- ggplot()
hcb = hcb + geom_histogram(aes(sampletrans01),bins = 30)
#hcb = hcb + facet_grid(. ~ Ttarget, labeller=labeller(parameter = plabels), scales="free", space="free")
hcb = hcb + theme_bw()
hcb = hcb + labs(x = "t")
hcb

