#----------------------------------------------------
#
# Model met bakjes, data van SSP
#
#----------------------------------------------------


source("TCRE+SSPnonCO2.R")


# functie om het punt op de rechte lijn tussen twee punten uit te rekenen
punt_rechteLijn <- function(x, x.links, x.rechts, fx.links, fx.rechts) {
  q <- (fx.rechts - fx.links)/(x.rechts - x.links)
  return(fx.links + (x - x.links)*q)
}


#---------------- data SSP inlezen ----------------------

#kostenSSP <- read.csv(file = "./../Databases/kostenSSP.csv", header = TRUE, sep = ";")
kostenSSP1 <- read.csv(file = "./../Databases/kostenSSPno0.csv", header = TRUE, sep = ";")
kostenSSP <- read.csv(file = "./../Databases/kostenSSPno02.csv", header = TRUE, sep = ";")

# schalen naar Tt CO2
kostenSSP$Cum.CO2 <- kostenSSP$Cum.CO2/1000
#kostenSSPno0$Cum.CO2 <- kostenSSPno0$Cum.CO2/1000

#plot
plot(kostenSSP$Cost.Estimate..ktrillion.~kostenSSP$Cum.CO2, xlab = "delta CO2 (Tt)", ylab = "", pch = 16, col = "blue", xlim = c(0,6))
points(kostenSSP$MAC.Costs..ktrillion.~kostenSSP$Cum.CO2, pch = 17, col = "green" )
points(kostenSSP$Consumption.Loss..ktrillion.~kostenSSP$Cum.CO2, pch = 18, col = "red" )

kostenSSP.gather <- gather(kostenSSP, variable, value)
kostenSSP.gather$cumuCO2 <- kostenSSP$Cum.CO2
kostenSSP.gather <- data.table(kostenSSP.gather)

s <- ggplot(kostenSSP.gather[variable %in% c('Cost.Estimate..ktrillion.','MAC.Costs..ktrillion.','Consumption.Loss..ktrillion.')])
s = s + geom_point(aes(x=cumuCO2,y=value, col=variable),stat="identity")
s = s + theme_bw()
s = s + scale_color_manual(values=c("Cost.Estimate..ktrillion."="blue","MAC.Costs..ktrillion."="green", "Consumption.Loss..ktrillion."="red"),
#s = s + scale_shape_manual(values=c("tempStijging_door_F_nonCO2"=1,"tempStijging_door_F_nonCO2.met.avg"=4), #if you want shapes
                           labels=c("Carbon price total costs","Area under MAC curve","Consumption loss"))
s = s + guides(col=guide_legend(title=NULL))
s = s + theme(legend.justification=c(0.9,0.65), legend.position=c(0.9,0.65), 
              legend.text = element_text(size = 12))
s = s + labs(x = expression(Cumulative~carbon~emissions~(2010-2100)~(TtCO[2])), y = "Mitigation costs (Ktrillion US$2005)")
s = s + coord_cartesian(xlim = c(0,5), ylim = c(0,0.25))
s


#plot
#plot(kostenSSPno0$Cost.Estimate..ktrillion.~kostenSSPno0$Cum.CO2, xlab = "delta CO2 (Tt)", ylab = "", pch = 16, col = "blue")
#points(kostenSSPno0$MAC.Costs..ktrillion.~kostenSSPno0$Cum.CO2, pch = 17, col = "green" )
#points(kostenSSPno0$Consumption.Loss..ktrillion.~kostenSSPno0$Cum.CO2, pch = 18, col = "red" )


# delen door waarde in deltaCO2 = 1.503707, !!!!
# neem voor de index een gemiddelde/modus/mediaan van een bakje!
###### 1.503707 = 100%
#
#index.CostEstimate <- kostenSSPno0$Cost.Estimate..ktrillion.[16]
#index.ConsumptionLoss <- kostenSSPno0$Consumption.Loss..ktrillion.[16]
#
#kostenSSPno0.indexed <- kostenSSPno0
#kostenSSPno0.indexed$Cost.Estimate..ktrillion. <- kostenSSPno0.indexed$Cost.Estimate..ktrillion./index.CostEstimate
#kostenSSPno0.indexed$Consumption.Loss..ktrillion. <- kostenSSPno0.indexed$Consumption.Loss..ktrillion./index.ConsumptionLoss


# plot CostEstimate en ConsumptionLoss, MAC doet niet mee
#plot(kostenSSPno0.indexed$Cost.Estimate..ktrillion.~kostenSSPno0.indexed$Cum.CO2, xlab = "delta CO2 (Tt)", ylab = "", pch = 16, col = "blue")
#points(kostenSSPno0.indexed$Consumption.Loss..ktrillion.~kostenSSPno0.indexed$Cum.CO2, pch = 18, col = "red" )


#--------------- Deel in in bakjes ------------------------

bakje1 <- data.frame(deltaCO2=double(), CostEstimate=double(), MACCosts=double(), ConsumptionLoss=double(), stringsAsFactors=FALSE)
bakje2 <- data.frame(deltaCO2=double(), CostEstimate=double(), MACCosts=double(), ConsumptionLoss=double(), stringsAsFactors=FALSE)
bakje3 <- data.frame(deltaCO2=double(), CostEstimate=double(), MACCosts=double(), ConsumptionLoss=double(), stringsAsFactors=FALSE)
bakje4 <- data.frame(deltaCO2=double(), CostEstimate=double(), MACCosts=double(), ConsumptionLoss=double(), stringsAsFactors=FALSE)
bakje5 <- data.frame(deltaCO2=double(), CostEstimate=double(), MACCosts=double(), ConsumptionLoss=double(), stringsAsFactors=FALSE)
bakje6 <- data.frame(deltaCO2=double(), CostEstimate=double(), MACCosts=double(), ConsumptionLoss=double(), stringsAsFactors=FALSE)

for (i in 1:length(kostenSSP$Cum.CO2)) {
  if (kostenSSP$Cum.CO2[i] >= 0.534 & kostenSSP$Cum.CO2[i] < 0.852) {
    bakje1 <- rbind(bakje1,kostenSSP[i,])
    
  } else if (kostenSSP$Cum.CO2[i] >= 1.126 & kostenSSP$Cum.CO2[i] < 1.573) {
    bakje2 <- rbind(bakje2,kostenSSP[i,])
    
  } else if (kostenSSP$Cum.CO2[i] >= 1.780 & kostenSSP$Cum.CO2[i] < 2.09) {
    bakje3 <- rbind(bakje3,kostenSSP[i,])
    
  } else if (kostenSSP$Cum.CO2[i] >= 2.09 & kostenSSP$Cum.CO2[i] < 2.4) {
    bakje4 <- rbind(bakje4,kostenSSP[i,])
    
  } else if (kostenSSP$Cum.CO2[i] >= 2.63 & kostenSSP$Cum.CO2[i] < 3.21) {
    bakje5 <- rbind(bakje5,kostenSSP[i,])
    
  } else if (kostenSSP$Cum.CO2[i] >= 3.81 & kostenSSP$Cum.CO2[i] < 4.56) {
    bakje6 <- rbind(bakje6,kostenSSP[i,])
  }
}

# ANDERE INDEX!:
# neem de mediaan van bakje 2!:
# cumuCO2: 1.313 #1.965 
bakje2.summary <- summary(bakje2)
deltaCO2.index <- 1.313
index.CostEstimate <- 0.022377 #0.02207
index.MACCosts <- 0.007941 #0.016022
index.ConsumptionLoss <- 0.023205 #0.02677

kostenSSP.indexed <- kostenSSP
kostenSSP.indexed$Cost.Estimate..ktrillion. <- kostenSSP.indexed$Cost.Estimate..ktrillion./index.CostEstimate
kostenSSP.indexed$MAC.Costs..ktrillion. <- kostenSSP.indexed$MAC.Costs..ktrillion./index.MACCosts
kostenSSP.indexed$Consumption.Loss..ktrillion. <- kostenSSP.indexed$Consumption.Loss..ktrillion./index.ConsumptionLoss

#plot indexes
plot(kostenSSP.indexed$Cost.Estimate..ktrillion.~kostenSSP.indexed$Cum.CO2, xlab = "delta CO2 (Tt)", ylab = "indexed costs", pch = 16, col = "blue", xlim = c(0,6))
points(kostenSSP.indexed$MAC.Costs..ktrillion.~kostenSSP.indexed$Cum.CO2, pch = 17, col = "green" )
points(kostenSSP.indexed$Consumption.Loss..ktrillion.~kostenSSP.indexed$Cum.CO2, pch = 18, col = "red" )

# ggplot
kostenSSP.ind.gather <- gather(kostenSSP.indexed, variable, value)
kostenSSP.ind.gather$cumuCO2 <- kostenSSP.indexed$Cum.CO2
kostenSSP.ind.gather <- data.table(kostenSSP.ind.gather)

s <- ggplot(kostenSSP.ind.gather[variable %in% c('Cost.Estimate..ktrillion.','MAC.Costs..ktrillion.','Consumption.Loss..ktrillion.')])
s = s + geom_point(aes(x=cumuCO2,y=value, col=variable),stat="identity")
s = s + theme_bw()
s = s + scale_color_manual(values=c("Cost.Estimate..ktrillion."="blue","MAC.Costs..ktrillion."="green", "Consumption.Loss..ktrillion."="red"),
                           #s = s + scale_shape_manual(values=c("tempStijging_door_F_nonCO2"=1,"tempStijging_door_F_nonCO2.met.avg"=4), #if you want shapes
                           labels=c("Carbon price total costs","Area under MAC curve","Consumption loss"))
s = s + guides(col=guide_legend(title=NULL))
s = s + theme(legend.justification=c(0.9,0.65), legend.position=c(0.9,0.65), 
              legend.text = element_text(size = 12))
s = s + labs(x = expression(Cumulative~carbon~emissions~(2010-2100)~(TtCO[2])), y = expression(Cost~index~(1.3~TtCO[2]==100)))
s = s + coord_cartesian(xlim = c(0,5)) #, ylim = c(0,10))
s



# maak de bakjes geindexeerd
bakje1.indexed <- bakje1
bakje1.indexed$Cost.Estimate..ktrillion. <- bakje1.indexed$Cost.Estimate..ktrillion./index.CostEstimate
bakje1.indexed$MAC.Costs..ktrillion. <- bakje1.indexed$MAC.Costs..ktrillion./index.MACCosts
bakje1.indexed$Consumption.Loss..ktrillion. <- bakje1.indexed$Consumption.Loss..ktrillion./index.ConsumptionLoss

bakje2.indexed <- bakje2
bakje2.indexed$Cost.Estimate..ktrillion. <- bakje2.indexed$Cost.Estimate..ktrillion./index.CostEstimate
bakje2.indexed$MAC.Costs..ktrillion. <- bakje2.indexed$MAC.Costs..ktrillion./index.MACCosts
bakje2.indexed$Consumption.Loss..ktrillion. <- bakje2.indexed$Consumption.Loss..ktrillion./index.ConsumptionLoss

bakje3.indexed <- bakje3
bakje3.indexed$Cost.Estimate..ktrillion. <- bakje3.indexed$Cost.Estimate..ktrillion./index.CostEstimate
bakje3.indexed$MAC.Costs..ktrillion. <- bakje3.indexed$MAC.Costs..ktrillion./index.MACCosts
bakje3.indexed$Consumption.Loss..ktrillion. <- bakje3.indexed$Consumption.Loss..ktrillion./index.ConsumptionLoss

bakje4.indexed <- bakje4
bakje4.indexed$Cost.Estimate..ktrillion. <- bakje4.indexed$Cost.Estimate..ktrillion./index.CostEstimate
bakje4.indexed$MAC.Costs..ktrillion. <- bakje4.indexed$MAC.Costs..ktrillion./index.MACCosts
bakje4.indexed$Consumption.Loss..ktrillion. <- bakje4.indexed$Consumption.Loss..ktrillion./index.ConsumptionLoss

bakje5.indexed <- bakje5
bakje5.indexed$Cost.Estimate..ktrillion. <- bakje5.indexed$Cost.Estimate..ktrillion./index.CostEstimate
bakje5.indexed$MAC.Costs..ktrillion. <- bakje5.indexed$MAC.Costs..ktrillion./index.MACCosts
bakje5.indexed$Consumption.Loss..ktrillion. <- bakje5.indexed$Consumption.Loss..ktrillion./index.ConsumptionLoss

bakje6.indexed <- bakje6
bakje6.indexed$Cost.Estimate..ktrillion. <- bakje6.indexed$Cost.Estimate..ktrillion./index.CostEstimate
bakje6.indexed$MAC.Costs..ktrillion. <- bakje6.indexed$MAC.Costs..ktrillion./index.MACCosts
bakje6.indexed$Consumption.Loss..ktrillion. <- bakje6.indexed$Consumption.Loss..ktrillion./index.ConsumptionLoss


# maak bakjes met 1 kostenmaat
ind.bakje1 <- data.frame(deltaCO2=double(), Costs=double(), stringsAsFactors=FALSE)
ind.bakje2 <- data.frame(deltaCO2=double(), Costs=double(), stringsAsFactors=FALSE)
ind.bakje3 <- data.frame(deltaCO2=double(), Costs=double(), stringsAsFactors=FALSE)
ind.bakje4 <- data.frame(deltaCO2=double(), Costs=double(), stringsAsFactors=FALSE)
ind.bakje5 <- data.frame(deltaCO2=double(), Costs=double(), stringsAsFactors=FALSE)
ind.bakje6 <- data.frame(deltaCO2=double(), Costs=double(), stringsAsFactors=FALSE)

for (i in 1:length(bakje1.indexed$Cum.CO2)) {
  ind.bakje1 <- rbind(ind.bakje1,c(bakje1.indexed$Cum.CO2[i],bakje1.indexed$Cost.Estimate..ktrillion.[i]))
  ind.bakje1 <- rbind(ind.bakje1,c(bakje1.indexed$Cum.CO2[i],bakje1.indexed$MAC.Costs..ktrillion.[i]))
  ind.bakje1 <- rbind(ind.bakje1,c(bakje1.indexed$Cum.CO2[i],bakje1.indexed$Consumption.Loss..ktrillion.[i]))
}
colnames(ind.bakje1) <- c("deltaCO2", "Costs")

for (i in 1:length(bakje2.indexed$Cum.CO2)) {
  ind.bakje2 <- rbind(ind.bakje2,c(bakje2.indexed$Cum.CO2[i],bakje2.indexed$Cost.Estimate..ktrillion.[i]))
  ind.bakje2 <- rbind(ind.bakje2,c(bakje2.indexed$Cum.CO2[i],bakje2.indexed$MAC.Costs..ktrillion.[i]))
  ind.bakje2 <- rbind(ind.bakje2,c(bakje2.indexed$Cum.CO2[i],bakje2.indexed$Consumption.Loss..ktrillion.[i]))
}
colnames(ind.bakje2) <- c("deltaCO2", "Costs")

for (i in 1:length(bakje3.indexed$Cum.CO2)) {
  ind.bakje3 <- rbind(ind.bakje3,c(bakje3.indexed$Cum.CO2[i],bakje3.indexed$Cost.Estimate..ktrillion.[i]))
  ind.bakje3 <- rbind(ind.bakje3,c(bakje3.indexed$Cum.CO2[i],bakje3.indexed$MAC.Costs..ktrillion.[i]))
  ind.bakje3 <- rbind(ind.bakje3,c(bakje3.indexed$Cum.CO2[i],bakje3.indexed$Consumption.Loss..ktrillion.[i]))
}
colnames(ind.bakje3) <- c("deltaCO2", "Costs")

for (i in 1:length(bakje4.indexed$Cum.CO2)) {
  ind.bakje4 <- rbind(ind.bakje4,c(bakje4.indexed$Cum.CO2[i],bakje4.indexed$Cost.Estimate..ktrillion.[i]))
  ind.bakje4 <- rbind(ind.bakje4,c(bakje4.indexed$Cum.CO2[i],bakje4.indexed$MAC.Costs..ktrillion.[i]))
  ind.bakje4 <- rbind(ind.bakje4,c(bakje4.indexed$Cum.CO2[i],bakje4.indexed$Consumption.Loss..ktrillion.[i]))
}
colnames(ind.bakje4) <- c("deltaCO2", "Costs")

for (i in 1:length(bakje5.indexed$Cum.CO2)) {
  ind.bakje5 <- rbind(ind.bakje5,c(bakje5.indexed$Cum.CO2[i],bakje5.indexed$Cost.Estimate..ktrillion.[i]))
  ind.bakje5 <- rbind(ind.bakje5,c(bakje5.indexed$Cum.CO2[i],bakje5.indexed$MAC.Costs..ktrillion.[i]))
  ind.bakje5 <- rbind(ind.bakje5,c(bakje5.indexed$Cum.CO2[i],bakje5.indexed$Consumption.Loss..ktrillion.[i]))
}
colnames(ind.bakje5) <- c("deltaCO2", "Costs")

for (i in 1:length(bakje6.indexed$Cum.CO2)) {
  ind.bakje6 <- rbind(ind.bakje6,c(bakje6.indexed$Cum.CO2[i],bakje6.indexed$Cost.Estimate..ktrillion.[i]))
  ind.bakje6 <- rbind(ind.bakje6,c(bakje6.indexed$Cum.CO2[i],bakje6.indexed$MAC.Costs..ktrillion.[i]))
  ind.bakje6 <- rbind(ind.bakje6,c(bakje6.indexed$Cum.CO2[i],bakje6.indexed$Consumption.Loss..ktrillion.[i]))
}
colnames(ind.bakje6) <- c("deltaCO2", "Costs")




#------------------------------------------------
#------------- Define bakjes ----------------

# deltaCO2 waarde
bakje1.deltaCO2 <- (min(ind.bakje1$deltaCO2) + max(ind.bakje1$deltaCO2))/2
bakje2.deltaCO2 <- (min(ind.bakje2$deltaCO2) + max(ind.bakje2$deltaCO2))/2
bakje3.deltaCO2 <- (min(ind.bakje3$deltaCO2) + max(ind.bakje3$deltaCO2))/2
bakje4.deltaCO2 <- (min(ind.bakje4$deltaCO2) + max(ind.bakje4$deltaCO2))/2
bakje5.deltaCO2 <- (min(ind.bakje5$deltaCO2) + max(ind.bakje5$deltaCO2))/2
bakje6.deltaCO2 <- (min(ind.bakje6$deltaCO2) + max(ind.bakje6$deltaCO2))/2
bakjeNoCostsSSP.deltaCO2 <- 6

# median costs
bakje1.median <- median(ind.bakje1$Costs, na.rm = T)
bakje2.median <- median(ind.bakje2$Costs, na.rm = T)
bakje3.median <- 0.566732503 #median(ind.bakje3$Costs, na.rm = T)
bakje4.median <- median(ind.bakje4$Costs, na.rm = T)
bakje5.median <- median(ind.bakje5$Costs, na.rm = T)
bakje6.median <- median(ind.bakje6$Costs, na.rm = T)
bakjeNoCostsSSP.median <- 0

# minimum- en maximumwaarde van bakjes
bakje1.min <- min(ind.bakje1$Costs, na.rm = T)
bakje1.max <- 14.56416286 #max(ind.bakje1$Costs, na.rm = T)

bakje2.min <- min(ind.bakje2$Costs, na.rm = T)
bakje2.max <- max(ind.bakje2$Costs, na.rm = T)

bakje3.min <- 0.072391631 #min(ind.bakje3$Costs, na.rm = T)
bakje3.max <- 4.290558506 #max(ind.bakje3$Costs, na.rm = T)

bakje4.min <- min(ind.bakje4$Costs, na.rm = T)
bakje4.max <- 3.173387129 #max(ind.bakje4$Costs, na.rm = T)

bakje5.min <- 0.0112516 #min(ind.bakje5$Costs, na.rm = T)
bakje5.max <- max(ind.bakje5$Costs, na.rm = T)

bakje6.min <- min(ind.bakje6$Costs, na.rm = T)
bakje6.max <- max(ind.bakje6$Costs, na.rm = T)

bakjeNoCostsSSP.min <- 0
bakjeNoCostsSSP.max <- 0


#------------ Maak sample [0.1] ------------------

sampletrans01 <- rpert(N, min = 0, mode = 1/3, max = 1)


#------------ 'Reken' kosten uit ----------------

costs.oneRun <- function(deltaCO2, trans01) {
  # zit het onder bakje 1?
  if (deltaCO2 < bakje1.deltaCO2) {
    # return(-1) #return("lager dan bakje 430.480")
    kosten.median <- punt_rechteLijn(deltaCO2, bakje1.deltaCO2, bakje2.deltaCO2, bakje1.median, bakje2.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje1.deltaCO2, bakje2.deltaCO2, bakje1.min, bakje2.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje1.deltaCO2, bakje2.deltaCO2, bakje1.max, bakje2.max)
    
    # zit het tussen bakje 1 en 2?
  } else if (deltaCO2 >= bakje1.deltaCO2 & deltaCO2 < bakje2.deltaCO2) {
    kosten.median <- punt_rechteLijn(deltaCO2, bakje1.deltaCO2, bakje2.deltaCO2, bakje1.median, bakje2.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje1.deltaCO2, bakje2.deltaCO2, bakje1.min, bakje2.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje1.deltaCO2, bakje2.deltaCO2, bakje1.max, bakje2.max)
    
    # zit het tussen bakje 2 en 3?
  } else if (deltaCO2 >= bakje2.deltaCO2 & deltaCO2 < bakje3.deltaCO2) {
    kosten.median <- punt_rechteLijn(deltaCO2, bakje2.deltaCO2, bakje3.deltaCO2, bakje2.median, bakje3.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje2.deltaCO2, bakje3.deltaCO2, bakje2.min, bakje3.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje2.deltaCO2, bakje3.deltaCO2, bakje2.max, bakje3.max)
    
    # zit het tussen bakje 3 en 4?
  } else if (deltaCO2 >= bakje3.deltaCO2 & deltaCO2 < bakje4.deltaCO2) {
    kosten.median <- punt_rechteLijn(deltaCO2, bakje3.deltaCO2, bakje4.deltaCO2, bakje3.median, bakje4.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje3.deltaCO2, bakje4.deltaCO2, bakje3.min, bakje4.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje3.deltaCO2, bakje4.deltaCO2, bakje3.max, bakje4.max)
    
    # zit het tussen bakje 4 en 5?
  } else if (deltaCO2 >= bakje4.deltaCO2 & deltaCO2 < bakje5.deltaCO2) {
    kosten.median <- punt_rechteLijn(deltaCO2, bakje4.deltaCO2, bakje5.deltaCO2, bakje4.median, bakje5.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje4.deltaCO2, bakje5.deltaCO2, bakje4.min, bakje5.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje4.deltaCO2, bakje5.deltaCO2, bakje4.max, bakje5.max)
    
    # zit het tussen bakje 5 en 6?
  } else if (deltaCO2 >= bakje5.deltaCO2 & deltaCO2 < bakje6.deltaCO2) {
    kosten.median <- punt_rechteLijn(deltaCO2, bakje5.deltaCO2, bakje6.deltaCO2, bakje5.median, bakje6.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje5.deltaCO2, bakje6.deltaCO2, bakje5.min, bakje6.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje5.deltaCO2, bakje6.deltaCO2, bakje5.max, bakje6.max)
    
    # zit het tussen bakje 6 en NoCost?
  } else if (deltaCO2 >= bakje6.deltaCO2 & deltaCO2 < bakjeNoCostsSSP.deltaCO2) {
    kosten.median <- punt_rechteLijn(deltaCO2, bakje6.deltaCO2, bakjeNoCostsSSP.deltaCO2, bakje6.median, bakjeNoCostsSSP.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje6.deltaCO2, bakjeNoCostsSSP.deltaCO2, bakje6.min, bakjeNoCostsSSP.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje6.deltaCO2, bakjeNoCostsSSP.deltaCO2, bakje6.max, bakjeNoCostsSSP.max)
    
    # zit het hoger dan bakjeNoCostsSSP  
  } else  if (deltaCO2 >= bakjeNoCostsSSP.deltaCO2) {
    return(0) #return("hoger dan bakje NoCostsSSP")
  }
  
  grootte <- kosten.max - kosten.min
  kosten <- (kosten.max-kosten.min)*trans01 + kosten.min
  
  return(kosten)
}


#-------------- run model -------------------------------

model.costs <- function(deltaCO2) {
  
  # run model
  costs <- mapply(costs.oneRun, deltaCO2, sampletrans01)
  
  return(costs)
}


#-------------- maak data per Ttarget ------------------

f.dataframe.deltaCO2 <- function(N,Ttarget,f.seed) {
  # maak samples
  cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
  # reken resultaten uit
  sample_en_result <- f.cumuCO2result(N,Ttarget,cumuvstemp.sample)
  return(sample_en_result)
}

f.dataframe.kosten <- function(N,Ttarget,f.seed) {
  # maak samples en deltaCO2 resultaten
  sample_en_result.deltaCO2 <- f.dataframe.deltaCO2(N,Ttarget,f.seed)
  
  kosten.result <- model.costs(sample_en_result.deltaCO2$cumuCO2result)
  
  
  
  return(data.frame(sample_en_result.deltaCO2,kosten.result))
}


#-------------- CCmatrix -------------

N <- 10000
s.seed <- 21
#remove <- c(-1)

f.costs.CCmatrix <- function(N,f.seed) {
  # initialisatie
  CCmatrixP <- NULL
  CCmatrixS <- NULL
  #aantalMin1 <- vector(mode="numeric", length=0)
  teller <- 0
  
  
  cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
  
  for (i in seq(1.4, 3.4, by = 0.1)) {
    # print(i)
    sample_en_result.deltaCO2 <- f.cumuCO2result(N,i,cumuvstemp.sample)
    
    kosten.result <- model.costs(sample_en_result.deltaCO2$cumuCO2result)
    # herleid costsensitivity
    #cs <-kosten.result/sample_en_result.deltaCO2$cumuCO2result
    
    sample_en_result.kosten <- data.frame(sample_en_result.deltaCO2,sampletrans01,kosten.result)
    
    # verwijder resultaten buiten bakjes
    #waarZitMin1 <- which(sample_en_result.kosten$kosten.result %in% remove)
    #aantalMin1 <- c(aantalMin1, length(waarZitMin1))
    
    #if (!identical(waarZitMin1, integer(0))) {
    #  sample_en_result.kosten <- sample_en_result.kosten[-waarZitMin1,]
    #}
    
    # pearson CC:
    CCmatrixP.hulp <- cor(sample_en_result.kosten)[-1,]
    CCmatrixP <- rbind(CCmatrixP, CCmatrixP.hulp[-6,7])
    # Spearman CC:
    CCmatrixS.hulp <- cor(sample_en_result.kosten, method = "spearman")[-1,]
    CCmatrixS <- rbind(CCmatrixS, CCmatrixS.hulp[-6,7])
    
    teller <- teller + 1
  }
  rownames(CCmatrixP) <- as.character(seq(1.4, 3.4, by = 0.1))
  rownames(CCmatrixS) <- as.character(seq(1.4, 3.4, by = 0.1))
  
  return(list(CCmatrixP,CCmatrixS))
}

# maak een matrix van CCwaarden
#CCmat <- f.costs.CCmatrix(N,s.seed)

# maak een CC tabel voor een enkele Ttarget
f.CCtabel <- function(N,Ttarget, f.seed) {
  
  cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
  
  sample_en_result.deltaCO2 <- f.cumuCO2result(N,Ttarget,cumuvstemp.sample)
  
  kosten.result <- model.costs(sample_en_result.deltaCO2$cumuCO2result)
  # herleid costsensitivity
  #cs <-kosten.result/sample_en_result.deltaCO2$cumuCO2result
  
  sample_en_result.kosten <- data.frame(sample_en_result.deltaCO2,sampletrans01,kosten.result)
  
  
  # pearson CC:
  CCtabelP <- cor(sample_en_result.kosten)[-1,-1]
  
  # Spearman CC:
  CCtabelS <- cor(sample_en_result.kosten, method = "spearman")[-1,-1]
  
  return(list(CCtabelP,CCtabelS))
}


#maar een CCmatrix voor CC tussen cs en andere input
f.cs.CCmatrix <- function(N,f.seed) {
  # initialisatie
  CCmatrixP <- NULL
  CCmatrixS <- NULL
  #aantalMin1 <- vector(mode="numeric", length=0)
  teller <- 0
  
  
  cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
  
  for (i in seq(1.4, 3.4, by = 0.1)) {
    # print(i)
    sample_en_result.deltaCO2 <- f.cumuCO2result(N,i,cumuvstemp.sample)
    
    kosten.result <- model.costs(sample_en_result.deltaCO2$cumuCO2result)
    # herleid costsensitivity
    #cs <-kosten.result/sample_en_result.deltaCO2$cumuCO2result
    
    sample_en_result.kosten <- data.frame(sample_en_result.deltaCO2,sampletrans01,kosten.result)
    
    # verwijder resultaten buiten bakjes
    #waarZitMin1 <- which(sample_en_result.kosten$kosten.result %in% remove)
    #aantalMin1 <- c(aantalMin1, length(waarZitMin1))
    
    #if (!identical(waarZitMin1, integer(0))) {
    #  sample_en_result.kosten <- sample_en_result.kosten[-waarZitMin1,]
    #}
    
    # pearson CC:
    CCmatrixP.hulp <- cor(sample_en_result.kosten)[-1,]
    CCmatrixP <- rbind(CCmatrixP, CCmatrixP.hulp[-5,6])
    # Spearman CC:
    CCmatrixS.hulp <- cor(sample_en_result.kosten, method = "spearman")[-1,]
    CCmatrixS <- rbind(CCmatrixS, CCmatrixS.hulp[-5,6])
    
    teller <- teller + 1
  }
  rownames(CCmatrixP) <- as.character(seq(1.4, 3.4, by = 0.1))
  rownames(CCmatrixS) <- as.character(seq(1.4, 3.4, by = 0.1))
  
  return(list(CCmatrixP,CCmatrixS))
}
