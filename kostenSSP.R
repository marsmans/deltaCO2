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

kostenSSP <- read.csv(file = "./../Databases/kostenSSP.csv", header = TRUE, sep = ";")
#kostenSSPno0 <- read.csv(file = "./../Databases/kostenSSPno0.csv", header = TRUE, sep = ";")


# schalen naar Tt CO2
kostenSSP$Cum.CO2 <- kostenSSP$Cum.CO2/1000
#kostenSSPno0$Cum.CO2 <- kostenSSPno0$Cum.CO2/1000

#plot
plot(kostenSSP$Cost.Estimate..ktrillion.~kostenSSP$Cum.CO2, xlab = "delta CO2 (Tt)", ylab = "", pch = 16, col = "blue")
points(kostenSSP$MAC.Costs..ktrillion.~kostenSSP$Cum.CO2, pch = 17, col = "green" )
points(kostenSSP$Consumption.Loss..ktrillion.~kostenSSP$Cum.CO2, pch = 18, col = "red" )

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

for (i in 1:length(kostenSSP$Cum.CO2)) {
  if (kostenSSP$Cum.CO2[i] >= 0.534 & kostenSSP$Cum.CO2[i] < 0.852) {
    bakje1 <- rbind(bakje1,kostenSSP[i,])
    
  } else if (kostenSSP$Cum.CO2[i] >= 1.126 & kostenSSP$Cum.CO2[i] < 1.573) {
    bakje2 <- rbind(bakje2,kostenSSP[i,])
    
  } else if (kostenSSP$Cum.CO2[i] >= 1.780 & kostenSSP$Cum.CO2[i] < 2.4) {
    bakje3 <- rbind(bakje3,kostenSSP[i,])
    
  } else if (kostenSSP$Cum.CO2[i] >= 2.63 & kostenSSP$Cum.CO2[i] < 3.21) {
    bakje4 <- rbind(bakje4,kostenSSP[i,])
    
  } else if (kostenSSP$Cum.CO2[i] >= 3.81 & kostenSSP$Cum.CO2[i] < 4.56) {
    bakje5 <- rbind(bakje5,kostenSSP[i,])
  }
}
  
# indexed opnieuw


#------------- Define bakjes ----------------

# 430-480 bakje:
# 630-1180
# middelpunt:
bakje430.480.deltaCO2 <- (0.630+1.180)/2

# 480-530 bakje:
# 960-1550
# middelpunt:
bakje480.530.deltaCO2 <- (0.960+1.550)/2

# 530-580 bakje
# 1170-2240
# middelpunt:
bakje530.580.deltaCO2 <- (1.170+2.240)/2

# 580-650 bakje
# 1870-2440
# middelpunt:
bakje580.650.deltaCO2 <- (1.870+2.440)/2

# 650-720 bakje
# 2570-3340
# middelpunt:
bakje650.720.deltaCO2 <- (2.570+3.340)/2


# percentage GDP inlezen (mitigation costs: abatement costs, fig 6.21)
bakje430.480 <- read.csv(file = "./../Databases/430-480ppmCO2eq.txt", header = TRUE)
bakje480.530 <- read.csv(file = "./../Databases/480-530ppmCO2eq.txt", header = TRUE)
bakje530.580 <- read.csv(file = "./../Databases/530-580ppmCO2eq.txt", header = TRUE)
bakje580.650 <- read.csv(file = "./../Databases/580-650ppmCO2eq.txt", header = TRUE)
bakje650.720 <- read.csv(file = "./../Databases/650-720ppmCO2eq.txt", header = TRUE)


# mean
bakje430.480.median <- bakje430.480$percentGDP[3]
bakje480.530.median <- bakje480.530$percentGDP[3]
bakje530.580.median <- bakje530.580$percentGDP[3]
bakje580.650.median <- bakje580.650$percentGDP[3]
bakje650.720.median <- bakje650.720$percentGDP[3]

# std gebaseerd op 25 en 75 precentiel
bakje430.480std75 <- (bakje430.480$percentGDP[4] - bakje430.480.median)/abs(qnorm(0.75))
bakje430.480std25 <- (bakje430.480.median - bakje430.480$percentGDP[2])/abs(qnorm(0.25))
bakje430.480.std <- (bakje430.480std75 + bakje430.480std25)/2

bakje480.530std75 <- (bakje480.530$percentGDP[4] - bakje480.530.median)/abs(qnorm(0.75))
bakje480.530std25 <- (bakje480.530.median - bakje480.530$percentGDP[2])/abs(qnorm(0.25))
bakje480.530.std <- (bakje480.530std75 + bakje480.530std25)/2

bakje530.580std75 <- (bakje530.580$percentGDP[4] - bakje530.580.median)/abs(qnorm(0.75))
bakje530.580std25 <- (bakje530.580.median - bakje530.580$percentGDP[2])/abs(qnorm(0.25))
bakje530.580.std <- (bakje530.580std75 + bakje530.580std25)/2

bakje580.650std75 <- (bakje580.650$percentGDP[4] - bakje580.650.median)/abs(qnorm(0.75))
bakje580.650std25 <- (bakje580.650.median - bakje580.650$percentGDP[2])/abs(qnorm(0.25))
bakje580.650.std <- (bakje580.650std75 + bakje580.650std25)/2

bakje650.720std75 <- (bakje650.720$percentGDP[4] - bakje650.720.median)/abs(qnorm(0.75))
bakje650.720std25 <- (bakje650.720.median - bakje650.720$percentGDP[2])/abs(qnorm(0.25))
bakje650.720.std <- (bakje650.720std75 + bakje650.720std25)/2


# minimum- en maximumwaarde van bakjes
bakje430.480.min <- bakje430.480$percentGDP[1]
bakje430.480.max <- bakje430.480$percentGDP[5]

bakje480.530.min <- bakje480.530$percentGDP[1]
bakje480.530.max <- bakje480.530$percentGDP[5]

bakje530.580.min <- bakje530.580$percentGDP[1]
bakje530.580.max <- bakje530.580$percentGDP[5]

bakje580.650.min <- bakje580.650$percentGDP[1]
bakje580.650.max <- bakje580.650$percentGDP[5]

bakje650.720.min <- bakje650.720$percentGDP[1]
bakje650.720.max <- bakje650.720$percentGDP[5]


costs.oneRun <- function(deltaCO2) {
  # zit het onder bakje 430-480?
  if (deltaCO2 < bakje430.480.deltaCO2) {
    return(-1) #return("lager dan bakje 430.480")
    # zit het tussen bakje 430-480 en 480-530?
  } else if (deltaCO2 >= bakje430.480.deltaCO2 & deltaCO2 < bakje480.530.deltaCO2) {
    kosten.median <- punt_rechteLijn(deltaCO2, bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje430.480.median, bakje480.530.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje430.480.min, bakje480.530.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje430.480.max, bakje480.530.max)
    
    # zit het tussen bakje 480-530 en 530-580?
  } else if (deltaCO2 >= bakje480.530.deltaCO2 & deltaCO2 < bakje530.580.deltaCO2) {
    kosten.median <- punt_rechteLijn(deltaCO2, bakje480.530.deltaCO2, bakje530.580.deltaCO2, bakje480.530.median, bakje530.580.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje480.530.deltaCO2, bakje530.580.deltaCO2, bakje480.530.min, bakje530.580.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje480.530.deltaCO2, bakje530.580.deltaCO2, bakje480.530.max, bakje530.580.max)
    
    # zit het tussen bakje 530-580 en 580-650?
  } else if (deltaCO2 >= bakje530.580.deltaCO2 & deltaCO2 < bakje580.650.deltaCO2) {
    kosten.median <- punt_rechteLijn(deltaCO2, bakje530.580.deltaCO2, bakje580.650.deltaCO2, bakje530.580.median, bakje580.650.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje530.580.deltaCO2, bakje580.650.deltaCO2, bakje530.580.min, bakje580.650.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje530.580.deltaCO2, bakje580.650.deltaCO2, bakje530.580.max, bakje580.650.max)
    
    # zit het tussen bakje 580-650 en 650-720?
  } else if (deltaCO2 >= bakje580.650.deltaCO2 & deltaCO2 <= bakje650.720.deltaCO2) {
    kosten.median <- punt_rechteLijn(deltaCO2, bakje580.650.deltaCO2, bakje650.720.deltaCO2, bakje580.650.median, bakje650.720.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje580.650.deltaCO2, bakje650.720.deltaCO2, bakje580.650.min, bakje650.720.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje580.650.deltaCO2, bakje650.720.deltaCO2, bakje580.650.max, bakje650.720.max)
    
  } else  if (deltaCO2 > bakje650.720.deltaCO2) {
    return(-1) #return("hoger dan bakje 650-720")
  }
  
  grootte <- kosten.max - kosten.min
  trekking_kosten <- rpert(1, min = kosten.min, mode = kosten.median, max = kosten.max)
  
  return(trekking_kosten)
}


#-------------- run model -------------------------------

model.costs <- function(deltaCO2) {
  
  # run model
  costs <- mapply(costs.oneRun, deltaCO2)
  
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
remove <- c(-1)

f.costs.CCmatrix <- function(N,f.seed) {
  # initialisatie
  CCmatrixP <- NULL
  CCmatrixS <- NULL
  aantalMin1 <- vector(mode="numeric", length=0)
  teller <- 0
  
  
  cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
  
  for (i in seq(1.2, 3.4, by = 0.1)) {
    # print(i)
    sample_en_result.deltaCO2 <- f.cumuCO2result(N,i,cumuvstemp.sample)
    
    kosten.result <- model.costs(sample_en_result.deltaCO2$cumuCO2result)
    
    sample_en_result.kosten <- data.frame(sample_en_result.deltaCO2,kosten.result)
    
    # verwijder resultaten buiten bakjes
    waarZitMin1 <- which(sample_en_result.kosten$kosten.result %in% remove)
    aantalMin1 <- c(aantalMin1, length(waarZitMin1))
    
    if (!identical(waarZitMin1, integer(0))) {
      sample_en_result.kosten <- sample_en_result.kosten[-waarZitMin1,]
    }
    
    # pearson CC:
    CCmatrixP.hulp <- cor(sample_en_result.kosten)[-1,]
    CCmatrixP <- rbind(CCmatrixP, CCmatrixP.hulp[-5,6])
    # Spearman CC:
    CCmatrixS.hulp <- cor(sample_en_result.kosten, method = "spearman")[-1,]
    CCmatrixS <- rbind(CCmatrixS, CCmatrixS.hulp[-5,6])
    
    teller <- teller + 1
  }
  rownames(CCmatrixP) <- as.character(seq(1.2, 3.4, by = 0.1))
  rownames(CCmatrixS) <- as.character(seq(1.2, 3.4, by = 0.1))
  
  return(list(CCmatrixP,CCmatrixS))
}

# maak een matrix van CCwaarden
#CCmat <- f.costs.CCmatrix(N,s.seed)
