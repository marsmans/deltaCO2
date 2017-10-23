#----------------------------------------------------
#
# Model met bakjes
#
#----------------------------------------------------
# final met costs en trans01 voor AR5 data voor costs

source("TCRE+SSPnonCO2.R")


# functie om het punt op de rechte lijn tussen twee punten uit te rekenen
punt_rechteLijn <- function(x, x.links, x.rechts, fx.links, fx.rechts) {
  q <- (fx.rechts - fx.links)/(x.rechts - x.links)
  return(fx.links + (x - x.links)*q)
}

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


# 4TtCO2bakje
bakjeNoCosts.deltaCO2 <- 4

# percentage GDP inlezen (mitigation costs: abatement costs, fig 6.21)
bakje430.480 <- read.csv(file = "./../Databases/430-480ppmCO2eq.txt", header = TRUE)
bakje480.530 <- read.csv(file = "./../Databases/480-530ppmCO2eq.txt", header = TRUE)
bakje530.580 <- read.csv(file = "./../Databases/530-580ppmCO2eq.txt", header = TRUE)
bakje580.650 <- read.csv(file = "./../Databases/580-650ppmCO2eq.txt", header = TRUE)
bakje650.720 <- read.csv(file = "./../Databases/650-720ppmCO2eq.txt", header = TRUE)


# mean
bakje430.480.median <- 1.39487 # bakje430.480$percentGDP[3] # 1.39487 # 
bakje480.530.median <- bakje480.530$percentGDP[3]
bakje530.580.median <- bakje530.580$percentGDP[3]
bakje580.650.median <- bakje580.650$percentGDP[3]
bakje650.720.median <- 0.155960582 #bakje650.720$percentGDP[3]
bakjeNoCosts.median <- 0

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
bakje430.480.max <- 6.15821 #bakje430.480$percentGDP[5] # 6.15821 # 

bakje480.530.min <- bakje480.530$percentGDP[1]
bakje480.530.max <- bakje480.530$percentGDP[5]

bakje530.580.min <- bakje530.580$percentGDP[1]
bakje530.580.max <- bakje530.580$percentGDP[5]

bakje580.650.min <- bakje580.650$percentGDP[1]
bakje580.650.max <- bakje580.650$percentGDP[5]

bakje650.720.min <- 0.065679055 #bakje650.720$percentGDP[1]
bakje650.720.max <- 0.192474374 #bakje650.720$percentGDP[5]

bakjeNoCosts.min <- 0
bakjeNoCosts.max <- 0


#------------ Maak sample [0.1] ------------------

sampletrans01 <- rpert(N, min = 0, mode = 1/3, max = 1)

#------------ 'Reken' kosten uit ----------------

costs.oneRun <- function(deltaCO2, trans01) {
  # zit het onder bakje 430-480?
  if (deltaCO2 < bakje430.480.deltaCO2) {
    # return(-1) #return("lager dan bakje 430.480")
    kosten.median <- punt_rechteLijn(deltaCO2, bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje430.480.median, bakje480.530.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje430.480.min, bakje480.530.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje430.480.max, bakje480.530.max)
    
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
    
    # zit het tussen bakje 650-720 en 720-NoCosts?
  } else if (deltaCO2 >= bakje650.720.deltaCO2 & deltaCO2 <= bakjeNoCosts.deltaCO2) {
    kosten.median <- punt_rechteLijn(deltaCO2, bakje650.720.deltaCO2, bakjeNoCosts.deltaCO2, bakje650.720.median, bakjeNoCosts.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje650.720.deltaCO2, bakjeNoCosts.deltaCO2, bakje650.720.min, bakjeNoCosts.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje650.720.deltaCO2, bakjeNoCosts.deltaCO2, bakje650.720.max, bakjeNoCosts.max)
    
    # zit het hoger dan bakjeNoCosts  
  } else  if (deltaCO2 > bakjeNoCosts.deltaCO2) {
    return(0) #return("hoger dan bakje NoCosts")
  }
  
  grootte <- kosten.max - kosten.min
  #trekking_kosten <- rpert(1, min = kosten.min, mode = kosten.median, max = kosten.max)
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
  
  cs <-kosten.result/sample_en_result.deltaCO2$cumuCO2result
  
  return(data.frame(sample_en_result.deltaCO2,cs,kosten.result))
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
  
  for (i in seq(1.4, 3.4, by = 0.1)) {
    # print(i)
    sample_en_result.deltaCO2 <- f.cumuCO2result(N,i,cumuvstemp.sample)
    
    kosten.result <- model.costs(sample_en_result.deltaCO2$cumuCO2result)
    # herleid costsensitivity
    #cs <-kosten.result/sample_en_result.deltaCO2$cumuCO2result
    
    sample_en_result.kosten <- data.frame(sample_en_result.deltaCO2,sampletrans01,kosten.result)
    
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

