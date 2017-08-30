#----------------------------------------------------
#
# Model met bakjes
#
#----------------------------------------------------


source("TCRE+SSPnonCO2.R")


# functie om het punt op de rechte lijn tussen twee punten uit te rekenen
punt_rechteLijn <- function(x, x.links, x.rechts, fx.links, fx.rechts) {
  q <- (fx.rechts - fx.links)/(x.rechts - x.links)
  return(fx.links + (x - x.links)*q)
}

# functie die de rc geeft van de rechte lijn tussen twee punten
rc <- function(x1,x2,y1,y2) {
  return((y2-y1)/(x2-x1))
}

# functie die de intercept geeft van de rechte lijn geeft gegeven de rc en een punt op de lijn
b <- function(x,y,rc) {
  return(y-(rc*x))
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
bakje430.480.max <- 6.15821 #bakje430.480$percentGDP[5] # 6.15821 # 

bakje480.530.min <- bakje480.530$percentGDP[1]
bakje480.530.max <- bakje480.530$percentGDP[5]

bakje530.580.min <- bakje530.580$percentGDP[1]
bakje530.580.max <- bakje530.580$percentGDP[5]

bakje580.650.min <- bakje580.650$percentGDP[1]
bakje580.650.max <- bakje580.650$percentGDP[5]

bakje650.720.min <- bakje650.720$percentGDP[1]
bakje650.720.max <- bakje650.720$percentGDP[5]



# rc tussen de bakjes
# max rc
rcmax.bakje430.480_480.530 <- rc(bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje430.480.max, bakje480.530.max)
rcmax.bakje480.530_530.580 <- rc(bakje480.530.deltaCO2, bakje530.580.deltaCO2, bakje480.530.max, bakje530.580.max)
rcmax.bakje530.580_580.650 <- rc(bakje530.580.deltaCO2, bakje580.650.deltaCO2, bakje530.580.max, bakje580.650.max)
rcmax.bakje580.650_650.720 <- rc(bakje580.650.deltaCO2, bakje650.720.deltaCO2, bakje580.650.max, bakje650.720.max)

# gem rc
rcmodus.bakje430.480_480.530 <- rc(bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje430.480.median, bakje480.530.median)
rcmodus.bakje480.530_530.580 <- rc(bakje480.530.deltaCO2, bakje530.580.deltaCO2, bakje480.530.median, bakje530.580.median)
rcmodus.bakje530.580_580.650 <- rc(bakje530.580.deltaCO2, bakje580.650.deltaCO2, bakje530.580.median, bakje580.650.median)
rcmodus.bakje580.650_650.720 <- rc(bakje580.650.deltaCO2, bakje650.720.deltaCO2, bakje580.650.median, bakje650.720.median)

# min rc
rcmin.bakje430.480_480.530 <- rc(bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje430.480.min, bakje480.530.min)
rcmin.bakje480.530_530.580 <- rc(bakje480.530.deltaCO2, bakje530.580.deltaCO2, bakje480.530.min, bakje530.580.min)
rcmin.bakje530.580_580.650 <- rc(bakje530.580.deltaCO2, bakje580.650.deltaCO2, bakje530.580.min, bakje580.650.min)
rcmin.bakje580.650_650.720 <- rc(bakje580.650.deltaCO2, bakje650.720.deltaCO2, bakje580.650.min, bakje650.720.min)


# min en max intercept
# min
b.min.bakje430.480_480.530 <- b(bakje430.480.deltaCO2,bakje430.480.min,rcmin.bakje430.480_480.530)
b.min.bakje480.530_530.580 <- b(bakje480.530.deltaCO2,bakje480.530.min,rcmin.bakje480.530_530.580)
b.min.bakje530.580_580.650 <- b(bakje530.580.deltaCO2,bakje530.580.min,rcmin.bakje530.580_580.650)
b.min.bakje580.650_650.720 <- b(bakje580.650.deltaCO2,bakje580.650.min,rcmin.bakje580.650_650.720)

# max
b.max.bakje430.480_480.530 <- b(bakje430.480.deltaCO2,bakje430.480.max,rcmax.bakje430.480_480.530)
b.max.bakje480.530_530.580 <- b(bakje480.530.deltaCO2,bakje480.530.max,rcmax.bakje480.530_530.580)
b.max.bakje530.580_580.650 <- b(bakje530.580.deltaCO2,bakje530.580.max,rcmax.bakje530.580_580.650)
b.max.bakje580.650_650.720 <- b(bakje580.650.deltaCO2,bakje580.650.max,rcmax.bakje580.650_650.720)




costs.oneRun <- function(deltaCO2) {
  # zit het onder bakje 430-480?
  if (deltaCO2 < bakje430.480.deltaCO2) {
    # return(c(-1,-1)) #return("lager dan bakje 430.480")
    
    kosten.median <- punt_rechteLijn(deltaCO2, bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje430.480.median, bakje480.530.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje430.480.min, bakje480.530.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje430.480.max, bakje480.530.max)
    
    rc.modus <- rcmodus.bakje430.480_480.530
    rc.max <- rcmax.bakje430.480_480.530
    rc.min <- rcmin.bakje430.480_480.530
    
    b.max <- b.max.bakje430.480_480.530
    b.min <- b.min.bakje430.480_480.530
    
    # zit het tussen bakje 430-480 en 480-530?
  } else if (deltaCO2 >= bakje430.480.deltaCO2 & deltaCO2 < bakje480.530.deltaCO2) {
    kosten.median <- punt_rechteLijn(deltaCO2, bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje430.480.median, bakje480.530.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje430.480.min, bakje480.530.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje430.480.max, bakje480.530.max)
    
    rc.modus <- rcmodus.bakje430.480_480.530
    rc.max <- rcmax.bakje430.480_480.530
    rc.min <- rcmin.bakje430.480_480.530
    
    b.max <- b.max.bakje430.480_480.530
    b.min <- b.min.bakje430.480_480.530
    
    # zit het tussen bakje 480-530 en 530-580?
  } else if (deltaCO2 >= bakje480.530.deltaCO2 & deltaCO2 < bakje530.580.deltaCO2) {
    kosten.median <- punt_rechteLijn(deltaCO2, bakje480.530.deltaCO2, bakje530.580.deltaCO2, bakje480.530.median, bakje530.580.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje480.530.deltaCO2, bakje530.580.deltaCO2, bakje480.530.min, bakje530.580.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje480.530.deltaCO2, bakje530.580.deltaCO2, bakje480.530.max, bakje530.580.max)
    
    rc.modus <- rcmodus.bakje480.530_530.580
    rc.max <- rcmax.bakje480.530_530.580
    rc.min <- rcmin.bakje480.530_530.580
    
    b.max <- b.max.bakje480.530_530.580
    b.min <- b.min.bakje480.530_530.580
    
    # zit het tussen bakje 530-580 en 580-650?
  } else if (deltaCO2 >= bakje530.580.deltaCO2 & deltaCO2 < bakje580.650.deltaCO2) {
    kosten.median <- punt_rechteLijn(deltaCO2, bakje530.580.deltaCO2, bakje580.650.deltaCO2, bakje530.580.median, bakje580.650.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje530.580.deltaCO2, bakje580.650.deltaCO2, bakje530.580.min, bakje580.650.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje530.580.deltaCO2, bakje580.650.deltaCO2, bakje530.580.max, bakje580.650.max)
    
    rc.modus <- rcmodus.bakje530.580_580.650
    rc.max <- rcmax.bakje530.580_580.650
    rc.min <- rcmin.bakje530.580_580.650
    
    b.max <- b.max.bakje530.580_580.650
    b.min <- b.min.bakje530.580_580.650
    
    # zit het tussen bakje 580-650 en 650-720?
  } else if (deltaCO2 >= bakje580.650.deltaCO2 & deltaCO2 <= bakje650.720.deltaCO2) {
    kosten.median <- punt_rechteLijn(deltaCO2, bakje580.650.deltaCO2, bakje650.720.deltaCO2, bakje580.650.median, bakje650.720.median)
    kosten.min <- punt_rechteLijn(deltaCO2, bakje580.650.deltaCO2, bakje650.720.deltaCO2, bakje580.650.min, bakje650.720.min)
    kosten.max <- punt_rechteLijn(deltaCO2, bakje580.650.deltaCO2, bakje650.720.deltaCO2, bakje580.650.max, bakje650.720.max)
    
    rc.modus <- rcmodus.bakje580.650_650.720
    rc.max <- rcmax.bakje580.650_650.720
    rc.min <- rcmin.bakje580.650_650.720
    
    b.max <- b.max.bakje580.650_650.720
    b.min <- b.min.bakje580.650_650.720
    
  } else  if (deltaCO2 > bakje650.720.deltaCO2) {
    return(c(-1,-1)) #return("hoger dan bakje 650-720")
  }
  
  grootte <- kosten.max - kosten.min
  trekking_kosten <- rpert(1, min = kosten.min, mode = kosten.median, max = kosten.max)
  
  
  cs <- rpert(1, min = rc.min, mode = rc.modus, max = rc.max, shape = 4)
  
  verhouding <- (rc.max - cs)/(rc.max-rc.min)
  
  intercept <- b.max - verhouding*(b.max-b.min)
  
  kost <- deltaCO2*cs + intercept
  
  #print(verhouding)
  #print(intercept)
  #print(kost)
  
  return(c(cs,kost))
}


#-------------- run model -------------------------------

model.costs <- function(deltaCO2) {
  
  # run model
  #cs_en_costs <- mapply(costs.oneRun, deltaCO2)
  
  # initize
  cs_en_costs <- data.frame('cs'= 1:N, 'kosten.result' = 1:N)
  
  for (i in 1:N) {
    cs_en_costs[i,] <- costs.oneRun(deltaCO2[i])
  }
  
  return(data.frame(cs_en_costs))
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
  
  #cs <-kosten.result/sample_en_result.deltaCO2$cumuCO2result
  
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
    CCmatrixP <- rbind(CCmatrixP, CCmatrixP.hulp[-6,7])
    # Spearman CC:
    CCmatrixS.hulp <- cor(sample_en_result.kosten, method = "spearman")[-1,]
    CCmatrixS <- rbind(CCmatrixS, CCmatrixS.hulp[-6,7])
    
    teller <- teller + 1
  }
  rownames(CCmatrixP) <- as.character(seq(1.2, 3.4, by = 0.1))
  rownames(CCmatrixS) <- as.character(seq(1.2, 3.4, by = 0.1))
  
  return(list(CCmatrixP,CCmatrixS))
}

# maak een matrix van CCwaarden
#CCmat <- f.costs.CCmatrix(N,s.seed)
