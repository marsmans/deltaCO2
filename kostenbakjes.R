#----------------------------------------------------
#
# Model met bakjes
#
#----------------------------------------------------



# functie om het punt op de rechte lijn tussen twee punten uit te rekenen
punt_rechteLijn <- function(x, x.links, x.rechts, y.onder, y.boven) {
  q <- (y.boven - y.onder)/(x.rechts - x.links)
  return(y.onder + (x - x.links)*q)
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


costs <- function(deltaCO2) {
  # zit het onder bakje 430-480?
  if (deltaCO2 < bakje430.480.deltaCO2) {
    kosten.mean <- "lager dan bakje 430-480"
  # zit het tussen bakje 430-480 en 480-530?
  } else if (deltaCO2 >= bakje430.480.deltaCO2 & deltaCO2 < bakje480.530.deltaCO2) {
    kosten.mean <- punt_rechteLijn(deltaCO2, bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje430.480.median, bakje480.530.median)
  # zit het tussen bakje 480-530 en 530-580?
  } else if (deltaCO2 >= bakje480.530.deltaCO2 & deltaCO2 < bakje530.580.deltaCO2) {
    kosten.mean <- punt_rechteLijn(deltaCO2, bakje480.530.deltaCO2, bakje530.580.deltaCO2, bakje480.530.median, bakje530.580.median)
  # zit het tussen bakje 530-580 en 580-650?
  } else if (deltaCO2 >= bakje530.580.deltaCO2 & deltaCO2 < bakje580.650.deltaCO2) {
    kosten.mean <- punt_rechteLijn(deltaCO2, bakje530.580.deltaCO2, bakje580.650.deltaCO2, bakje530.580.median, bakje580.650.median)
  # zit het tussen bakje 580-650 en 650-720?
  } else if (deltaCO2 >= bakje580.650.deltaCO2 & deltaCO2 < bakje650.720.deltaCO2) {
    kosten.mean <- punt_rechteLijn(deltaCO2, bakje580.650.deltaCO2, bakje650.720.deltaCO2, bakje580.650.median, bakje650.720.median)
  } else 
    kosten.mean <- "hoger dan bakje 650-720"
    
  return(kosten.mean)
}

