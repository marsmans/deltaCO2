#-------------------------------------------
# 
# Dummymodel met vrij onzekere waarden
# Sensitivity in TCRE
#
#-------------------------------------------
# Punten zijn ingelezen met http://arohatgi.info/WebPlotDigitizer/app/
# 

# Den Haag
#setwd("~/disks/y/ontwapps/Timer/Users/Stijn/Model/modelTt")
# Thuis
#setwd("~/Documenten/Stage PBL/modelTt")

source("packages.R")

#----------- Relatie cumulatieve CO2 <-> temperatuur -----------------

# data inlezen
cumuvstempLL <- read.csv(file = "./../Databases/cumuvstemp_lowerlimit.txt", header = TRUE)
cumuvstempUL <- read.csv(file = "./../Databases/cumuvstemp_upperlimit.txt", header = TRUE)


# schalen naar Tt
cumuvstempLL[1] <- cumuvstempLL[1]/1000
cumuvstempUL[1] <- cumuvstempUL[1]/1000

# rechte lijn best fits
fLL <- lm(data = cumuvstempLL, temp ~ cumuCO2)
fUL <- lm(data = cumuvstempUL, temp ~ cumuCO2)

# "gemiddelde" lijn
intercept = (coef(fLL)[1] + coef(fUL)[1])/2
slope = (coef(fLL)[2] + coef(fUL)[2])/2

TCREmean <- slope

# Als onzekerheidsrange [10%,90%] is
TCREstd90 <- (coef(fUL)[2] - slope)/abs(qnorm(0.90))
TCREstd10 <- (slope - coef(fLL)[2])/abs(qnorm(0.10))
TCREstd = (TCREstd10 + TCREstd90)/2

# Als onzekerheidsrange [5%,95%] is
TCREstd95 <- (coef(fUL)[2] - slope)/abs(qnorm(0.95))
TCREstd05 <- (slope - coef(fLL)[2])/abs(qnorm(0.05))
TCREstd2 = (TCREstd05 + TCREstd95)/2


#------------- Temp 2010 Hans Visser --------------------
# artikel Hans Visser: 
# T2016: 1.01 +- 0.13 (2sigma)
# verschil in HadCrut + IRW tussen T2016 en T2010: 0.1151
# Neem dan voor T2010: 1.01 - 0.115 = 0.90 +- 0.13 (2sigma)
# gemiddelde: 0.90
# standaardafwijking: 0.13/2 = 0.065

T2010mean <- 0.90
T2010std <- 0.065


#------------- non-CO2 ----------------------------------

# nonCO2low+up bevat de punten op de ellips boven en onder het middelpunt van de ellips
nonCO2temp <- read.csv(file = "./../Databases/nonCO2low+up.txt", header = TRUE)
# nonCO2temps geeft het hoogste en laagste punt van de ellips
#nonCO2temp <- read.csv(file = "./../Databases/nonCO2temps.txt", header = TRUE)

nonCO2.upperbounds <- (nonCO2temp$uppertemp - nonCO2temp$lowertemp)/2

# rechte lijn door upperbounds
# nonCO2lijn <- lm(nonCO2.upperbounds ~ nonCO2temp$cumuCO2)

#Deze gaat niet door 0!
#plot(nonCO2.upperbound~nonCO2temp$cumuCO2)
#abline(nonCO2lijn)
#coef(nonCO2lijn)
# dan nemen we het gemiddelde van alle 6 lijnen door 0 en een upperbound

# 6 ricos van 0 naar andere punten
#nonCO2ricos <- (nonCO2.upperbounds)/nonCO2temp$cumuCO2

# maar: hij hoeft ook niet door 0 te gaan, sterker nog, hij gaat door de waarde van de ellips van 430-480:

# We nemen aan: in 2010 is de afwijking door nonCO2 gelijk aan die in de ellips van 430-480
nonCO22010max <- nonCO2.upperbounds[1]

# 5 richtingscoefficienten van nonCO22010max naar andere punten: !!! niet correct! !!!
#nonCO2ricos <- (nonCO2.upperbounds - nonCO22010max)/nonCO2temp$cumuCO2

# 5 richtingscoefficienten van nonCO22010max op cumuCO2 (2010-2100) = 0, en dus op cumuCO2 (2010-2100) = 1.339, naar andere punten
cumuCO22010.rico <- 1.339
nonCO2ricos <- (nonCO2.upperbounds - nonCO22010max)/(nonCO2temp$cumuCO2 - cumuCO22010.rico)


# gemiddelde van 5 of 6 richtingscoefficienten:
#TCRnonCO2max <- mean(nonCO2ricos)
TCRnonCO2max2 <- mean(nonCO2ricos[-1])
TCRnonCO2max <- mean(nonCO2ricos)

# om te zorgen dat als nonCO22010 op een maximale waarde wordt gesampled, TCRnonCO2 ook maximaal is (om een scheve verdeling te vermijden)
# nemen we een relatie aan tussen nonCO22010 en TCRnonCO2
# nonCO22010 = a*TCRnonCO2 + b
# we weten: nonCO22010 = 0 => TCRnonCO2 = 0
# en: nonCO22010 = nonCO22010max => TCRnonCO2 = TCRnonCO2max
# dus b = 0 en a = nonCO22010max/TCRnonCO2max

a.nonCO2 <- nonCO22010max/TCRnonCO2max

# afwijking door nonCO2 schommelt rond 0
nonCO2mean <- 0

#----------- Maak sample ----------------------
# Zie ook http://r.789695.n4.nabble.com/Latin-Hypercube-Sampling-with-a-condition-td3563765.html
# en https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2570191/

# maakt een sample van T2010, TCRE en CO22010
f.cumuvstemp.sample <- function(N, f.seed) {
  require(lhs)
  
  # maak random LHS
  set.seed(f.seed)
  x <- randomLHS(N, 3)
  # geef namen
  colnames(x) <- c("T2010", "TCRE", "TCRnonCO2")
  
  # transformeer random LHS naar LHS met goede parameters
  T2010 <- qnorm(x[,1], mean=T2010mean, sd=T2010std)
  TCRE <- qpert(x[,2], coef(fLL)[2], TCREmean, coef(fUL)[2], shape = 4)
  nonCO2 <- qpert(x[,3], -1*TCRnonCO2max, nonCO2mean, TCRnonCO2max, shape = 4)
  
  
  # bundel in dataframe
  return(data.frame(T2010,TCRE,nonCO2))
}



#----------- Define model ---------------------

oneRun <- function(Ttarget,T2010,TCRE,TCRnonCO2) {
  return((Ttarget - T2010 + a.nonCO2*TCRnonCO2)/(TCRE + TCRnonCO2))
}


#--------------- Run model  ---------

# functie om cumuCO2 uit te rekenen aan de hand van een gegeven Ttarget 
# en gegeven sample voor T2010, TCRE

f.cumuCO2result <- function(N, Ttarget, sample) {
  f.Ttarget <- rep(Ttarget, N)
  # run model
  cumuCO2result <- mapply(oneRun, f.Ttarget, sample[,1], sample[,2], sample[,3])
  # plak resultaat aan sample
  return(data.frame(f.Ttarget, sample, cumuCO2result))
}

#-------------- CCmatrix -------------

N <- 10000
s.seed <- 21

# functie die voor waarden van Ttarget tussen 1 en 4 de Correlation Coefficent uitrekent tussen de inputparameters en cumuCO2, de model uitkomst
f.CCmatrix <- function(N,f.seed) {
  # initialisatie
  CCmatrix <- NULL
  teller <- 0
  
  cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
  
  for (i in seq(1, 4, by = 0.1)) {
    # print(i)
    sample_en_result <- f.cumuCO2result(N,i,cumuvstemp.sample)
    CCmatrix.hulp <- cor(sample_en_result)[-1,]
    CCmatrix <- rbind(CCmatrix, CCmatrix.hulp[-4,5])
    
    teller <- teller + 1
  }
  rownames(CCmatrix) <- as.character(seq(1, 4, by = 0.1))
  
  return(CCmatrix)
}




