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
# Neem dan voor T2010: 1.01 - 0.115 = 0.895 +- 0.13 (2sigma)
# gemiddelde: 0.895
# standaardafwijking: 0.13/2 = 0.065

T2010mean <- 0.895
T2010std <- 0.065


#------------- non-CO2 ----------------------------------

# data van excel sheet Detlef
nonCO2ssp <- read.csv(file = "./../Databases/ssp_data_update_naar_R.csv", header = TRUE, sep = ";")
#nonCO2ssp <- read.csv(file = "./../Databases/ssp_data_update_R_no1,5.csv", header = TRUE, sep = ";")
#alleen1.5 <- read.csv(file = "./../Databases/ssp_data_update_R_alleen1,5.csv", header = TRUE, sep = ";")


# schaal Gt naar Tt
nonCO2ssp$CumCO2..GtCO2. <- nonCO2ssp$CumCO2..GtCO2./1000

# nonCO2 forcing vs deltaCO2
#s = ggplot(nonCO2ssp,aes(x=CumCO2..GtCO2.,y=F_nonCO2))
#s = s + geom_point()
#s = s + theme_bw()
#s = s + labs(x = expression(Cumulative~carbon~emissions~(2010-2100)~(TtCO[2])), y = expression(non-CO[2]~forcing~(W/m^2)))
#s = s + coord_cartesian(xlim = c(0,8.5), ylim = c(0,1.8))
#s

#Ftot vs temp
#s = ggplot(nonCO2ssp,aes(x=F_tot,y=Temp...C.))
#s = s + geom_point()
#s = s + theme_bw()
#s = s + labs(x = expression(Total~forcing~(W/m^2)), y = expression(Temperature~change~relative~to~p.i.~( degree*C)))
#s = s + coord_cartesian(xlim = c(0,9), ylim = c(0,5.5))
#s

#alleen1.5$CumCO2..GtCO2. <- alleen1.5$CumCO2..GtCO2./1000

#reken fractie uit
fractie.F_nonCO2F_tot <- nonCO2ssp$F_nonCO2/nonCO2ssp$F_tot
tempStijging_door_F_nonCO2 <- fractie.F_nonCO2F_tot*nonCO2ssp$Temp...C.

#fractie.alleen1.5 <- alleen1.5$F_nonCO2/alleen1.5$F_tot
#tempStijging.alleen1.5 <- fractie.alleen1.5*alleen1.5$Temp...C.

#TFnonCO2 vs deltaCO2
nonCO2ssp <- cbind(nonCO2ssp,tempStijging_door_F_nonCO2)

#s = ggplot(nonCO2ssp,aes(x=CumCO2..GtCO2.,y=tempStijging_door_F_nonCO2))
#s = s + geom_point()
#s = s + theme_bw()
#s = s + labs(x = expression(Cumulative~carbon~emissions~(2010-2100)~(TtCO[2])), y = expression(Temperature~change~relative~to~p.i.~( degree*C)))
#s = s + coord_cartesian(xlim = c(0,8.5), ylim = c(0,1.2))
#s

# reken gemiddelde uit
T_tot.avg <- (sum(nonCO2ssp$Temp...C.))/length(nonCO2ssp$Temp...C.)
F_tot.avg <- (sum(nonCO2ssp$F_tot))/length(nonCO2ssp$F_tot)

# reken fractie uit met gemiddelde verhouding
tempStijging_door_F_nonCO2.met.avg <- nonCO2ssp$F_nonCO2 * (T_tot.avg/F_tot.avg)

#TFnonCO2 vs deltaCO2
#nonCO2ssp <- cbind(nonCO2ssp,tempStijging_door_F_nonCO2)
nonCO2ssp <- cbind(nonCO2ssp,tempStijging_door_F_nonCO2.met.avg)
nonCO2ssp.gather <- gather(nonCO2ssp, variable, value)
nonCO2ssp.gather$deltaCO2 <- nonCO2ssp$CumCO2..GtCO2.
nonCO2ssp.gather <- data.table(nonCO2ssp.gather)

s <- ggplot(nonCO2ssp.gather[variable %in% c('tempStijging_door_F_nonCO2','tempStijging_door_F_nonCO2.met.avg')])
s = s + geom_point(aes(x=deltaCO2,y=value, shape=variable),stat="identity")
s = s + theme_bw()
#s = s + scale_color_manual(values=c("tempStijging_door_F_nonCO2"="blue","tempStijging_door_F_nonCO2.met.avg"="dark green"),
s = s + scale_shape_manual(values=c("tempStijging_door_F_nonCO2"=1,"tempStijging_door_F_nonCO2.met.avg"=4), #if you want shapes
                           labels=c("Method 1","Method 2"))
s = s + guides(shape=guide_legend(title=NULL))
s = s + theme(legend.justification=c(0.9,0.1), legend.position=c(0.9,0.1), 
              legend.text = element_text(size = 14))
s = s + labs(x = expression(Cumulative~carbon~emissions~(2010-2100)~(TtCO[2])), y = expression(Temperature~change~relative~to~p.i.~( degree*C)))
s = s + coord_cartesian(xlim = c(0,8.5), ylim = c(0,1.2))
s

# nieuwe poging:
#nonCO2ssp2 <- data.frame(nonCO2ssp$CumCO2..GtCO2.,nonCO2ssp$tempStijging_door_F_nonCO2,nonCO2ssp$tempStijging_door_F_nonCO2.met.avg)

#s2 <- ggplot(nonCO2ssp2)
#s2 = s2 + geom_point(aes(x=nonCO2ssp.CumCO2..GtCO2.,y=nonCO2ssp.tempStijging_door_F_nonCO2,col="blue"))
#s2 = s2 + geom_point(aes(x=nonCO2ssp.CumCO2..GtCO2.,y=nonCO2ssp.tempStijging_door_F_nonCO2.met.avg,col="yellow"))
#s2 = s2 + scale_fill_manual(values=c("nonCO2ssp.tempStijging_door_F_nonCO2"="blue","nonCO2ssp.tempStijging_door_F_nonCO2.met.avg"="yellow"))
#s2


# kijken hoe groot de bandbreedte van nonCO2 is:
#plot(tempStijging_door_F_nonCO2~nonCO2ssp$CumCO2..GtCO2., main = "Temperatuurstijging door F_nonCO2", xlab = "deltaCO2 (TtCO2)", ylab = "T (*C)")
#points(tempStijging.alleen1.5~alleen1.5$CumCO2..GtCO2., pch = 4, col = "blue" )
#fitlijn <- lm(data = nonCO2ssp, tempStijging_door_F_nonCO2 ~ CumCO2..GtCO2.)
#abline(fitlijn)

# kijken hoe groot de bandbreedte van nonCO2 is met gemiddelde verhouding:
plot(tempStijging_door_F_nonCO2.met.avg~nonCO2ssp$CumCO2..GtCO2., main = "Temperatuurstijging door F_nonCO2 (gem. verh.)", xlab = "deltaCO2 (TtCO2)", ylab = "T (*C)")
fitlijn <- lm(data = nonCO2ssp, tempStijging_door_F_nonCO2.met.avg ~ CumCO2..GtCO2.)
abline(fitlijn)

t <- ggplot(nonCO2ssp.gather[variable %in% c('tempStijging_door_F_nonCO2.met.avg')])
t = t + geom_point(aes(x=deltaCO2,y=value),stat="identity")
t = t + geom_smooth(aes(x=deltaCO2,y=value),method = 'lm',formula = y~x, se=F)
t = t + geom_abline(intercept = coef(fitlijn)[1] - afwijking_nonCO2, slope = coef(fitlijn)[2], color="red")
t = t + geom_abline(intercept = coef(fitlijn)[1] + afwijking_nonCO2, slope = coef(fitlijn)[2], color="red")
t = t + theme_bw()
t = t + labs(x = expression(Cumulative~carbon~emissions~(2010-2100)~(TtCO[2])), y = expression(Temperature~change~relative~to~p.i.~( degree*C)))
t

# hij ligt steeds maximaal ongeveer 0.2 onder of bover de best-fit-lijn, dus
afwijking_nonCO2 <- 0.23

abline(b = coef(fitlijn)[2], a = coef(fitlijn)[1] - afwijking_nonCO2, col = "red")
abline(b = coef(fitlijn)[2], a = coef(fitlijn)[1] + afwijking_nonCO2, col = "red")

# hellingshoek best-fit-lijn
TCRnonCO2 <- coef(fitlijn)[2]

# intercept best-fit-lijn
TF2010 <- coef(fitlijn)[1]


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
  nonCO2 <- qpert(x[,3], -1*afwijking_nonCO2 + TF2010, TF2010, afwijking_nonCO2 + TF2010, shape = 4) # gemiddelde niet rond 0 maar rond de 2010 waarde!
  
  
  # bundel in dataframe
  return(data.frame(T2010,TCRE,nonCO2))
}



#----------- Define model ---------------------

oneRun <- function(Ttarget,T2010,TCRE,nonCO2) {
  return((Ttarget - T2010 + nonCO2)/(TCRE + TCRnonCO2))
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

f.CCmatrix <- function(N,f.seed) {
  # initialisatie
  CCmatrixP <- NULL
  CCmatrixS <- NULL
  teller <- 0
  
  cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
  
  for (i in seq(1, 4, by = 0.1)) {
    # print(i)
    sample_en_result <- f.cumuCO2result(N,i,cumuvstemp.sample)
    # pearson CC:
    CCmatrixP.hulp <- cor(sample_en_result)[-1,]
    CCmatrixP <- rbind(CCmatrixP, CCmatrixP.hulp[-4,5])
    # Spearman CC:
    CCmatrixS.hulp <- cor(sample_en_result, method = "spearman")[-1,]
    CCmatrixS <- rbind(CCmatrixS, CCmatrixS.hulp[-4,5])
    
    teller <- teller + 1
  }
  rownames(CCmatrixP) <- as.character(seq(1, 4, by = 0.1))
  rownames(CCmatrixS) <- as.character(seq(1, 4, by = 0.1))
  
  return(list(CCmatrixP,CCmatrixS))
}


# alleen Pearson:
# functie die voor waarden van Ttarget tussen 1 en 4 de Correlation Coefficent uitrekent tussen de inputparameters en cumuCO2, de model uitkomst
# f.CCmatrix <- function(N,f.seed) {
#   # initialisatie
#   CCmatrix <- NULL
#   teller <- 0
#   
#   cumuvstemp.sample <- f.cumuvstemp.sample(N,f.seed)
#   
#   for (i in seq(1, 4, by = 0.1)) {
#     # print(i)
#     sample_en_result <- f.cumuCO2result(N,i,cumuvstemp.sample)
#     CCmatrix.hulp <- cor(sample_en_result)[-1,]
#     CCmatrix <- rbind(CCmatrix, CCmatrix.hulp[-4,5])
#     
#     teller <- teller + 1
#   }
#   rownames(CCmatrix) <- as.character(seq(1, 4, by = 0.1))
#   
#   return(CCmatrix)
# }
# 



