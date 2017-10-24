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

data1.5.test <- subset(data1.5, select = -f.Ttarget)
data2.test <- subset(data2, select = -f.Ttarget)
data3.test <- subset(data3, select = -f.Ttarget)

# Deze is goed volgens mij!:
data1.5.test <- melt(data1.5.test, id.vars = 'kosten.result', variable.name = 'parameter')
data1.5.test$Ttarget <- 1.5

data2.test <- melt(data2.test, id.vars = 'kosten.result', variable.name = 'parameter')
data2.test$Ttarget <- 2

data3.test <- melt(data3.test, id.vars = 'kosten.result', variable.name = 'parameter')
data3.test$Ttarget <- 3

data1.523.test <- rbind(data1.5.test,data2.test,data3.test)
data1.523.test <- data.table(data1.523.test)

# test plot
sp <- ggplot(data1.523.test[parameter %in% c('T2010','TCRE','nonCO2','sampletrans01')], aes(x=value, y=kosten.result))
sp = sp + geom_point()
sp = sp + facet_grid(parameter ~ Ttarget) #, scales="free", space="free"
sp

# test plot omgedraaid
ps <- ggplot(data1.523.test[parameter %in% c('T2010','TCRE','nonCO2','sampletrans01')], aes(x=value, y=kosten.result))
ps = ps + geom_point()
ps = ps + facet_grid(Ttarget ~ parameter) #, scales="free", space="free"
ps


colnames(data1.5) <- paste("1.5:",colnames(data1.5))
colnames(data2) <- paste("2:",colnames(data2))
colnames(data3) <- paste("3:",colnames(data3))

data1.523 <- cbind(data1.5,data2,data3)
data1.523 <- data.table(data1.523)



# maak plots 1
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


#----------------------------------------
# maak plots 2, y-as niet steeds hetzelfde, 1 keer asnamen
T20101.5 <- ggplot(data = data1.523)
T20101.5 <- T20101.5 + geom_point(aes(x = data1.523$`1.5: T2010`, y = data1.523$`1.5: kosten.result`))
T20101.5 <- T20101.5 + theme_bw()
T20101.5 <- T20101.5 + labs(x=NULL,y = NULL)
T20101.5 <- T20101.5 + coord_cartesian(ylim = c(0, 6.7))
T20101.5
ggsave(paste("SPT201015AR5.png"),T20101.5)

T20102 <- ggplot(data = data1.523)
T20102 <- T20102 + geom_point(aes(x = data1.523$`2: T2010`, y = data1.523$`2: kosten.result`))
T20102 <- T20102 + theme_bw()
T20102 <- T20102 + labs(x=NULL,y = NULL) #labs(y = NULL) #x = expression(Temperature~relative~to~p.i.~( degree*C))
T20102 <- T20102 + coord_cartesian(ylim = c(0, 2.9))
T20102
ggsave(paste("SPT20102AR5.png"),T20102)

T20103 <- ggplot(data = data1.523)
T20103 <- T20103 + geom_point(aes(x = data1.523$`3: T2010`, y = data1.523$`3: kosten.result`))
T20103 <- T20103 + theme_bw()
T20103 <- T20103 + labs(x=NULL,y = NULL) #labs(y = NULL) # x = "", 
T20103 <- T20103 + coord_cartesian(ylim = c(0, 0.4))
T20103
ggsave(paste("SPT20103AR5.png"),T20103)

TCRE1.5 <- ggplot(data = data1.523)
TCRE1.5 <- TCRE1.5 + geom_point(aes(x = data1.523$`1.5: TCRE`, y = data1.523$`1.5: kosten.result`))
TCRE1.5 <- TCRE1.5 + theme_bw()
TCRE1.5 <- TCRE1.5 + labs(x=NULL,y = NULL) #labs(y = "") #(x = "", 
TCRE1.5 <- TCRE1.5 + coord_cartesian(ylim = c(0,6.7))
TCRE1.5
ggsave(paste("SPTCRE15AR5.png"),TCRE1.5)


TCRE2 <- ggplot(data = data1.523)
TCRE2 <- TCRE2 + geom_point(aes(x = data1.523$`2: TCRE`, y = data1.523$`2: kosten.result`))
TCRE2 <- TCRE2 + theme_bw()
TCRE2 <- TCRE2 + labs(x=NULL,y = NULL) #labs(y = NULL) #x = expression(TCRE~( degree*C/TtCO[2]))
TCRE2 <- TCRE2 + coord_cartesian(ylim = c(0, 2.9))
TCRE2
ggsave(paste("SPTCRE2AR5.png"),TCRE2)

TCRE3 <- ggplot(data = data1.523)
TCRE3 <- TCRE3 + geom_point(aes(x = data1.523$`3: TCRE`, y = data1.523$`3: kosten.result`))
TCRE3 <- TCRE3 + theme_bw()
TCRE3 <- TCRE3 + labs(x=NULL,y = NULL) #labs( y = NULL) #x = "",
TCRE3 <- TCRE3 + coord_cartesian(ylim = c(0, 0.4))
TCRE3
ggsave(paste("SPTCRE3AR5.png"),TCRE3)

FnCO21.5 <- ggplot(data = data1.523)
FnCO21.5 <- FnCO21.5 + geom_point(aes(x = data1.523$`1.5: nonCO2`, y = data1.523$`1.5: kosten.result`))
FnCO21.5 <- FnCO21.5 + theme_bw()
FnCO21.5 <- FnCO21.5 + labs(x=NULL,y = NULL) #labs(y = "Mitigation Costs (%GDP)") #x = "", 
FnCO21.5 <- FnCO21.5 + coord_cartesian(ylim = c(0, 6.7))
FnCO21.5
ggsave(paste("SPFnCO215AR5.png"),FnCO21.5)

FnCO22 <- ggplot(data = data1.523)
FnCO22 <- FnCO22 + geom_point(aes(x = data1.523$`1.5: nonCO2`, y = data1.523$`2: kosten.result`))
FnCO22 <- FnCO22 + theme_bw()
FnCO22 <- FnCO22 + labs(x=NULL,y = NULL) #labs(y = NULL) #x = expression(Temperature~relative~to~p.i.~( degree*C)), 
FnCO22 <- FnCO22 + coord_cartesian(ylim = c(0, 2.9))
FnCO22
ggsave(paste("SPFnCO22AR5.png"),FnCO22)

FnCO23 <- ggplot(data = data1.523)
FnCO23 <- FnCO23 + geom_point(aes(x = data1.523$`1.5: nonCO2`, y = data1.523$`3: kosten.result`))
FnCO23 <- FnCO23 + theme_bw()
FnCO23 <- FnCO23 + labs(x=NULL,y = NULL) #labs(y = NULL) #x = "", 
FnCO23 <- FnCO23 + coord_cartesian(ylim = c(0, 0.4))
FnCO23
ggsave(paste("SPFnCO23AR5.png"),FnCO23)

t1.5 <- ggplot(data = data1.523)
t1.5 <- t1.5 + geom_point(aes(x = data1.523$`1.5: sampletrans01`, y = data1.523$`1.5: kosten.result`))
t1.5 <- t1.5 + theme_bw()
t1.5 <- t1.5 + labs(x=NULL,y = NULL) #labs(y = "") #x = "", 
t1.5 <- t1.5 + coord_cartesian(ylim = c(0, 6.7))
t1.5
ggsave(paste("SPt15AR5.png"),t1.5)

t2 <- ggplot(data = data1.523)
t2 <- t2 + geom_point(aes(x = data1.523$`1.5: sampletrans01`, y = data1.523$`2: kosten.result`))
t2 <- t2 + theme_bw()
t2 <- t2 + labs(x=NULL,y = NULL) #labs(y = NULL) #x = "t", 
t2 <- t2 + coord_cartesian(ylim = c(0, 2.9))
t2
ggsave(paste("SPt2AR5.png"),t2)

t3 <- ggplot(data = data1.523)
t3 <- t3 + geom_point(aes(x = data1.523$`1.5: sampletrans01`, y = data1.523$`3: kosten.result`))
t3 <- t3 + theme_bw()
t3 <- t3 + labs(x=NULL,y = NULL) #labs( y = NULL) #x = "" ,
t3 <- t3 + coord_cartesian(ylim = c(0, 0.4))
t3
ggsave(paste("SPt3AR5.png"),t3)



source("multiplot.R")
# 3x4
#multiplot(T20101.5, T20102, T20103, TCRE1.5, TCRE2, TCRE3, FnCO21.5, FnCO22, FnCO23, t1.5, t2, t3, layout = matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), nrow=3))
# 4x3
multiplot(T20101.5, T20102, T20103, TCRE1.5, TCRE2, TCRE3, FnCO21.5, FnCO22, FnCO23, t1.5, t2, t3, layout = matrix(c(1,4,7,10,2,5,8,11,3,6,9,12), nrow=4))

# 1x3
multiplot(T20101.5, T20102, T20103, layout = matrix(c(1,2,3), nrow=1))

