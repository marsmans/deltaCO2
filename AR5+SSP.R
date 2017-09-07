#-------------------------------------------------------------

# bakjes AR5 samenvoegen met bakjes SSP

#-------------------------------------------------------------


source("kostenbakjes.R")
source("kostenSSP.R")

# krijg AR5 bakjes
AR5bakjes <- data.frame(bakje430.480.deltaCO2, bakje430.480.min, bakje430.480.median, bakje430.480.max)
AR5bakjes <- rbind(AR5bakjes, c(bakje480.530.deltaCO2, bakje480.530.min, bakje480.530.median, bakje480.530.max))
AR5bakjes <- rbind(AR5bakjes, c(bakje530.580.deltaCO2, bakje530.580.min, bakje530.580.median, bakje530.580.max))
AR5bakjes <- rbind(AR5bakjes, c(bakje580.650.deltaCO2, bakje580.650.min, bakje580.650.median, bakje580.650.max))
AR5bakjes <- rbind(AR5bakjes, c(bakje650.720.deltaCO2, bakje650.720.min, bakje650.720.median, bakje650.720.max))
AR5bakjes <- rbind(AR5bakjes, c(bakjeNoCosts.deltaCO2, bakjeNoCosts.min, bakjeNoCosts.median, bakjeNoCosts.max))
colnames(AR5bakjes) <- c("deltaCO2", "min", "median", "max")

# andere manier
#AR5bakjes <- data.frame(deltaCO2 = c(bakje430.480.deltaCO2, bakje480.530.deltaCO2, bakje530.580.deltaCO2, bakje580.650.deltaCO2, bakje650.720.deltaCO2, bakjeNoCosts.deltaCO2),
#                        min = c(bakje430.480.min, bakje480.530.min, bakje530.580.min, bakje580.650.min, bakje650.720.min, bakjeNoCosts.min),
#                        mediaan = c(bakje430.480.median, bakje480.530.median, bakje530.580.median, bakje580.650.median, bakje650.720.median, bakjeNoCosts.median),
#                        max = c(bakje430.480.max, bakje480.530.max, bakje530.580.max, bakje580.650.max, bakje650.720.max, bakjeNoCosts.max))


# krijg SSP bakjes
SSPbakjes <- data.frame(bakje1.deltaCO2, bakje1.min, bakje1.median, bakje1.max)
SSPbakjes <- rbind(SSPbakjes, c(bakje2.deltaCO2, bakje2.min, bakje2.median, bakje2.max))
SSPbakjes <- rbind(SSPbakjes, c(bakje3.deltaCO2, bakje3.min, bakje3.median, bakje3.max))
SSPbakjes <- rbind(SSPbakjes, c(bakje4.deltaCO2, bakje4.min, bakje4.median, bakje4.max))
SSPbakjes <- rbind(SSPbakjes, c(bakje5.deltaCO2, bakje5.min, bakje5.median, bakje5.max))
SSPbakjes <- rbind(SSPbakjes, c(bakje6.deltaCO2, bakje6.min, bakje6.median, bakje6.max))
SSPbakjes <- rbind(SSPbakjes, c(bakjeNoCostsSSP.deltaCO2, bakjeNoCostsSSP.min, bakjeNoCostsSSP.median, bakjeNoCostsSSP.max))
colnames(SSPbakjes) <- c("deltaCO2", "min", "median", "max")


# plot ze beide apart
require(reshape2)

AR5bakjes.melt <- melt(AR5bakjes ,  id.vars = 'deltaCO2', variable.name = 'minmedmax')
SSPbakjes.melt <- melt(SSPbakjes ,  id.vars = 'deltaCO2', variable.name = 'minmedmax')

ggplot(AR5bakjes.melt, aes(deltaCO2,value)) + geom_line(aes(colour = minmedmax))
ggplot(SSPbakjes.melt, aes(deltaCO2,value)) + geom_line(aes(colour = minmedmax))


AR5_SSPbakjes <- rbind(AR5bakjes, SSPbakjes)




# plot ze beide in 1

AR5bakjes.melt$group <- "AR5"
SSPbakjes.melt$group <- "SSP"

AR5_SSPbakjes.melt <- rbind(AR5bakjes.melt, SSPbakjes.melt)

p <- ggplot(AR5_SSPbakjes.melt, aes(x=deltaCO2, y=value, group=interaction(minmedmax,group), colour=group, shape=minmedmax)) #col=interaction(minmedmax,group)
p <- p + geom_point()
p <- p + geom_line() #aes(x=deltaCO2, y=value, group=minmedmax, colour = minmedmax)
#p <- p + geom_smooth(size=1)
p


