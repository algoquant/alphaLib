library(xts)
library(TTR)
library(PerformanceAnalytics)
library(quantmod)

# library(ROneTick)
# oneTickLib()

# root.dir <- "C:/Develop/"
data.dir <- "C:/Data/"
alpha.dir <- "C:/Develop/GIThub/alphaLib/"
rmodels.dir <- "C:/Develop/GIThub/Rmodels/"
# otq.dir <- "../../OneTick/"
# jr.dir <- "../JR/"
# filter.dir <- "../BetaFilters/"

source(paste(alpha.dir, "alphaModel.R", sep=""))
source(paste(alpha.dir, "utilLib.R", sep=""))
# source(paste(jr.dir, "sgfilter.R", sep=""))
# source(paste(rmodels.dir, "dataLib.R", sep=""))
source(paste(rmodels.dir, "chartLib.R", sep=""))
source(paste(rmodels.dir, "riskLib.R", sep=""))
source(paste(rmodels.dir, "optimLib.R", sep=""))
# source(paste(filter.dir, "Filt.RobF.Signal.HL.R", sep=""))
# source(paste(filter.dir, "Filt.Rob.Signal.R", sep=""))
# source(paste(filter.dir, "rfUtil.R", sep=""))
