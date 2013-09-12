########################
###  alphaModel
########################
### Comments
# alphaModel object contains:
# scalar data:  func.signal name and parameters, threshold parameters list, and trading limits list
# Time series input data: asset prices, bidoffers, and explanatory variables
# Derived time series data: signal, betas, buy.sell.orders, positions, and pnls

# source("utilLib.R")

########################
### alphaModel Object Constructor
########################
#' @export
alphaModel <- function(name.model=NULL) {

### Create object list
  model <- list()
  class(model) <- c("alphaModel", "list")

### Initialize the model parameters to their default values
# func.signal is a list defining the signal function and its parameters
#  names(model) <- colnames(ts.prices)
  model$name <- name.model
  model$signal.list <- list(signal.func="signals.alphaModel",
                            signal.params=c(3, 800, 0, 2),
                            filter.func="filtSavGol", filter.params=c(3,800,0,2),
                            quantile.params=list("hours",18,0.1),
                            order.book.params=list("hours",18,c(0.01,0.983)),
                            normalize.returns=TRUE, normalize.returns.window=800,
                            normalize.signal=TRUE, normalize.signal.window=800,
                            filter.returns=TRUE)

  model$rules.list <- list(rules.func="fillOrders.alphaModel",
                           rules.params=rbind(c(0.5,1.0,1.0),c(-0.5,-1.0,-1.0)),
                           stop.loss=list(apply=FALSE, limit=-5),
						   scalingVector=NULL,upperQ=NULL,lowerQ=NULL)

# Boolean
  model$recalc$signal <- TRUE
  model$recalc$rules <- TRUE
  model$recalc$pnls <- TRUE
  model$recalc$performance <- TRUE

# Time series
  model$prices <- NULL
  model$bidoffers <- NULL
  model$ancillary <- NULL
  model$betas <- NULL
  model$signals <- NULL
  model$positions <- NULL
  model$pnls <- NULL
 # These variables control how many ticks to look back on to repress the signal, and how we should control the regime change (-1 convert to mean reversion, 2 double the trend following, NA fully repress etc.)
  model$ancillary$look.back <- 100 
  model$ancillary$flips.repress <- NA 

# Variables
  model$performance <- NULL

# Persistent
  model$persist$develop.mode <- TRUE
  model$persist$positions <- 0.0
  model$persist$lastPxChange <- 0.0
  model$persist$lastRunSD <- 0.0
  model$persist$lastSignals <- 0.0
  model$persist$lastVol <- NULL
  model$persist$lastIndex <- 0.0
  model$persist$state

# Return object
  model

}
# End alphaModel Constructor



########################
### alphaModel Updater
########################
# Update the model parameters with new input time series or scalar data (no calculations are performed)
#' @export
update.alphaModel <- function(model, func.signal=NULL, trading.rules=NULL, ts.prices=NULL, ts.bidoffers=NULL, ts.ancillary=NULL, ts.betas=NULL, develop.mode=NULL, init.position=NULL) {

  stopifnot(inherits(model, "alphaModel"))

### Update the model parameters with new input data
  if (!is.null(func.signal))
    {
      model$signal.list <- updateList(model$signal.list, func.signal)
      model$recalc$signal <- TRUE
      model$recalc$rules <- TRUE
      model$recalc$pnls <- TRUE
    }

  if (!is.null(trading.rules))
    {
      model$rules.list <- updateList(model$rules.list, trading.rules)
      model$recalc$rules <- TRUE
      model$recalc$pnls <- TRUE
    }

### Flip develop mode
  if (!is.null(develop.mode))
    {
      if (develop.mode)
        {
          model$persist$develop.mode <- TRUE
          model$recalc$pnls <- TRUE
          model$recalc$performance <- TRUE
        }
      else
        {
          model$persist$develop.mode <- FALSE
          model$recalc$pnls <- TRUE
          model$recalc$performance <- FALSE
        }
    }

### Update time series data
  if (!is.null(ts.prices))
    {
      model$prices <- ts.prices
#      colnames(model$prices) <- "Prices"
      model$bidoffers <- xts(rep(0, length(model$prices[,1])), order.by=index(model$prices))
      colnames(model$bidoffers) <- "Bid.Offers"
      model$betas <- xts(rep(1.0, length(model$prices[,1])), order.by=index(model$prices))
      colnames(model$betas) <- "Betas"
      model$recalc$signal <- TRUE
      model$recalc$rules <- TRUE
      model$recalc$pnls <- TRUE
    }

  if (!is.null(ts.bidoffers))
    {
      model$bidoffers <- ts.bidoffers
#      colnames(model$bidoffers) <- "Bid.Offers"
      model$recalc$pnls <- TRUE
    }

  if (!is.null(ts.ancillary))
    {
      model$ancillary$prices <- ts.ancillary[,1]
#      colnames(model$ancillary$prices) <- "Prices"
      model$ancillary$order.balance <- ts.ancillary[,2]
      colnames(model$ancillary$order.balance) <- "Order.Balance"
      model$recalc$signal <- TRUE
      model$recalc$rules <- TRUE
      model$recalc$pnls <- TRUE
    }

  if (!is.null(ts.betas))
    {
      model$betas <- ts.betas
#      colnames(model$betas) <- "Betas"
      model$recalc$pnls <- TRUE
    }

  if (!is.null(init.position))
    model$persist$positions <- init.position

  model
}
# End update.alphaModel



########################
### alphaModel Calculator
########################
# Calculate the model and its derived time series data
#' @export
recalc.alphaModel <- function(model) {

  stopifnot(inherits(model, "alphaModel"))

### Calculate signals
  if (model$recalc$signal)
    {
      func.name <- match.fun(model$signal.list$signal.func)
      model <- func.name(model)
      model$recalc$signal <- FALSE
      model$recalc$rules <- TRUE
      model$recalc$pnls <- TRUE
    }

### Apply trading rules
  if (model$recalc$rules)
    {
# Parse the trading.rules list
# The first element of model$rules.list is the name of the trading rules function
# The remaining elements of model$rules.list are the rules function parameters
      func.name <- match.fun(model$rules.list$rules.func)
# Calculate positions
      model <- func.name(model)
      model$recalc$rules <- FALSE
      model$recalc$pnls <- TRUE
    }

  if (model$recalc$pnls)
    {
### Calculate PnLs
      model <- calcProfitLoss.alphaModel(model)
      model$recalc$pnls <- FALSE
      if (model$recalc$performance)
### Compile performance
        model <- calcPerformance.alphaModel(model)
    }

  model
}
# End recalc.alphaModel



########################
### alphaModel Generic Signal Calculator
########################
signals.alphaModel <- function(model) {
  stopifnot(inherits(model, "alphaModel"))

  ts.prices <- model$prices

### Normalize returns
  if(model$signal.list$normalize.returns) {
    time.window <- model$signal.list$normalize.returns.window
# Calculate the running standard deviation
    ts.diff.prices <- diff(ts.prices)

# Read back persisted price diff (CEP mode only)
    if (!model$persist$develop.mode &&
        is.xts(model$persist$lastPxChange) && 
        index(model$persist$lastPxChange)%in%index(ts.diff.prices))
      {
# This code will only get hit in CEP mode
        ts.diff.prices[index(model$persist$lastPxChange)] <- model$persist$lastPxChange
      }
    else
      {
        ts.diff.prices[1,] <- 0.0
      }
# Persist next to last price diff for CEP mode
    model$persist$lastPxChange <- ts.diff.prices[2,1] #persist the second change, as this will go to NA on the next iteration.

#    ts.diff.prices <- na.locf(ts.diff.prices)
### Calculate the running standard deviation
    ts.sd <- runSD(ts.diff.prices, n=time.window)
    if (is.na(ts.sd[1,])) ts.sd[1,] <- 0.1
    ts.sd <- na.locf(ts.sd)

# Read back persisted price SD (CEP mode only)
    if (!model$persist$develop.mode) {
      if (is.xts(model$persist$lastRunSD)) {
        ts.sd[index(tail(model$persist$lastRunSD,-1)),] <- tail(model$persist$lastRunSD,-1)
      }
# Persist price SD (CEP mode only)
        model$persist$lastRunSD <- ts.sd
    }

#  ts.sd.prices <- runSD(model$prices, n=trunc(time.window/4))
#  ts.sd.prices[1] <- 0.1
#  ts.sd.prices <- na.locf(ts.sd.prices)


### This should be in OTQ###
# Scrub the data from single outliers
# Fix outliers where two consecutive scaled returns exceed 5*SD
    if (model$signal.list$filter.returns) {
        ts.suspect <- ((abs(ts.diff.prices)>5*ts.sd) & (abs(ts.diff.prices+lag(ts.diff.prices,-1))<5*ts.sd))
        ts.prices[ts.suspect] <- NA
        ts.prices <- na.locf(ts.prices)
        model$prices <- ts.prices
# Re-calculate the running standard deviation
        ts.diff.prices <- diff(ts.prices) 	
        ts.diff.prices[1,] <- 0.0
        ts.sd <- runSD(ts.diff.prices, n=time.window)
        ts.sd[1,] <- median(na.omit(ts.sd))#0.1
        ts.sd <- na.locf(ts.sd)
      }
# End scrub

# Scale the returns by the running standard deviation
# Reconstitute prices
    ts.prices <- cumsum(ts.diff.prices/ts.sd)
  }
# End normalize returns


### Calculate signals
# Parse the func.signal list
# The first element of model$signal.list is the func.signal name
  func.name <- match.fun(model$signal.list$filter.func)
# The remaining elements of model$signal.list are the signal function parameters
# Perform the signal calculation

  sigObject <- do.call(func.name,c(list(ts.prices), model$signal.list$filter.params,list(state=model$persist$state)))
  if(is.null(names(sigObject))){
	sig <- sigObject
   }else{
		sig <- sigObject$signal
		if('state' %in% names(sigObject)) model$persist$state <- sigObject$state
	}
  
  model$signals <- xts(
                       tail(sig,nrow(model$prices)),
                       order.by=index(model$prices)
                       )
  if (is.na(model$signals[1,])) model$signals[1,] <- 0.0
  model$signals <- na.locf(model$signals)


# Read back persisted signals (CEP mode only)
### Normalize signal
  if(model$signal.list$normalize.signal) {
    if (!model$persist$develop.mode) {
      if (is.xts(model$persist$lastSignals))
# Read back persisted signals (CEP mode only)
        model$signals[index(tail(model$persist$lastSignals,-1)),] <- tail(model$persist$lastSignals,-1)
# Persist signals (CEP mode only)
      model$persist$lastSignals <- model$signals
    }

# This is an alternative global normalization - for R&D only
#  signal.sd <- sum(abs(quantile(model$signals, c(0.95,0.05))))/2
#  model$signals <- model$signals/signal.sd


    time.window <- model$signal.list$normalize.signal.window
    signal.sd <- na.omit(runMax(model$signals, n=time.window)-runMin(model$signals, n=time.window))
##  model$signals <- ifelse(is.na(signal.sd) || signal.sd==0, model$signals, model$signals/signal.sd)
    signal.sd <- cbind(signal.sd, model$prices)
    signal.sd[1,1] <- mean(na.omit(signal.sd[,1])) # Instead of 1.0
    signal.sd <- na.locf(signal.sd)
    signal.sd <- signal.sd[,1]
# This is an alternative global normalization
#    signal.sd <- sum(abs(quantile(model$signals, c(0.95,0.05))))/2

# Persist signal SD (CEP mode only)
    if (!model$persist$develop.mode) {
      if(is.xts(model$persist$lastSignal.sd)){
        signal.sd[index(tail(model$persist$lastSignal.sd,-1)),] <- tail(model$persist$lastSignal.sd,-1)
      }
      model$persist$lastSignal.sd <- signal.sd
    }
# End persist signal SD (CEP mode only)

    model$signals <- model$signals/signal.sd
    if (is.na(model$signals[1,])) model$signals[1,] <- 0.0
    model$signals <- na.locf(model$signals)
  }
# End normalize signal


# Keep track of the size of the window if you are normalizing either the signal or the returns
# The buffer will not be in the correct state until it is completely primed with persistent data
# I assume this to be twice the size of the buffer, as you can't normalize greater than the size of the buffer
  if(!model$persist$develop.mode)
    {
      if(model$signal.list$normalize.signal||
         model$signal.list$normalize.returns)
        {
          if(length(model$persist$lastIndex)==1)
            {
              model$persist$lastIndex <- c(rep(NA,2 * nrow(model$signals)))
            }
           model$persist$lastIndex <- append(tail(model$persist$lastIndex,-1),last(index(model$signals)) )
        }
      else
        {
# if you aren't normalizing all of the other gymnastics don't matter, just return the size of the buffer
          model$persist$lastIndex <- first(index(model$signals))
        }
                                                     
    }
# End if
  
  colnames(model$signals) <- "Signals"
  model
}
# End signals.alphaModel



########################
### alphaModel Ancillary Signal Calculator
########################

######THIS WILL RECALC THE ENTIRE HISTORY EVERYTIME... FIX THIS TO ONLY RUN ON NEW DATA (Existing persistant logic iss flawed)##################
signals.ancillary.alphaModel <- function(model) {
  stopifnot(inherits(model, "alphaModel"))

  ts.prices <- model$ancillary$prices
### Calculate signals
# Parse the func.signal list
# The first element of model$signal.list is the func.signal name
  func.name <- match.fun(model$signal.list$filter.func)
# The remaining elements of model$signal.list are the signal function parameters
# Apply the func.signal to the ancillary$prices
  signal.list <- model$signal.list$filter.params
  signal.list[3] <- 0
  signal.list[4] <- 0
  model$signals <- xts(
                       do.call(
                               func.name,
                               append(list(coredata(ts.prices)), signal.list)
                               ),
                       order.by=index(model$prices)
                       )
  model$signals[1,] <- na.omit(model$signals)[1,]
  model$signals <- na.locf(model$signals)
  colnames(model$signals) <- "Signals"


# Read back persisted signals (CEP mode only)
#    if (!model$persist$develop.mode) {
#      if (is.xts(model$persist$lastSignals))
# Read back persisted signals (CEP mode only)
#        model$signals[index(tail(model$persist$lastSignals,-1)),] <- tail(model$persist$lastSignals,-1)
# Persist signals (CEP mode only)
#      model$persist$lastSignals <- model$signals
#    }
# End persisted signals (CEP mode only)

### Calculate running number of potential model flips
  ts.diff <- sign(diff(model$signals))
  ts.diff <- abs(diff(ts.diff))/2
  ts.diff[1:2] <- 0.0
  look.back <- model$signal.list$filter.params[3]
  model$ancillary$num.flips <- runSum(ts.diff, look.back)
  model$ancillary$num.flips[1,] <- na.omit(model$ancillary$num.flips)[1,]
  model$ancillary$num.flips <- na.locf(model$ancillary$num.flips)
  ## signal.sd <- sum(abs(quantile(model$ancillary$num.flips, c(0.95,0.05))))/2
  ## model$ancillary$num.flips <- model$ancillary$num.flips/signal.sd
  ## model$ancillary$num.flips[1,] <- na.omit(model$ancillary$num.flips)[1,]
  ## model$ancillary$num.flips <- na.locf(model$ancillary$num.flips)
  colnames(model$ancillary$num.flips) <- "Num.flips"

### Calculate running Signal volatility
  model$ancillary$signals.vol <- runMax(model$signals, look.back)-runMin(model$signals, look.back)
  model$ancillary$signals.vol[1,] <- na.omit(model$ancillary$signals.vol)[1,]
  model$ancillary$signals.vol <- na.locf(model$ancillary$signals.vol)
  ## signal.sd <- sum(abs(quantile(model$ancillary$signals.vol, c(0.95,0.05))))/2
  ## model$ancillary$signals.vol <- model$ancillary$signals.vol/signal.sd
  ## model$ancillary$signals.vol[1,] <- na.omit(model$ancillary$signals.vol)[1,]
  ## model$ancillary$signals.vol <- na.locf(model$ancillary$signals.vol)
  colnames(model$ancillary$signals.vol) <- "Signals.vol"



### Persist volatility estimates (CEP mode only)
  #if (!model$persist$develop.mode) {
  #  if(is.xts(model$persist$lastVol$prices)){
  #    model$ancillary$num.flips[index(tail(model$persist$lastVol$prices,-1)),] <- tail(model$persist$lastVol$prices,-1)
  #  }
  #  model$persist$lastVol$prices <- model$ancillary$num.flips

   # if(is.xts(model$persist$lastVol$signals)){
   #   model$ancillary$signals.vol[index(tail(model$persist$lastVol$signals,-1)),] <- tail(model$persist$lastVol$signals,-1)
   # }
   # model$persist$lastVol$signals <- model$ancillary$signals.vol
  #}
# End persist volatility estimates (CEP mode only)


  model
}
### End signals.ancillary.alphaModel


### This version of function signals.ancillary1.1 scales the diff of signals by signals.vol
# To be used in conjunction with function fillOrders4.1
signals.ancillary1.1.alphaModel <- function(model) {
  stopifnot(inherits(model, "alphaModel"))

  ts.prices <- model$ancillary$prices
### Calculate signals
# Parse the func.signal list
# The first element of model$signal.list is the func.signal name
  func.name <- match.fun(model$signal.list$filter.func)
# The remaining elements of model$signal.list are the signal function parameters
# Apply the func.signal to the ancillary$prices
  signal.list <- model$signal.list$filter.params
  signal.list[3] <- 0
  signal.list[4] <- 0
  model$signals <- xts(
                       do.call(
                               func.name,
                               append(list(coredata(ts.prices)), signal.list)
                               ),
                       order.by=index(model$prices)
                       )
  model$signals[1,] <- na.omit(model$signals)[1,]
  model$signals <- na.locf(model$signals)
  colnames(model$signals) <- "Signals"

  look.back <- model$signal.list$filter.params[3]
  look.back.short <- model$signal.list$filter.params[4]

### Calculate running number of potential model flips
## ts.diff <- sign(diff(model$signals))
## ts.diff <- abs(diff(ts.diff))/2
## ts.diff[1:2] <- 0.0
## model$ancillary$num.flips <- runSum(ts.diff, look.back)
## model$ancillary$num.flips[1,] <- na.omit(model$ancillary$num.flips)[1,]
## model$ancillary$num.flips <- na.locf(model$ancillary$num.flips)
## signal.sd <- sum(abs(quantile(model$ancillary$num.flips, c(0.95,0.05))))/2
## model$ancillary$num.flips <- model$ancillary$num.flips/signal.sd
## model$ancillary$num.flips[1,] <- na.omit(model$ancillary$num.flips)[1,]
## model$ancillary$num.flips <- na.locf(model$ancillary$num.flips)
  model$ancillary$num.flips <- xts(rep(0.0, nrow(model$signals)), order.by=index(model$signals))
  colnames(model$ancillary$num.flips) <- "Num.flips"

### Calculate running Signal volatility
  ## signals.vol.long <- runSD(ts.prices, n=look.back)
  ## signals.vol.long[1,] <- na.omit(signals.vol.long)[1,]
  ## signals.vol.long <- na.locf(signals.vol.long)+1e-5

## signals.vol.short <- runSD(ts.prices, n=look.back.short)
## signals.vol.short[1,] <- na.omit(signals.vol.short)[1,]
## signals.vol.short <- na.locf(signals.vol.short)

## model$ancillary$signals.vol <- signals.vol.short/signals.vol.long
## signal.sd <- sum(abs(quantile(model$ancillary$signals.vol, c(0.95,0.05))))/2
## model$ancillary$signals.vol <- model$ancillary$signals.vol/signal.sd
##  model$ancillary$signals.vol[1,] <- na.omit(model$ancillary$signals.vol)[1,]
##  model$ancillary$signals.vol <- na.locf(model$ancillary$signals.vol)

#  model$signals <- diff(model$signals)/signals.vol.long
  model$signals <- diff(model$signals, look.back)
  model$signals[1:look.back,] <- 0.0
  model$signals <- diff(model$signals, look.back.short)
  model$signals[1:look.back.short,] <- 0.0
  model$signals <- cumsum(model$signals)
  colnames(model$signals) <- "Signals"

  model
}
### End signals.ancillary1.1.alphaModel


########################
### alphaModel Ancillary2 Signal Calculator for non SG
########################
signals.ancillary2.alphaModel <- function(model) {
  stopifnot(inherits(model, "alphaModel"))

  ts.prices <- model$ancillary$prices
### Calculate signals
# Parse the func.signal list
# The first element of model$signal.list is the func.signal name
  func.name <- match.fun(model$signal.list$filter.func)
# The remaining elements of model$signal.list are the signal function parameters
# Apply the func.signal to the ancillary$prices
  signal.list <- model$signal.list$filter.params
  
  model$signals <- xts(
                       do.call(
                               func.name,
                               append(list(coredata(ts.prices)), signal.list)
                               ),
                       order.by=index(model$prices)
                       )
  if (is.na(model$signals[1,])) model$signals[1,] <- na.omit(model$signals)[1,]
  model$signals <- na.locf(model$signals)
  colnames(model$signals) <- "Signals"


# Read back persisted signals (CEP mode only)
    if (!model$persist$develop.mode) {
      if (is.xts(model$persist$lastSignals))
# Read back persisted signals (CEP mode only)
        model$signals[index(tail(model$persist$lastSignals,-1)),] <- tail(model$persist$lastSignals,-1)
# Persist signals (CEP mode only)
      model$persist$lastSignals <- model$signals
    }
# End persisted signals (CEP mode only)

  model
}
# End signals.ancillary2.alphaModel


########################
### signals.ancillary3.alphaModel
########################
signals.ancillary3.alphaModel <- function(model) {
  stopifnot(inherits(model, "alphaModel"))

  ts.prices <- model$ancillary$prices
### Calculate signals
# Parse the func.signal list
# The first element of model$signal.list is the func.signal name
  func.name <- match.fun(model$signal.list$filter.func)
# The remaining elements of model$signal.list are the signal function parameters
# Apply the func.signal to the ancillary$prices
  signal.list <- model$signal.list$filter.params
  signal.list[3] <- 0
  signal.list[4] <- 0
  
  sigObject <- do.call(func.name,c(list(ts.prices), model$signal.list$filter.params,list(state=model$persist$state)))
  if(is.null(names(sigObject))){
	sig <- sigObject
   }else{
		sig <- sigObject$signal
		if('state' %in% names(sigObject)) model$persist$state <- sigObject$state
  }
	model$signals <- xts(
                       tail(sig,nrow(ts.prices)),
                       order.by=index(ts.prices)
                       )
					   
 
  model$signals[1,] <- na.omit(model$signals)[1,]
  model$signals <- na.locf(model$signals)
  colnames(model$signals) <- "Signals"


### Calculate running number of potential model flips
  ts.diff <- sign(diff(model$signals))
  ts.diff <- abs(diff(ts.diff))/2
  ts.diff[1:2] <- 0.0
  look.back <- model$ancillary$look.back
  model$ancillary$num.flips <- runSum(ts.diff, look.back)
  model$ancillary$num.flips[1,] <- na.omit(model$ancillary$num.flips)[1,]
  model$ancillary$num.flips <- na.locf(model$ancillary$num.flips)
  colnames(model$ancillary$num.flips) <- "Num.flips"

### Calculate running Signal volatility
  model$ancillary$signals.vol <- runMax(model$signals, look.back)-runMin(model$signals, look.back)
  model$ancillary$signals.vol[1,] <- na.omit(model$ancillary$signals.vol)[1,]
  model$ancillary$signals.vol <- na.locf(model$ancillary$signals.vol)
  colnames(model$ancillary$signals.vol) <- "Signals.vol"

  model
}
### End signals.ancillary3.alphaModel

########################
### fillOrders
########################
# fillOrders function calculates buy.sell.orders from a set of thresholds, and calculates net positions
#' @export
fillOrders.alphaModel <- function(model) {

# Initialize the positions vector
  model$positions <- xts(rep(NA, length(model$prices)), order.by=index(model$prices))
# Loop through the thresholds, find all the crossing points, compile buy.sell.orders vector, and append to positions vector
  num.thresholds <- dim(model$rules.list$rules.params)[1]
  for (threshold in 1:num.thresholds)
    {
      params.threshold <- model$rules.list$rules.params[threshold,]
      buy.sell.orders <- ifelse((((model$signals-params.threshold[1])*(lag(model$signals)-params.threshold[1]))<0)&((model$signals-params.threshold[1])*params.threshold[2])>0, params.threshold[3], NA)
#      buy.sell.orders <- makeOrders.alphaModel(model, model$rules.list$rules.params[threshold,])
      model$positions <- ifelse(is.na(buy.sell.orders), model$positions, buy.sell.orders)
    }

# Restore xts
  model$positions <- xts(model$positions, order.by=index(model$prices))
  colnames(model$positions) <- "Positions"
# Set initial and carried positions
  model$positions[1,] <- 0.0


# CEP mode only
  if (!model$persist$develop.mode) {
# Ensure that the index of the persistant position is in the buffer before u begin updating.
    if(index(model$persist$positions)%in%index(model$positions))
      { 
        model$positions[index(model$persist$positions)] <- model$persist$positions
        model$positions <- na.locf(model$positions)
# Persist the last positions
        model$persist$positions <- last(model$positions)
      }
    else
      {
# If the position is within the time index, insert it at its closest time. 
        timediff <-  index(model$positions) -  index(model$persist$positions)
        if (sum(timediff>=0)) {
          lc <- which(abs(timediff)%in%min(abs(timediff)))
		  model$positions[lc:nrow(model$positions)] <- NA
          model$positions[lc] <- model$persist$positions
          print(paste("Warning:: inserting a postion that isn't in the index:",index(model$persist$positions),coredata(model$persist$positions),"not found inserting the last position at:", index(model$positions[lc])))
          model$persist$positions <- model$positions[lc]
        }
      }
  }
# End if model$persist$develop.mode

#ep <- endpoints(model$positions, on = 'days')
#model$positions[ep]  <- 0 
# Carry forward correct positions
  model$positions <- na.locf(model$positions)

  model
}
# End fillOrders


# fillOrders2 function calculates positions based on peaks in signal
#' @export
fillOrders2.alphaModel <- function(model, ...) {

# First find turning points to go long after min has been reached
  time.window <- 10
  signal.extreme <- runMin(model$signals, n=time.window)
  signal.extreme[1,1] <- 0.0
  signal.extreme <- na.locf(signal.extreme)
# Go long after the local min is past the lookback window
  turning.points <- (signal.extreme>lag(signal.extreme))&(lag(signal.extreme)==(lag(lag(signal.extreme))))
  turning.points[1,1] <- FALSE
  positions.long <- xts(rep(NA, length(model$prices)), order.by=index(model$prices))
  positions.long[turning.points] <- 1.0

# Next find turning points to go short after max has been reached
  signal.extreme <- runMax(model$signals, n=time.window)
  signal.extreme[1,1] <- 0.0
  signal.extreme <- na.locf(signal.extreme)
# Go short after the local max is past the lookback window
  turning.points <- (signal.extreme<lag(signal.extreme))&(lag(signal.extreme)==(lag(lag(signal.extreme))))
  turning.points[1,1] <- FALSE
  positions.short <- xts(rep(NA, length(model$prices)), order.by=index(model$prices))
  positions.short[turning.points] <- -1.0

# Initialize the positions vector
  model$positions <- xts(rep(NA, length(model$prices)), order.by=index(model$prices))
  model$positions <- ifelse(is.na(positions.long), model$positions, positions.long)
  model$positions <- ifelse(is.na(positions.short), model$positions, positions.short)

# Carry forward positions
  model$positions[1,1] <- 0.0
  model$positions <- xts(na.locf(model$positions), order.by=index(model$prices))
  colnames(model$positions) <- "Positions"

  model
}
# End fillOrders2


# fillOrders3 function calculates positions proportional to the signal
#' @export
fillOrders3.alphaModel <- function(model, ...) {

  model$positions <- trunc(5*model$signals)/5
  colnames(model$positions) <- "Positions"
  model
}
# End fillOrders3



# fillOrders4 function calculates positions proportional to the signal slope
#' @export
fillOrders4.alphaModel <- function(model, ...) {

#  quantile.second.deriv <- quantile(model$ancillary$signal.second.deriv, c(model$rules.list$rules.params,1-model$rules.list$rules.params))

# Allow flipping only if order.balance is less than threshold value
# Calculate regions allowed for flipping
  ts.diff <- sign(diff(model$signals))

  model$positions <- ts.diff*ifelse(
                                    (model$ancillary$num.flips < model$rules.list$rules.params[1])
                                    &
                                    (model$ancillary$signals.vol > model$rules.list$rules.params[2])
                                    , 1, model$ancillary$flips.repress)

  model$positions[1,] <- 0.0
  model$positions <- na.locf(model$positions)
  model$positions <- sign(model$positions)
  colnames(model$positions) <- "Positions"
  model
}
# End fillOrders4.alphaModel


### The function fillOrders4.1 calculates positions proportional to the signal slope
# It applies a threshold value to the diff signals to determine if prices are in a trending regime
#' @export
fillOrders4.1.alphaModel <- function(model, ...) {

# Allow position flipping only if in trending regime
# Calculate regime
  look.back <- model$signal.list$filter.params[3]
  look.back.short <- model$signal.list$filter.params[4]
  ts.tmp <- diff(model$signals, 1)
  ts.tmp[1,] <- 0.0
  signals.vol.long <- runSD(ts.tmp, n=look.back)
  signals.vol.long[1,] <- na.omit(signals.vol.long)[1,]
  signals.vol.long <- na.locf(signals.vol.long)+1e-5
  ts.tmp <- ts.tmp/signals.vol.long
  ts.diff <- sign(ts.tmp)
  model$ancillary$signals.vol <- ts.tmp
  colnames(model$ancillary$signals.vol) <- "Sig.Vol"
  ts.tmp <- abs(ts.tmp)

  model$positions <- ts.diff*ifelse((ts.tmp > model$rules.list$rules.params[1] | ts.tmp < model$rules.list$rules.params[2]), 1, model$ancillary$flips.repress)

  model$positions[1,] <- 0.0
  model$positions <- na.locf(model$positions)
  model$positions <- sign(model$positions)
  colnames(model$positions) <- "Positions"
  model
}
# End fillOrders4.1.alphaModel


fillOrders5.alphaModel <- function(model,...) {
	
	 #Initialize the positions vector
	  model$positions <- xts(rep(NA, length(model$prices)), order.by=index(model$prices))
	# Loop through the thresholds, find all the crossing points, compile buy.sell.orders vector, and append to positions vector
	  num.thresholds <- dim(model$rules.list$rules.params)[1]
	  for (threshold in 1:num.thresholds)
		{
		  params.threshold <- model$rules.list$rules.params[threshold,]
		  buy.sell.orders <- ifelse((((model$signals-params.threshold[1])*(lag(model$signals)-params.threshold[1]))<0)&((model$signals-params.threshold[1])*params.threshold[2])>0, params.threshold[3], NA)
	#      buy.sell.orders <- makeOrders.alphaModel(model, model$rules.list$rules.params[threshold,])
		  model$positions <- ifelse(is.na(buy.sell.orders), model$positions, buy.sell.orders)
		}

	# Restore xts
	  model$positions <- xts(model$positions, order.by=index(model$prices))
	  colnames(model$positions) <- "Positions"
	  
	  
	  
	idxLong <- index(model$positions[model$positions>0])
	
	#scale the positions 
	idxShort <- index(model$positions[model$positions<=0])
	scalingVector <- model$rules.list$scalingVector
	upperQ<-model$rules.list$upperQ
	lowerQ<-model$rules.list$lowerQ
	if(length(idxLong)!=0){
	model$positions	[idxLong] <- model$positions[idxLong] * ifelse(scalingVector[idxLong]>0,
																		1-(findInterval(scalingVector[idxLong],upperQ)/length(upperQ)),
																	   (findInterval(scalingVector[idxLong],lowerQ)/length(lowerQ)))
																	   }
	
	if(length(idxShort)!=0){
	model$positions[idxShort] <- model$positions[idxShort] * ifelse(scalingVector[idxShort]>0,
																		(findInterval(scalingVector[idxShort],upperQ)/length(upperQ)),
																		1-(findInterval(scalingVector[idxShort],lowerQ)/length(lowerQ)))
																		}
	# Carry forward correct positions
	if(model$persist$develop.mode) model$rules.list$scalingVector =NULL
	  
	  
	# Set initial and carried positions
	  model$positions[1,] <- 0.0


	# CEP mode only
	  if (!model$persist$develop.mode) {
	# Ensure that the index of the persistant position is in the buffer before u begin updating.
		if(index(model$persist$positions)%in%index(model$positions))
		  { 
		  
			model$positions[index(model$persist$positions)] <- model$persist$positions
			model$positions <- na.locf(model$positions)
	# Persist the last positions
			model$persist$positions <- last(model$positions)
		  }
		else
		  {
	# If the position is within the time index, insert it at its closest time. 
			timediff <-  index(model$positions) -  index(model$persist$positions)
			if (sum(timediff>=0)) {
			  lc <- which(abs(timediff)%in%min(abs(timediff)))
			  model$positions[lc] <- model$persist$positions
			  print(paste("Warning:: inserting a postion that isn't in the index:",index(model$persist$positions),coredata(model$persist$positions),"not found inserting the last position at:", index(model$positions[lc])))
			  model$persist$positions <- last(model$positions)
			}
		  }
	  }
	# End if model$persist$develop.mode

	
	
	model$positions <- na.locf(model$positions)
	
	
	
	model
 }
# End fillOrders5

fillOrders6.alphaModel <- function(model, ...) {
	model$positions <- model$signals[,1]*NA
	index.tp <- which(diff(sign(model$signals))!=0&abs(model$signals)>model$rules.list$rules.params)
	model$positions[index.tp] <- sign(model$signals[index.tp])
	model$positions[1,] <- 0.0
	
	if (!model$persist$develop.mode) {
	# Ensure that the index of the persistant position is in the buffer before u begin updating.
		if(index(model$persist$positions)%in%index(model$positions))
		  { 
			model$positions[index(model$persist$positions)] <- model$persist$positions
			model$positions <- na.locf(model$positions)
	# Persist the last positions
			model$persist$positions <- last(model$positions)
		  }
		else
		  {
	# If the position is within the time index, insert it at its closest time. 
			timediff <-  index(model$positions) -  index(model$persist$positions)
			if (sum(timediff>=0)) {
			  lc <- which(abs(timediff)%in%min(abs(timediff)))
			  model$positions[lc] <- model$persist$positions
			  print(paste("Warning:: inserting a postion that isn't in the index:",index(model$persist$positions),coredata(model$persist$positions),"not found inserting the last position at:", index(model$positions[lc])))
			  #model$persist$positions <- last(model$positions)
			}
		  }
		 }
	
	model$positions <- na.locf(model$positions)
	colnames(model$positions) <- "Positions"
	model
}
# End fillOrders6

########################
###  calcProfitLoss
########################
# calcProfitLoss function calculates PnLs from positions, asset prices, and bidoffers
#' @export
calcProfitLoss.alphaModel <- function(model) {

  singlePnL <- function(returns, positions, bidoffers, betas) {
    pnl <- lag(positions*betas)*returns - abs(diff(positions*betas))*bidoffers/2
#    pnl[1,1] <- 0.0
#    pnl <- na.locf(pnl)
    pnl
  }

  asset.returns <- diff(model$prices)
  asset.returns[1,] <- 0.0
#  asset.returns <- na.omit(asset.returns)

# Calculate pnls (first pass)
  pnls.model <- singlePnL(asset.returns, model$positions, model$bidoffers, model$betas)
#  model$pnls <- mapply.xts(singlePnL, asset.returns, model$positions, model$bidoffers, model$betas)
  pnls.model[1:2,] <- 0.0


  times.stop.loss <- xts(rep(FALSE, length(model$prices[,1])), order.by=index(model$prices))
# Calculate stop-losses
  if (model$rules.list$stop.loss$apply)
    {
# Calculate pnls in each holding period (after position flip)
      pnl.period <- xts(rep(NA, length(model$prices[,1])), order.by=index(model$prices))
      pnl.period[1,] <- 0.0
      trade.flips <- diff(sign(model$positions))
      trade.flips[1,] <- 0.0
      trade.flips <- coredata(abs(trade.flips)>0)
      trade.flips <- xts(trade.flips, order.by=index(model$prices))
      pnl.cum <- cumsum(pnls.model)
      pnl.period[trade.flips] <- pnl.cum[trade.flips]
      pnl.period <- na.locf(pnl.period)
      pnl.period <- pnl.cum - pnl.period
# Calculate stop-loss times and set positions to zero when model hits stop-loss
      times.stop.loss <- coredata(pnl.period < model$rules.list$stop.loss$limit)
      times.stop.loss <- xts(times.stop.loss, order.by=index(model$prices))
      positions.stop.loss <- xts(rep(NA, length(model$prices[,1])), order.by=index(model$prices))
      positions.stop.loss[1,] <- 0.0
      positions.stop.loss[times.stop.loss] <- 0.0
# Reset positions when the model flips
      positions.stop.loss[trade.flips] <- model$positions[trade.flips]
      positions.stop.loss <- na.locf(positions.stop.loss)

# Recalculate pnls (second pass)
      pnls.model <- singlePnL(asset.returns, positions.stop.loss, model$bidoffers, model$betas)
      pnls.model[1:2,] <- 0.0
    }
# End stop-loss
  model$times.stop.loss <- times.stop.loss

  model$pnls <- pnls.model
  colnames(model$pnls) <- "PnLs"

  model

}
# End calcProfitLoss


# Function calcPerformance.alphaModel calculates model performance statistics
calcPerformance.alphaModel <- function(model) {

### Calculate performance statistics
# Aggregate pnl data
#  pnls <- period.apply(cumsum(model$pnls), endpoints(model$pnls, "days"), median)
#  model.stat <- statPACF(pnls)
  model$performance$CumPnL <- round(sum(model$pnls),4)
  model$performance$n.flips <- sum(abs(na.omit(diff(sign(model$positions)))))/2
  model$performance$PnL.flip <- round(model$performance$CumPnL/model$performance$n.flips,4)
#  daily.pnls <- to.daily(cumsum(model$pnls))
#  indeks <- index(daily.pnls)
#  daily.pnls <- diff(rowMeans(daily.pnls))
#  daily.pnls <- c(0.0, daily.pnls)
#  daily.pnls <- xts(daily.pnls, order.by=indeks)
#  model$performance$SharpeRatios <- sqrt(252)*SharpeRatio(daily.pnls)
  model$performance$SharpeRatios <- NA
  model

}
# End calcPerformance

#Function to trim the size of the alphaModel, this assumes all relevent history of data is stored as .xts
trm.alphaModel <- function(model, size=NULL)
{
	if(is.null(size)) size <- nrow(model$position)
	 nms <- lapply(model.test, names)
	 lapply(names(nms), function(x) {
		if (is.xts(model.test[[x]]))
		{
			model[[x]] <<- tail(model[[x]],size)
		} else{
		
			model[[x]] <<- model[[x]]
		}
	})
    model	
}

