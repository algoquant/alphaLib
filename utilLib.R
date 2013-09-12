################################################
### Utility Functions  #########################
################################################


##################################
### Generic Utility Functions  ###
##################################


### Lag an xts object, and pad with zeros
L <- function(ts.data, vec.lags=1) {
  ts.data <- lag.xts(ts.data, vec.lags)
# Pad with zeros
  for(n.col in 1:length(vec.lags))
    ts.data[1:vec.lags[n.col],n.col] <- ts.data[vec.lags[n.col]+1,n.col]
  return(ts.data)
}

### Pass in a default list of elements and an update list, and update the defaults with the new elements
updateList <- function(default.list, new.list) {

  names.default <- names(default.list)
  out.list <- lapply(names.default, function(name.default) {
                       if (is.null(new.list[[name.default]]))
                         default.list[[name.default]]
                       else
                         new.list[[name.default]]
                     }
                     )
# End lapply
  names(out.list) <- names.default
  out.list
}
### End updateList


### Combine all the elements of a list into a string
flattenList <- function(input.list) {

  if (is.list(input.list))
    {
      names.list <- names(input.list)
      out.string <- sapply(names.list, function(name.list) paste(name.list, "=", paste(input.list[[name.list]], collapse=","), collapse="") )
    }
  else
    out.string <- input.list
# End lapply
  paste(out.string, collapse=" / ")
}
### End flattenList


##########################
### Name Parsing Functions
##########################
### Compose xts data column names from symbol and field names
makeColNames <- function(symbol="GLENCR", field="VALID_MID,VALID_BID_OFFER") {

  col.names <- strsplit(strsplit(field, ",")[[1]], "_")
  col.names <- sapply(col.names, function(cn) paste(cn[-1], collapse="."))
  col.names <- paste(symbol, col.names, sep=".")
  col.names
}
### End makeColNames


############
### Error and Warning Message Handling Functions
#############
### Compose xts data column names from symbol and field names
writeMessage <- function(txt.msg, file.message="S:/Data/R_Data/Messages.txt") {
  cat(paste(txt.msg, collapse=";"), file=file.message, sep="\n", append=TRUE)
}
### End writeMessage



#####################################
### alphaModel Utility Functions  ###
#####################################
# cbind returns all the internal model time series bound into one xts
#' @export
cbind.alphaModel <- function(model) {

  stopifnot(inherits(model, "alphaModel"))
#  cbind(model$prices, model$bidoffers, model$signals, model$betas, model$ancillary$order.balance, model$positions, cumsum(model$pnls))
  cbind(model$prices, model$signals, model$positions, cumsum(model$pnls))

}
# End cbind.alphaModel

head.alphaModel <- function(model, ...) {

  stopifnot(inherits(model, "alphaModel"))
  head(cbind(model), ...)

}
# End head.alphaModel

tail.alphaModel <- function(model, ...) {

  stopifnot(inherits(model, "alphaModel"))
  tail(cbind(model), ...)

}
# End tail.alphaModel


title.alphaModel <- function(model, ...) {

  stopifnot(inherits(model, "alphaModel"))
#  paste(model$name, "/", format(Sys.Date(), format="%m-%d-%y"), "\nSignal function:", flattenList(model$signal.list), "\nTrading Rules:", flattenList(model$rules.list), "\nPerformance:", paste("CumPnL:", round(model$performance$CumPnL,2), "/ SharpeRatios:", paste(round(model$performance$SharpeRatios,2), collapse="/")))

  names.list <- list("name","signal.list","rules.list","performance")
  out.list <- lapply(names.list, function(model.name) paste(model.name, ": ", flattenList(model[[model.name]]), "\n" ,sep="") )
# End lapply
  out.list

}
# End title.alphaModel



### Calculate rolling xts quantiles over endpoints
rollingQuantiles <- function(ts.data, end.period, look.back, n.quantiles=c(0.1,0.9), expand.window=FALSE) {

# Type checking
  stopifnot(inherits(ts.data, "xts") && !any(is.na(ts.data)))

### Calculate endpoints
  end.points <- if(inherits(end.period, "numeric"))
    seq(1, nrow(ts.data), end.period)
  else
    tail(endpoints(ts.data, end.period), -1)
  stopifnot(look.back < length(end.points)-1)

# Initialize Data
  vol.data <- xts(rep(NA, length(ts.data)), order.by=index(ts.data))
  vol.data <- cbind(vol.data, vol.data)

### Loop over endpoints
  for (end.point in (look.back+1):(length(end.points)-1)) {
#    print(paste("endpoint", end.point, "datetime", index(ts.data)[now.point]))
    now.point <- end.points[end.point]

# Find calc window
    if (expand.window)
      {
        calc.window <- 1:now.point
        calc.window.plus <- 1:(now.point+1)
      }
    else
      {
        calc.window <- (end.points[end.point-look.back]+1):now.point
        calc.window.plus <- (end.points[end.point-look.back]+1):(now.point+1)
      }

    window.data <- ts.data[calc.window,]
#    window.data.plus <- ts.data[calc.window.plus,]

# Calculate vol over window.data
    vol.data[now.point,] <- quantile(window.data, n.quantiles)

  }
### End loop over endpoints

### Prepare and return data
  vol.data[1,] <- na.omit(vol.data)[1,]
  vol.data <- na.locf(vol.data)
  colnames(vol.data) <- paste(paste(n.quantiles,"confi"), colnames(ts.data))
  vol.data

}
### End rollingQuantiles



### Standardize an xts time series (say of residuals)
standard.xts <- function(ts.data, look.back, n.quantiles=c(0.1,0.9), expand.window=FALSE) {

# Type checking
  stopifnot(inherits(ts.data, "xts") && !any(is.na(ts.data)))

# Calculate running mean and vol
  run.mean <- runMean(ts.data, n=look.back)
  na.index <- is.na(run.mean)
  run.mean[na.index,] <- ts.data[na.index,]
  run.vol <- runSD(ts.data, n=look.back)
  run.vol <- na.locf(run.vol, fromLast=TRUE)
  ret.data <- (ts.data-run.mean)/run.vol
  colnames(ret.data) <- paste('Standard',colnames(ts.data))
  ret.data

}
### End standard.xts

