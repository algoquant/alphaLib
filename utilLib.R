################################################
### Utility Functions  #########################
################################################


##################################
### Generic Utility Functions  ###
##################################

### Pass in a default list of elements and an update list, and update the defaults with the new elements
updateList <- function(default.list, new.list) {
  names.default <- names(default.list)
  out.list <- lapply(names.default, function(name.default) {
    if (is.null(new.list[[name.default]]))
      default.list[[name.default]]
    else
      new.list[[name.default]]
  })  # end lapply
  names(out.list) <- names.default
  out.list
}  # end updateList


### Combine all the elements of a list into a string
flattenList <- function(input.list) {
  if (is.list(input.list))
  {
    names.list <- names(input.list)
    out.string <- 
      sapply(names.list, 
             function(name.list) 
               paste(name.list, "=", paste(input.list[[name.list]], collapse=","), collapse="") )  # end sapply
  }
  else
    out.string <- input.list
  paste(out.string, collapse=" / ")
}  # end flattenList


### Lag an xts object, and pad with zeros
L <- function(ts.data, vec.lags=1) {
  ts.data <- lag.xts(ts.data, vec.lags)
# Pad with zeros
  for(n.col in 1:length(vec.lags))
    ts.data[1:vec.lags[n.col],n.col] <- ts.data[vec.lags[n.col]+1,n.col]
  return(ts.data)
}  # end L


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
}  # end standard.xts



##########################
### Name Parsing Functions
##########################

### Compose xts data column names from symbol and field names
makeColNames <- function(symbol="GLENCR", field="VALID_MID,VALID_BID_OFFER") {
  col.names <- strsplit(strsplit(field, ",")[[1]], "_")
  col.names <- sapply(col.names, function(cn) paste(cn[-1], collapse="."))
  col.names <- paste(symbol, col.names, sep=".")
  col.names
}  # end makeColNames



############
### Error and Warning Message Handling Functions
#############

### Compose xts data column names from symbol and field names
writeMessage <- function(txt.msg, file.message=paste(data.dir, "Messages.txt", sep="")) {
  cat(paste(txt.msg, collapse=";"), file=file.message, sep="\n", append=TRUE)
}  # end writeMessage



#####################################
### alphaModel Utility Functions  ###
#####################################

# cbind returns all the internal model time series bound into one xts
#' @export
cbind.alphaModel <- function(model) {
  stopifnot(inherits(model, "alphaModel"))
#  cbind(model$prices, model$bidoffers, model$signals, model$betas, model$ancillary$order.balance, model$positions, cumsum(model$pnls))
  cbind(model$prices, model$signals, model$positions, cumsum(model$pnls))
}  # end cbind.alphaModel


head.alphaModel <- function(model, ...) {
  stopifnot(inherits(model, "alphaModel"))
  head(cbind(model), ...)

}  # end head.alphaModel

tail.alphaModel <- function(model, ...) {
  stopifnot(inherits(model, "alphaModel"))
  tail(cbind(model), ...)
}  # end tail.alphaModel


title.alphaModel <- function(model, ...) {
  stopifnot(inherits(model, "alphaModel"))
#  paste(model$name, "/", format(Sys.Date(), format="%m-%d-%y"), "\nSignal function:", flattenList(model$signal.list), "\nTrading Rules:", flattenList(model$rules.list), "\nPerformance:", paste("CumPnL:", round(model$performance$CumPnL,2), "/ SharpeRatios:", paste(round(model$performance$SharpeRatios,2), collapse="/")))
  names.list <- list("name","signal.list","rules.list","performance")
  lapply(names.list, 
         function(model.name) 
           paste(model.name, ": ", flattenList(model[[model.name]]), "\n" ,sep="") )  # end lapply
}  # end title.alphaModel



#####################################
### alphaModel Utility Functions  ###
#####################################

### Calculate rolling xts quantiles over endpoints
rollingQuantiles <- function(ts.data, end.period, look.back, n.quantiles=c(0.1,0.9), expand.window=FALSE) {
  stopifnot(inherits(ts.data, "xts") && !any(is.na(ts.data)))
# calculate endpoints
  end.points <- if(inherits(end.period, "numeric"))
    seq(1, nrow(ts.data), end.period)
  else
    tail(endpoints(ts.data, end.period), -1)
  stopifnot(look.back < length(end.points)-1)

# initialize Data
  vol.data <- xts(rep(NA, length(ts.data)), order.by=index(ts.data))
  vol.data <- cbind(vol.data, vol.data)

# loop over endpoints
  for (end.point in (look.back+1):(length(end.points)-1)) {
#    print(paste("endpoint", end.point, "datetime", index(ts.data)[now.point]))
    now.point <- end.points[end.point]

# find calc window
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

    window.data <- ts.data[calc.window, ]
#    window.data.plus <- ts.data[calc.window.plus, ]

# calculate vol over window.data
    vol.data[now.point,] <- quantile(window.data, n.quantiles)

  }  # end for loop over endpoints

# prepare and return data
  vol.data[1,] <- na.omit(vol.data)[1,]
  vol.data <- na.locf(vol.data)
  colnames(vol.data) <- paste(paste(n.quantiles,"confi"), colnames(ts.data))
  vol.data
}  # end rollingQuantiles



### functional apply_rolling() 
# apply a function over a vector of endpoints. 
# Functional apply_rolling() is a generalization of functional period.apply() from package xts.
# Boolean "expand_margin" controls whether to expand the rolling margin. 
# if (look_back>1) then the function is applied over overlapping intervals
# func_tion() should return a numeric vector, not a matrix
apply_rolling <- function (x_ts, end_points, func_tion, look_back=1, expand_margin=FALSE, ...) {
# coerce x_ts into an xts series, but don't throw error if fails
  x_ts <- try.xts(x_ts, error=FALSE)
# extract function from name
  func_tion <- match.fun(func_tion)
  len_gth <- length(end_points)
# define starting points of intervals
# if expand_margin=TRUE then always start at first point
#  if (expand_margin)
#    start_points <- rep_len(1, len_gth)
#  else
  start_points <- end_points[c(rep_len(1, look_back), 1:(len_gth-look_back))]
# perform sapply() loop over length of end_points
  out_put <- sapply(2:len_gth, function(in_dex) {
    func_tion(x_ts[(start_points[in_dex] + 1):end_points[in_dex]], ...)
  })  # end sapply
# coerce out_put into matrix and transpose it
  if (is.vector(out_put))
    out_put <- t(out_put)
  out_put <- t(out_put)
# add colnames to out_put
  if (is.null(colnames(out_put)) && NCOL(x_ts)==NCOL(out_put))
    colnames(out_put) <- colnames(x_ts)
# coerce out_put into xts series
  reclass(out_put, match.to=x_ts[end_points])
}  # end apply_rolling


library(HighFreq)
### functional apply_xts() 
# apply a function over an xts series and a vector of endpoints. 
# Functional apply_xts() is a generalization of functional period.apply() from package xts.
# Boolean "expand_margin" controls whether to expand the rolling margin. 
# func_tion() should return an xts series with a single row
apply_xts <- function (x_ts, end_points, func_tion, look_back=1, expand_margin=FALSE, ...) {
# extract function from name
  func_tion <- match.fun(func_tion)
  len_gth <- length(end_points)
# if expand_margin=TRUE then always start at first point
#  if (expand_margin)
#    start_points <- rep_len(1, len_gth)
#  else
  start_points <- end_points[c(rep_len(1, look_back), 1:(len_gth-look_back))]
# perform lapply() loop over length of end_points
  agg_regations <- lapply(2:len_gth, function(in_dex) {
    func_tion(x_ts[(start_points[in_dex] + 1):end_points[in_dex]], ...)
  })  # end lapply
# recursively "rbind" the list into a single xts
  agg_regations <- do_call_rbind(agg_regations)
# add colnames to agg_regations
  if (is.null(colnames(agg_regations)) && NCOL(x_ts)==NCOL(agg_regations))
    colnames(agg_regations) <- colnames(x_ts)
  agg_regations
}  # end apply_xts


