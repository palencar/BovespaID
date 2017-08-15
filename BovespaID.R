# Author: Paulo Alencar
# github: https://github.com/palencar
#
# Uses Marcelo Perlin's GetHFData package to get intraday data from Bovespa FTP.
# Agregate into OHLC xts time series to use with quantmod.
# Optionally can be adjusted from splits and dividends data from yahoo.
# adjustOHLC.intraday and adjRatios.intraday were adapted from quandtmod and TTR packages.
#
#* Files will be created in 'ftp files', 'datacache', 'intraday' and 'adjusted' directories.
#* Fetch the data with get.data function ('ftp files' and 'datacache' directories).
#* Export to rds files with export.data function ('intraday' and 'adjusted' directories).

library(GetHFData)
library(data.table)
library(quantmod)
library(xts)


get.data <- function(my.assets = NULL, time.frame = "1M", start.date = as.Date("2015-06-01"), end.date = Sys.Date(),
                     cache.dir = "datacache/", append = FALSE)
{
  first.date <- seq(start.date, end.date, by="1 week")
  last.date  <- c(tail(first.date, length(first.date)-1)-1, end.date)

  type.output <- 'raw'
  type.market <- 'equity'

  symbol.list <- c()

  map <- new.env(hash=T, parent=emptyenv())

  for(i in 1:length(first.date))
  {
    cat(paste0("\nCaching data from ", first.date[i], " to ", last.date[i], "\n"))

    df.out <- ghfd_get_HF_data(my.assets = my.assets,
                               type.market = type.market,
                               first.date = first.date[i],
                               last.date = last.date[i],
                               type.output = type.output,
                               only.dl = FALSE)

    for(symbol in unique(df.out$InstrumentSymbol))
    {
      symbolData <- df.out[df.out$InstrumentSymbol == symbol,]

      df <- xts(data.frame(open=symbolData$TradePrice, high=symbolData$TradePrice,
                       low=symbolData$TradePrice, close=symbolData$TradePrice,
                       volume=symbolData$TradedQuantity), symbolData$TradeDateTime)

      df <- switch(time.frame,
                   "1M" = to.minutes(df),
                   "3M" = to.minutes3(df),
                   "5M" = to.minutes5(df),
                   "10M" = to.minutes10(df),
                   "15M" = to.minutes15(df),
                   "30M" = to.minutes30(df),
                   "1H" = to.hourly(df))

      df <- align.time(df)

      names(df) <- c("Open", "High", "Low", "Close", "Volume")

      file.name <- paste0(cache.dir, paste(symbol, time.frame, sep="."), ".csv")

      append <- (append || !is.null(map[[symbol]]))

      fwrite(as.data.table(df), file = file.name, append = append, row.names = FALSE, dateTimeAs = "write.csv")

      map[[symbol]] <- max(symbolData$SessionDate)

      rm(df)
      rm(symbolData)
    }

    symbol.list <- unique(c(symbol.list, df.out$InstrumentSymbol))

    rm(df.out)
    gc()
  }

  return(symbol.list)
}

export.data <- function(time.frame = "1M", cache.dir = "datacache/",
                        save.files = c("intraday", "adjusted"))
{
  if("adjusted" %in% save.files)
    dir.create("adjusted", showWarnings=FALSE)

  if("intraday" %in% save.files)
    dir.create("intraday", showWarnings=FALSE)

  file.names <- list.files(cache.dir, paste0(time.frame, ".csv"))

  for(file.name in sort(file.names))
  {
    symbol <- unlist(strsplit(file.name, "[.]"))[1]

    cat(paste("Exporting:", symbol, "\n"))

    dt <- fread(paste0(cache.dir, file.name), data.table = TRUE)
    dt[, index := as.POSIXct(index)]
    xt <- as.xts.data.table(dt)

    if("intraday" %in% save.files)
    {
      file.name <- paste0("intraday", "/", symbol, ".", time.frame, ".rds")
      cat(paste0("Saving ", file.name, "\n"))
      saveRDS(xt, file.name)
    }

    if("adjusted" %in% save.files)
    {
      cat(paste0("Adjusting: ", symbol, "\n"))
      file.name <- paste0("adjusted", "/", symbol, ".", time.frame, ".rds")
      cat(paste0("Saving ", file.name, "\n"))

      try({
        xt <- adjustOHLC.intraday(xt, symbol.name = paste0(symbol, ".SA"))
        saveRDS(xt, file.name)
      })
    }
  }
}

adjustOHLC.intraday <- function (x, adjust = c("split", "dividend"), use.Adjusted = FALSE,
                                 ratio = NULL, symbol.name = deparse(substitute(x)))
{
  if (is.null(ratio)) {
    if (use.Adjusted) {
      if (!has.Ad(x))
        stop("no Adjusted column in 'x'")
      ratio <- Ad(x)/Cl(x)
    }
    else {
      div <- getDividends(symbol.name, from = "1949-01-01")
      splits <- getSplits(symbol.name, from = "1949-01-01")
      if (is.xts(splits) && is.xts(div) && nrow(splits) >
          0 && nrow(div) > 0)
        div <- div * 1/adjRatios.intraday(splits = base::merge(splits, index(div)))[, 1]
      ratios <- adjRatios.intraday(splits, div, Cl(x))
      if (length(adjust) == 1 && adjust == "split") {
        ratio <- ratios[, 1]
      }
      else if (length(adjust) == 1 && adjust == "dividend") {
        ratio <- ratios[, 2]
      }
      else ratio <- ratios[, 1] * ratios[, 2]
    }
  }
  Adjusted <- Cl(x) * ratio
  structure(
    cbind((ratio * (Op(x) - Cl(x)) + Adjusted),
          (ratio * (Hi(x) - Cl(x)) + Adjusted),
          (ratio * (Lo(x) - Cl(x)) + Adjusted),
          Adjusted,
          if (has.Vo(x)) Vo(x) else NULL,
          if (has.Ad(x)) Ad(x) else NULL
          ),
          .Dimnames = list(NULL, colnames(x)))
}

adjRatios.intraday <- function (splits, dividends, close)
{
  if (!missing(dividends) &&
      missing(close))
    stop("\"close\" must be specified to adjust dividends")

  if (missing(close) || all(is.na(close)) || NROW(close) == 0) {
    close <- NA
  }
  else {
    if (NCOL(close) != 1)
      stop("\"close\" must be univariate")
    close <- try.xts(close, error = stop("\"as.xts(close)\" failed"))
  }
  if (missing(splits) || all(is.na(splits)) || NROW(splits) == 0) {
    splits <- NA
  }
  else {
    if (NCOL(splits) != 1)
      stop("\"splits\" must be univariate")
    splits <- try.xts(splits, order.by=as.POSIXct(index(splits)), error = stop("\"as.xts(splits)\" failed"))
  }
  if (missing(dividends) || all(is.na(dividends)) || NROW(dividends) == 0) {
    dividends <- NA
  }
  else {
    if (NCOL(dividends) != 1)
      stop("\"dividends\" must be univariate")
    dividends <- try.xts(dividends, order.by=as.POSIXct(index(dividends)), error = stop("\"as.xts(dividends)\" failed"))
  }

  obj <- merge.xts(close, splits, dividends)
  #if (!isTRUE(is.na(close))) {
  #  obj <- obj[!is.na(obj[, 1]), ]
  #}
  adj <- .Call("adjRatios", obj[, 2], obj[, 3], obj[, 1], PACKAGE = "TTR")
  adj <- xts(cbind(adj[[1]], adj[[2]]), index(obj))

  colnames(adj) <- c("Split", "Div")

  return(adj)
}
