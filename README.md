# BovespaID
Fetch public trade data from Bovespa Stocks and converts into OHLC xts time series to use with quantmod in R

This script uses [GetHFData](https://github.com/msperlin/GetHFData/) package to download intraday data from Bovespa FTP.
The trade price, time and volume information is stored as OHLC time series on *csv* format inside *datacache* directory.

## Data Volume
These files are big, some have about **700MB** after uncompressed for every trade session. They are stored in the *ftp files* directory.
At this date they sum up to **11Gb** on zip format. The computer need to have enought memory to handle up to 5 trade sessions at time.

## Install Packages
To install *GetHFData*, *quantmod* and *data.table* packages required, type the following on the *R console*:
```R
install.packages(c('GetHFData', 'quantmod', 'data.table'))
```

## Get Data
Go to the working directory and call the *R* console. Call *get.data* function specifiying the desired assets on *my.assets*
parameter or **NULL** to all on the period. Be aware of the storage and working memory required.

The default *time.frame* parameter is **1M** (one minute). It is possible to chose any of the following: *1M*, *3M*, *5M*, *10M*, *15M*, *30M* or *1H*.

```R
source('BovespaID.R')
get.data(my.assets = c('BVMF3'))
```
### Append Data
If already there is previous data stored on *datacache*, it is possible to append the *csv* files specifying the *assets*, *start.date*
with the starting date and *append* as **true**. Type the following on the *R* console:

```R
get.data(my.assets = c('BVMF3'), start.date = as.Date('2017-08-01'), append = TRUE)
```
### Export Data

### Convert to xts rds Files
To export the data as *xts* times series on *rds* files, call the *export.data* function with *save.files* parameter as **intraday**.
The resulting files will be stored in *intraday* directory.
```R
export.data(save.files = 'intraday')
```
### Adjust to Splits and Dividends
Call *export.data* function with *save.files* parameter as **adjusted**. The files will be stored in *adjusted* directory.
Some assets won't will be adjusted because don't have *splits* or *dividends* info on yahoo.
```R
export.data(save.files = 'adjusted')
```
