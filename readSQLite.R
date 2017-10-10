#!/usr/bin/env Rscript
# LOADING LIBRARIES
# ***********************************************
library(RSQLite)
library(lattice)
library(latticeExtra)
library(tidyr)
setwd("/home/multicultivator/multicultivator/data/")
# rm(list=ls())


# make list of database files in data folder
dblist <- list.files(pattern=".db")
cat(c("List of database files:\n", dblist, "\n"))
for (SQliteFile in dblist) {

  # IMPORTING DATA FROM SQLite
  # ***********************************************
  # First, define path to SQLite database and connect
  db <- dbConnect(SQLite(), dbname=SQliteFile)
  cat(c("The following tables are contained in", SQliteFile,";\n" ,dbListTables(db)))
  if (any(dbListTables(db) %in% "measurement")) {
    dat <- dbReadTable(db, "measurement")
    cat("\nExtracting measurements...")
  
    # Reshaping data
    # Now that we have imported measurements, reformat time by eliminating
    # seconds; and transform to time object
    dat$time <- strptime(dat$time, format="%Y-%m-%d %H:%M")
    # simple substraction allows calculation of batchtime
    dat$batchtime_h <- difftime(dat$time, dat[1, "time"], units="hours")
    # change number of vessels from 0-7 to 1-8
    dat$channel_id <- as.numeric(dat$channel_id)+1
    
    } else
  stop("\nNo measurements in this database")
  
  if (any(dbListTables(db) %in% "turbidostat")) {
    turb <- dbReadTable(db, "turbidostat")
    cat("\nExtracting turbidostat data...\n")
    
    # For turbidostat data, also transform time
    turb$time <- strptime(turb$time, format="%Y-%m-%d %H:%M")
    # eliminate double entries in turb data
    doubles <- with(turb, {
      time <- as.character(time)
      time %in% names(which(table(time)>8))
    })
    turb <- subset(turb, !doubles)
    
    # merge dat and dilution data by time and 720 nm wavelength only
    dat$dilution <- rep(NA, nrow(dat))
    index <- dat$time %in% turb$time & dat$od_led==720
    dat[index , "dilution"] <- turb[turb$time %in% dat$time, "decision"]
  } else
    cat("\nNo turbidostat data in this database\n")


  # OD CORRECTION
  # ***********************************************
  # Per channel and per wavelength OD correction based on
  # first n hour's measurements
  ODcorr <- with(subset(dat, batchtime_h <= 1), {
    # calculate median per channel and led and ...
    ODcorr.table <- tapply(od_value, list(channel_id, od_led), median)
    ODcorr.table <- gather(as.data.frame(ODcorr.table), od_led, od_value)
    # subtract raw OD values from mean to obtain correction factor
    ODcorr <- with(ODcorr.table, 
      tapply(od_value, od_led, function(x) {
        mean(x)-x
      })
    )
    ODcorr
  })
  print(ODcorr)

  # the actual channel-wise correction is applied as formula y = m*x
  ODcorr.func <- function(x, chan, wl) {
    x+ODcorr[[wl]][chan]
  }
  # iteratively applied to every element of x with recycled arguments channel annd wavelength
  dat$od_value_corr <- mapply(ODcorr.func, x=dat$od_value, chan=1:8, wl=rep(c("680","720"), each=8))
  

  # EXPORT DATA
  # ***********************************************
  # first construct filename from original db name
  csvname <- gsub("\\.db", "\\.csv", SQliteFile)
  cat(paste("Saving", csvname, "to folder", getwd(),"\n\n"))
  # finally write modified csv table into data folder
  write.csv(dat, file=csvname)
}
