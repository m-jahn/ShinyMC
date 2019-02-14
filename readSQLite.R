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
    
    # optional correction of OD readings by using a modified inverse Gompertz 
    # function. The fit has been obtained by a correlation curve between PR and MC
    OD_PR <- function(x, alpha, beta, gamma, k) -log(-log((x-gamma)/alpha)/beta)/k
    # apply function for 680 and 720 nm separately
    dat$od_corr <- OD_PR(x=log(dat$od_value),
      alpha=-9.7834454, beta=1.9832230, gamma=1.2797354, k=-0.2858521) %>% exp
    dat[dat$od_led==720, "od_corr"] <- OD_PR(x=log(dat[dat$od_led==720, "od_value"]), 
      alpha=-8.9527641, beta=2.2338071, gamma=0.6507006, k=-0.3088536) %>% exp
    
  } else
  stop("\nNo measurements in this database")
  
  if (any(dbListTables(db) %in% "turbidostat")) {
    turb <- dbReadTable(db, "turbidostat")
    cat("\nExtracting turbidostat data...\n")
    
    # For turbidostat data, also transform time
    turb$time <- strptime(turb$time, format="%Y-%m-%d %H:%M")
    # eliminate double or incomplete entries in turb data
    doubles <- with(turb, {
      time <- as.character(time)
      time %in% names(which(table(time)!=8))
    })
    turb <- subset(turb, !doubles)
    
    # merge dat and dilution data by time and 720 nm wavelength only
    dat$dilution <- rep(NA, nrow(dat))
    index <- dat$time %in% turb$time & dat$od_led==720
    dat[index , "dilution"] <- turb[turb$time %in% dat$time, "decision"]
  } else
    cat("\nNo turbidostat data in this database\n")


  # EXPORT DATA
  # ***********************************************
  # first construct filename from original db name
  csvname <- gsub("\\.db", "\\.csv", SQliteFile)
  cat(paste("Saving", csvname, "to folder", getwd(),"\n\n"))
  # finally write modified csv table into data folder
  write.csv(dat, file=csvname)
  
  dbDisconnect(db)
}
