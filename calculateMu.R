# CACULATE GROWTH RATE IN THREE DIFFERENT WAYS (BATCH, DILUTIONS, INTERVALS)
#
#
calculate.mu <- function(data, input, od_select) {
  # first, mu for continuous cultivation and determination via dilution events
  if (input$UserMuType == "conti - dilution") {
    # sum up dilutions over user-selected time frame (mu.time)
    mu.time <- as.numeric(input$UserMuTime)
    mu <- with(data, {
      # first sum up dilution over full hours and then apply sliding window
      tapply(dilution, list(round(batchtime_h), channel_id), function(x)
      {
        sum(x, na.rm = TRUE)
      }) %>% as.data.frame %>%
        rollapply(.,
                  width = mu.time,
                  by = 1,
                  FUN = sum)
    }) %>% as.data.frame
    # multiply number of dilutions with dilution factor (V_dil/V_total)
    mu <- mu * input$UserDilFactor / mu.time
    mu$batchtime_h <- unique(round(data$batchtime_h))[1:nrow(mu)]
    # reshape to long data.frame
    mu <- gather(mu, channel_id, value,-batchtime_h)
  }
  
  # second, mu for continuous cultiv. and determination via interval growth
  if (input$UserMuType == "conti - interval") {
    # function for interval growth rate calculation
    # changed by U. Hoffmann, attempt to fix a few things
    interval.mu <-
      function(time_x,
               od_values_y,
               max_OD_value = 0.2,
               filter_threshold = TRUE) {
        ## input:
        # time_x, vector: measured time values
        # od_values_y, vector: accompanying OD values
        # max_OD_value, default=0.2, float: local OD maxima should be above this value
        # filter_threshold, default=TRUE, bool: should max_OD_value be applied or not?
        ## output:
        # data frame with the following columns: max.pos, max.x, max.y, mu, r.squared, length.iv, residuals
        
        ## First step: identify local maxima
        # identify positions at which dilution took place (OD value drops), accept some downwards fluctuations in OD measurements
        positions_before_drop <-
          diff(od_values_y) < (-0.01) # vector of boolean
        # filter out stretches where dilution took place over several steps, i.e. stretches of "TRUE" - keep first in stretch
        indices_local_maxima_prelim <- which(positions_before_drop)
        if (length(indices_local_maxima_prelim) > 1) {
          differences_indices <-
            diff(indices_local_maxima_prelim) # if only one local maximum, this gives no number: only check if there is more than one local maximum
          for (i in 1:length(differences_indices)) {
            if (differences_indices[i] == 1) {
              positions_before_drop[indices_local_maxima_prelim[i] + 1] <- FALSE
            }
          }
        }
        indices_local_maxima <- which(positions_before_drop)
        if (filter_threshold) {
          indices_local_maxima <-
            indices_local_maxima[od_values_y[indices_local_maxima] > max_OD_value]
        }
        
        ## Second step: identify minima
        # interval between first and second entry in list is: seq(indices_of_maxima[1], indices_of_maxima[-1][1])
        # create dataframe with first index of interval, last index of interval and find minimum within
        # problem: if no single maximum, this will throw error --> better behaviour: calculate lm for whole stretch, ignore in this case max_OD_value that defines dilution steps
        if (length(indices_local_maxima)) {
          positions_dataframe <-
            data.frame(
              maxpos = indices_local_maxima,
              start_of_interval = c(1, indices_local_maxima[1:length(indices_local_maxima) -
                                                              1])
            ) # include first position so that first maximum is also used for calculations
        } else {
          positions_dataframe <-
            data.frame(maxpos = c(length(od_values_y)),
                       start_of_interval = c(1))
        }
        positions_dataframe$minimalpos <- NA
        for (i in 1:nrow(positions_dataframe)) {
          values <-
            od_values_y[positions_dataframe$start_of_interval[i]:positions_dataframe$maxpos[i]]
          positions_dataframe[i, ]$minimalpos <-
            positions_dataframe[i, ]$start_of_interval + which(values == min(values, na.rm =
                                                                               TRUE))[1] - 1 ## ignore NA values
        }
        
        ## Third step: run linear regression on that stuff
        positions_dataframe$slope <- NA
        positions_dataframe$rsquared <- NA
        positions_dataframe$length_interv <- NA
        positions_dataframe$residuals <- NA
        for (i in 1:nrow(positions_dataframe)) {
          indices_of_interval <-
            seq(positions_dataframe[i, ]$minimalpos,
                positions_dataframe[i, ]$maxpos)
          od_values <- log(od_values_y[indices_of_interval])
          od_values[is.infinite(od_values)] <- NA
          time_values <- time_x[indices_of_interval]
          linMod <- lm(od_values ~ time_values)
          positions_dataframe[i, ]$slope <- linMod$coefficients[2]
          positions_dataframe[i, ]$rsquared <-
            summary(linMod)$r.squared
          positions_dataframe[i, ]$length_interv <-
            length(indices_of_interval)
          positions_dataframe[i, ]$residuals <- sd(linMod$residuals)
        }
        #output: data frame with the following columns: max.pos, max.x, max.y, mu, r.squared, length.iv, residuals
        return(
          data.frame(
            max.pos = positions_dataframe$maxpos,
            max.x = time_x[positions_dataframe$maxpos],
            max.y = od_values_y[positions_dataframe$maxpos],
            mu = positions_dataframe$slope,
            r.squared = positions_dataframe$rsquared,
            length.iv = positions_dataframe$length_interv,
            residuals = positions_dataframe$residuals
          )
        )
      }
    
    # growth rate is calculated by applying interval µ function
    # for each channel individually
    mu <- by(data, data$channel_id, function(data) {
      # fitting an lm() linear model to each interval
      # between minimum and maximum
      interval.mu(data$batchtime_h,
                  data[[od_select]],
                  input$UserMaxInterval)
    }) %>% unclass(.) %>% # unclass() seems to be needed beginning from dplyr 0.7.2, compare https://github.com/tidyverse/dplyr/issues/2962
      # convert to data.frame
      bind_rows(.id = "channel") %>%
      # and set names on the fly
      setNames(
        c(
          "channel_id",
          "rowNumb",
          "batchtime_h",
          od_select,
          "value",
          "r.squared",
          "length_int",
          "residuals"
        )
      )
    
    # filter determined growth rates by r.squared and min length of interval
    # as a quality criterion
    if (input$UserMinSelect != "min-max") {
      mu <- subset(mu, r.squared > input$UserRsquared & length_int >= 3)
    }
  }
  
  # third, batch mode
  if (input$UserMuType == "batch mode") {
    # calculate growth rate per time by using linear model lm()
    #define time period for mu determination
    mu.time <- as.numeric(input$UserMuTime)
    
    # first summarize values per hour for simplicity, then apply model
    mu <- with(data, {
      df <-
        tapply(get(od_select), list(round(batchtime_h), channel_id), mean)
      apply(df, 2, function(x) {
        sapply(1:(length(x) - mu.time), function(rownumber) {
          # select OD by given time range
          OD <- log(x[rownumber:(rownumber + mu.time)])
          OD <- OD[!is.na(OD)]
          # linear model that calculates slope = spec growth rate µ
          mu.model <-
            lm(y ~ x, data.frame(y = OD, x = 1:length(OD)))
          coefficients(mu.model)[[2]]
        })
      })
    })
    # transform to df and add time
    mu <- as.data.frame(mu)
    mu$batchtime_h <- unique(round(data$batchtime_h))[1:nrow(mu)]
    # reshape to long data.frame
    mu <- gather(mu, key = "channel_id", value = "value",-batchtime_h)
  }
  mu
}