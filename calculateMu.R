## CACULATE GROWTH RATE IN THREE DIFFERENT WAYS (BATCH, DILUTIONS, INTERVALS)
#
#
calculate.mu <- function(data, input) {
  # first, mu for continuous cultivation and determination via dilution
  if (input$UserMuType == "conti - dilution") {
    # sum up dilutions over user-selected time frame (mu.time)
    # using integer division %/%
    mu.time <- as.numeric(input$UserMuTime)
    mu <- as.data.frame(with(data, 
      tapply(dilution, list(batchtime_h %/% mu.time * mu.time, channel_id), sum)
    ))
    # multiply number of dilutions with dilution factor (V_dil/V_total)
    mu <- mu * input$UserDilFactor / mu.time
    mu$batchtime_h <- as.numeric(rownames(mu))
    # remove inaccurate last value
    mu <- mu[-nrow(mu), ]
    # reshape to long data.frame
    mu <- gather(mu, channel_id, value, -batchtime_h)
  }
  
  # second, mu for continuous cultiv. and determination via interval growth
  if (input$UserMuType == "conti - interval") {
    # function for interval growth rate calculation
    interval.mu <- function(x, y, UserMinSelect, UserMaxInterval) {
      # function to compute local maxima
      # based on difference between two neighbours
      localMaxima <- function(x) {
        y <- diff(c(-Inf, x)) > 0
        y <- cumsum(rle(y)$lengths)
        y <- y[seq(1, length(y), 2)]
        if (x[1] == x[2] | y[1] == 1) {
          y <- y[-1]
        }
        # filter local maxima by manual threshold
        y[x[y] > UserMaxInterval]
      }
      
      # positions of local maxima
      max.pos <- localMaxima(y)
      
      # positions of local minimum in intervals
      if (UserMinSelect %in% c("auto", "min-max")) {
        # compute interval between two local maxima
        intervals <-
          mapply(seq, c(1, max.pos[-length(max.pos)] + 1), max.pos)
        # determine minimum as local minimum of the interval
        # to correct for lag phases
        min.pos <- unlist(lapply(intervals, function(iv)
          iv[which.min(y[iv])])) 
      } else {
        # or alternatively, minimum is n steps backward from max, user defined
        max.pos <- max.pos[max.pos > as.numeric(UserMinSelect)]
        min.pos <- max.pos - as.numeric(UserMinSelect)
      }
      
      # determine growth rate in intervals from local minima to maxima
      t(mapply(function(min.pos, max.pos) {
        # in most simple case, only min and max are considered
        if (UserMinSelect == "min-max") {
          index <- c(min.pos, max.pos)} 
        else {
          index <- seq(min.pos, max.pos)
        }
        index <- index[y[index] > 0]
        model <- lm(log(y) ~ x, data.frame(x = x[index], y = y[index]))
        list(
          max.pos = max.pos,
          max.x = x[max.pos],
          max.y = y[max.pos],
          mu = coefficients(model)[[2]],
          r.squared = summary(model)$r.squared,
          length.iv = length(index),
          residuals = sd(model$residuals)
        )
      }, min.pos, max.pos))
    }
    
    # growth rate is calculated by applying interval µ function
    # for each channel individually
    mu <- by(data, data$channel_id, function(data) {
      # fitting an lm() linear model to each interval
      # between minimum and maximum
      interval.mu(data$batchtime_h,
        data$od_value,
        input$UserMinSelect,
        input$UserMaxInterval)
    })
    
    # convert to data.frame
    mu <- ldply(lapply(mu, as.data.frame))
    mu <- as.data.frame(apply(mu, 2, function(x)
        as.numeric(unlist(x))))
    colnames(mu) <-
      c(
        "channel_id",
        "rowNumb",
        "batchtime_h",
        "od_value",
        "value",
        "r.squared",
        "length_int",
        "residuals"
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
      df <- tapply(od_value, list(round(batchtime_h), channel_id), mean)
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
    mu <- gather(mu, key="channel_id", value="value", -batchtime_h)
  }
  mu
}