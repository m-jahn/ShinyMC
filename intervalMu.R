interval.mu <- function(x, y, UserMinSelect, UserMaxInterval){
  
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
  if (UserMinSelect=="auto") {
    # compute interval between two local maxima
    intervals <- mapply(seq, c(1, max.pos[-length(max.pos)]+1), max.pos)
    # position of minimum +3, to avoid underestimation of µ at turning point
    min.pos <- unlist(lapply(intervals, function(iv) iv[which.min(y[iv])]))+3
  } else {
    # or alternatively, minimum is n steps backward from max, user defined
    max.pos <- max.pos[max.pos > as.numeric(UserMinSelect)]
    min.pos <- max.pos - as.numeric(UserMinSelect)
  }
  
  # determine growth rate in intervals from local minima to maxima
  t(mapply(function(min.pos, max.pos) {
    index <- seq(min.pos, max.pos)
    index <- index[y[index] > 0]
    model <- lm(log(y) ~ x, data.frame(x=x[index], y=y[index]))
    list(
      max.pos=max.pos,
      max.x=x[max.pos],
      max.y=y[max.pos],
      mu=coefficients(model)[[2]], 
      r.squared=summary(model)$r.squared,
      length.iv=length(index),
      residuals=sd(model$residuals)
    )
  }, min.pos, max.pos))
}