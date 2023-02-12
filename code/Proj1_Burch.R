### Code for Sensitivity Analysis of logistic model

# output variable - change in population size from now to 25 years from now

# function that takes objects r, K N0, and tmax, and returns the percent change in the population
logistic.fun <- function(r, K, N0, tmax, q, E) {
  years <- 0:tmax
  output <- rep(NA, times = length(years))
  output[1] <- N0
  n.loop <- length(years)
  for (i in 2:n.loop) {
    n.t <- output[i-1]
    n.t.plus.1 <- n.t + n.t * r * (1 - n.t / K) - (q*E*n.t)
    output[i] <- n.t.plus.1
  }
  percent.change <- 100 * (output[n.loop] - N0) / N0
  return(percent.change)
}


# Set up parameter lower and upper bounds
rlim <- c(0.025, 0.049)

#I backsolved for N(2014) which allows me to solve for q, I used the best estimate of N(2015) and the average r value
q <- 98/(277*1308.5)

  #NORTH
Nolim_nor <- c(46, 280)
Klim_nor <- c(437, 524)
E_nor <- 138

  #SOUTHWEST
Nolim_sw <- c(1032, 1665)
Klim_sw <- c(2159, 2389)
E_sw <- 82

  #SOUTHEAST
Nolim_se <- c(1399, 2344)
Klim_se <- c(4340, 5045)
E_se <- 118

# how many simulations
nsims <- 1000

# create matrix to store output
  #NORTH
mcoutput_nor <- matrix(NA, nrow = nsims, ncol = 6)
colnames(mcoutput_nor) <- c("r", "K", "No", "PercentChange", "E", "q")

  #SOUTHWEST
mcoutput_sw <- matrix(NA, nrow = nsims, ncol = 6)
colnames(mcoutput_sw) <- c("r", "K", "No", "PercentChange", "E", "q")

  #SOUTHEAST
mcoutput_se <- matrix(NA, nrow = nsims, ncol = 6)
colnames(mcoutput_se) <- c("r", "K", "No", "PercentChange", "E", "q")


# use a loop to repeat calculations
  #NORTH
for (i in 1:nsims) {
  # take random draws = uniform between lower and upper bound
  r <- runif(1, min = rlim[1], max = rlim[2])
  K <- runif(1, min = Klim_nor[1], max = Klim_nor[2])
  No <- runif(1, min = Nolim_nor[1], max = Nolim_nor[2])
  q <- q
  E <- E_nor
  # Send these to the "logistic.fun" and save the output
  per.change <- logistic.fun(r, K, No, tmax = 25, q, E)
  # Put the parameter values and model output into the matrix
  mcoutput_nor[i,] <- c(r, K, No, per.change, E, q)
}

  #SOUTHWEST
for (i in 1:nsims) {
  # take random draws = uniform between lower and upper bound
  r <- runif(1, min = rlim[1], max = rlim[2])
  K <- runif(1, min = Klim_sw[1], max = Klim_sw[2])
  No <- runif(1, min = Nolim_sw[1], max = Nolim_sw[2])
  q <- q
  E <- E_sw
  # Send these to the "logistic.fun" and save the output
  per.change <- logistic.fun(r, K, No, tmax = 25, q, E)
  # Put the parameter values and model output into the matrix
  mcoutput_sw[i,] <- c(r, K, No, per.change, E, q)
}

  #SOUTHEAST
for (i in 1:nsims) {
  # take random draws = uniform between lower and upper bound
  r <- runif(1, min = rlim[1], max = rlim[2])
  K <- runif(1, min = Klim_se[1], max = Klim_se[2])
  No <- runif(1, min = Nolim_se[1], max = Nolim_se[2])
  q <- q
  E <- E_se
  # Send these to the "logistic.fun" and save the output
  per.change <- logistic.fun(r, K, No, tmax = 25, q, E)
  # Put the parameter values and model output into the matrix
  mcoutput_se[i,] <- c(r, K, No, per.change, E, q)
}


# make some plots!
# create a 3 panel plot (3 rows, 1 column)
  #NORTH
par(mfrow = c(3,1))
for (i in 1:3) {
  plot(mcoutput_nor[,i], mcoutput_nor[,4],
       type = "p",
       pch = 21,
       bg = "black",
       ylab = "% change",
       xlab = colnames(mcoutput_nor)[i],
       cex.lab = 2,
       cex.axis = 2
  )
}

  #SOUTHWEST
par(mfrow = c(3,1))
for (i in 1:3) {
  plot(mcoutput_sw[,i], mcoutput_sw[,4],
       type = "p",
       pch = 21,
       bg = "black",
       ylab = "% change",
       xlab = colnames(mcoutput_sw)[i],
       cex.lab = 2,
       cex.axis = 2
  )
}

  #SOUTHEAST
par(mfrow = c(3,1))
for (i in 1:3) {
  plot(mcoutput_se[,i], mcoutput_se[,4],
       type = "p",
       pch = 21,
       bg = "black",
       ylab = "% change",
       xlab = colnames(mcoutput_se)[i],
       cex.lab = 2,
       cex.axis = 2
  )
}

#Quantile Summary
summary(mcoutput_nor[,4])
summary(mcoutput_sw[,4])
summary(mcoutput_se[,4])


