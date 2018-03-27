grid <- expand.grid(rep=1:10,
                    inputStdDev=exp(seq(-2,.5,.2)),
                    activities=64,
                    sampleSize=992,
                    sd=NA)

for (gx in 1:nrow(grid)) {
  activities <- grid[gx,'activities']
  sampleSize <- grid[gx,'sampleSize']
  
  theta <- rnorm(activities, sd = grid[gx, 'inputStdDev'])
  d1 <- rep(NA, sampleSize)
  for (dx in 1:sampleSize) {
    d1[dx] <- diff(theta[sample.int(length(theta),2)])
  }
  grid[gx,'sd'] <- sd(d1)
}

summary(lm(sd ~ inputStdDev + 0, data=grid))

# set slope to mean(sigma)/1.4
