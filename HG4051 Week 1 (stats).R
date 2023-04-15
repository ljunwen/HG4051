# sets up a dataframe with the x- and y-values according to the standard normal distribution using the function 'dnorm'

x <- seq(from = -5, to = 5, by = 0.01)
dist <- as.data.frame(x)
dist$y <- dnorm(x, mean = 0, sd = 1)


# plots the graph using the x- and y-values in the dataframe

plot(dist$x,dist$y, lty = 1, type = "l", ylim = c(0,1), col = "blue")


# changing the mean and standard deviation of the distribution

mean <- 2
stdev <- 0.5
dist$y <- dnorm(x, mean = mean, sd = stdev)
lines(dist$x,dist$y, lty = 2, col = "blue")


# transforming the distribution back to the standard normal distribution

dist$x <- (dist$x - 2)/0.5
dist$y <- dist$y*0.5
lines(dist$x,dist$y, lty = 2, col = "red")