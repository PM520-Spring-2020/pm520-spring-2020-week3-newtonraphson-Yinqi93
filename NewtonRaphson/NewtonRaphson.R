newton <- function(fx, fx2, start, tol = 1e-3, maxitr = 1000){
  res <- start
  distance <- -Inf
  x1 <- start
  i <- 1
  while(i <= maxitr & abs(distance) > tol){
    x2 <- x1 - fx(x1) / fx2(x1)
    res <- c(res, x2)
    distance <- abs(x2 - x1)
    x1 <- x2
    print(paste0("iteration ", i, ": ", "X = ", x1, ", Y = ", fx(x1)))
    i <- i + 1
  }
  if(abs(distance) > tol){
    print("fail to converge, please try another start value")
  }
  print(paste0("converged! the root is ", x1))
  return(list(root = x2,
              res = res))
}

plot.process <- function(fx, fx2, res, xrange = c(-1, 1)){
  grid <- seq(from = xrange[1], to = xrange[2], length.out = 1000)
  plot(grid, fx(grid), type = "l", xlab = "x", ylab = "y")
  abline(a = 0, b = 0)
  for(i in 1:length(res)){
    x <- res[i]
    segments(x0 = x, y0 = 0, x1 = x, y1 = fx(x), col = "red")
    if(i > 1){
      segments(x0 = x, y0 = 0, x1 = res[i - 1], y1 = fx(res[i - 1]), col = "red", lty = 5)
    }
  }
}


##### f(x) = x^2 ####
fx.1 <- function(x){
  return(x^2)
}
fx2.1 <- function(x){
  return(2 * x)
}

temp <- newton(fx = fx.1, fx2 = fx2.1, start = 1)
plot.process(fx.1, fx.2, res = temp$res)


#### f(x) = (x - 2)^3 - 6x
fx.1 <- function(x){
  return((x - 2)^3 - 6 * x)
}
fx2.1 <- function(x){
  return(3 * (x - 2)^2 - 6)
}

temp <- newton(fx = fx.1, fx2 = fx2.1, start = 3)
plot.process(fx.1, fx.2, res = temp$res, xrange = c(-3, 7))


#### f(x) = sin(x) ####
fx.1 <- function(x){
  return(sin(x))
}
fx2.1 <- function(x){
  return(cos(x))
}

temp <- newton(fx = fx.1, fx2 = fx2.1, start = 2)
plot.process(fx.1, fx.2, res = temp$res, xrange = c(0, 7))


#### f(x) = cos(x) - x ####
fx.1 <- function(x){
  return(cos(x) - x)
}
fx2.1 <- function(x){
  return(-sin(x) - 1)
}

temp <- newton(fx = fx.1, fx2 = fx2.1, start = 0)
plot.process(fx.1, fx.2, res = temp$res, xrange = c(-1, 2))

