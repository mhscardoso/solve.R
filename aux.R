Ad1 <- function(order) {
  m <- matrix(0, order ** 2, order ** 2)
  for (i in 1:order) {
    m[i, i] <- 1
  }
  
  for (i in (order + 1):(order ** 2)) {
    m[i, i] <- 1
    m[i, i - order] <- -1
  }
  
  return(m)
}

Ad2 <- function(order) {
  m <- matrix(0, order ** 2, order ** 2)
  for (i in 1:order) {
    m[i, i] <- -2
    m[i, i + order] <- 1
  }
  
  for (i in (order + 1):(order ** 2 - order)) {
    m[i, i] <- -2
    m[i, i - order] <- 1
    m[i, i + order] <- 1
  }
  
  for (i in (order ** 2 - order + 1):(order ** 2)) {
    m[i, i] <- -2
    m[i, i - order] <- 1
  }
  
  return(m)
}

Bd1 <- function(order) {
  m <- matrix(0, order ** 2, order ** 2)
  m[1, 1] <- 1
  
  for (i in 2:(order ** 2)) {
    m[i, i] <- 1
    m[i, i - 1] <- -1
  }
  
  return(m)
}

Bd2 <- function(order) {
  m <- matrix(0, order ** 2, order ** 2)
  
  m[1, 1] <- -2
  m[1, 2] <- 1
  
  for (i in 2:(order ** 2 - 1)) {
    m[i, i] <- -2
    m[i, i + 1] <- 1
    m[i, i - 1] <- 1
  }
  
  m[order ** 2, order ** 2] <- -2
  m[order ** 2, order ** 2 - 1] <- 1
  
  return(m)
}

k1 <- function(order, initial, limit) {
  m <- matrix(0, order ** 2, 1)
  X <- seq(initial, limit, (limit - initial) / (order - 1))
  
  for (i in 1:order) {
    for (j in 1:order) {
      m[i + (j - 1) * n] <- 2 * X[i] + X[j] + 2
    }
  }
  
  p <- matrix(0, order ** 2, order ** 2)
  for (i in 1:(order ** 2)) {
    p[i, i] <- m[i]
  }
  
  return(p)
}

k2 <- function(order, initial, limit) {
  m <- matrix(0, order ** 2, 1)
  X <- seq(initial, limit, (limit - initial) / (order - 1))
  
  for (i in 1:order) {
    for (j in 1:order) {
      m[i + (j - 1) * n] <-  X[i] ** 2 + 3 * X[j] + 1
    }
  }
  
  p <- matrix(0, order ** 2, order ** 2)
  for (i in 1:(order ** 2)) {
    p[i, i] <- m[i]
  }
  
  return(p)
}