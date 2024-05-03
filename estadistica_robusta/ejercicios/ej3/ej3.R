# Ejercicio 3
load(here("estadistica_robusta/ejercicios/ej3/data.RData"))

## a
Lestimator <- function(a, x) {
  if (sum(a<0) != 0) {
    stop("Los ai no son mayores que cero!")
  }

  # if (sum(a) != 1) {
  #   stop("Los ai no suman 1!")
  # }

  if (sum(a == rev(a)) == 0) {
    stop("Los ai no son simétricos!")
  }

  if (length(a) != length(x)) {
    stop("a y x no tienen la misma dimensión!")
  }
  
  sum(a %*% sort(x))
}

# Menores a cero
a <- c(0, .1, -.1)
x <- c(1, 2, 3) 
Lestimator(a,x)

# No suman 1
a <- c(0, .1, .1)
x <- c(1, 2, 3) 
Lestimator(a,x)

# No son siméetricos
a <- c(0, .1, .2, .7)
x <- c(1, 2, 3, 4) 
Lestimator(a,x)

# Distinta dimensión
a <- c(0, .1, .8, .1, 0)
x <- c(1, 2, 3, 4) 
Lestimator(a,x)

# Distinta dimensión
a <- c(0, .1, .8, .1, 0)
x <- c(1, 2, 3, 4, 5) 
Lestimator(a,x)

## c
media_winsorizada <- function(x, alpha) {
  n <- length(x)
  m <- floor(alpha*n)
  a <- rep(0, n)
  
  a[m] <- m/n
  a[seq(m+1, n-m)] <- 1/n
  a[n-m+1] <- m/n
  
  Lestimator(a, x)
}

## d
x <- rnorm(20)
media_winsorizada(x, .2)

media_winsorizada(data, .1)
media_winsorizada(data, .2)
