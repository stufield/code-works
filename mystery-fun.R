
mystery_fun <- function(theta) {
  20 * dnorm(x = theta, mean = -1, sd = 5) +
    ifelse(theta > -1,
           6 * dgamma(x = theta + 1, shape = 2, scale = 0.5),
           dgamma(x = -theta, shape = 5, scale = 0.2)) +
    2 * dgamma(x = 3 - theta, shape = 4, scale = 0.25)
}

mystery_fun(0.3)

curve(mystery_fun(theta = x), from = -3, to = 6)

z <- integrate(mystery_fun, lower = -20, upper = 20)$value
z

