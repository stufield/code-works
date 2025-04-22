################################
# mixed effects models
# in R from Crawley R Bible pg.
################################
my_LME <- function() {
  data(Ovary)
  attach(Ovary)
  print(names(Ovary))
  print(plot(Ovary, as.table = TRUE))  # as.table orders panels top > bottom
  model1 <- lme(follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time),
                data = Ovary, random = ~1 | Mare)
  x11()
  print(plot(ACF(model1), alpha = 0.05, main = "model 1"))
  
  model2 <- update(model1, correlation = corARMA(q = 2)) # moving average time series
  model3 <- update(model2, correlation = corAR1())       # 1st order autoregressive model
  
  # check residuals
  x11()
  print(plot(model3, resid(., type = "p") ~ fitted(.) | Mare,
             as.table = TRUE, main = "Residuals"))

  # normality assumption?
  x11()
  print(qqnorm(model3, ~resid(.) | Mare, as.table = TRUE))

  detach(Ovary)
  list(NoAutocorrelation = summary(model1),  # no correlation in residuals
       Autocorrelation = summary(model2),    # correlation in residuals
       Compare_1vs2 = anova(model1, model2),
       Compare_2vs3 = anova(model2, model3)
  )
}
