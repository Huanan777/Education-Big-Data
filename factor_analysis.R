library(lavaan)

data1 <- read.csv("data_1.csv", stringsAsFactors = FALSE)

ED.model <- '
  future =~ x1 + x2 + x3 + x4 + x5
  barrier =~ x6 + x7
  efficiency =~ m1 + m2 + m3 + m4 + m5
  outcome =~ m6 + m7 + m8
  interest =~ y1 + y2 + y3
  hope =~ y5 + y6 + y7
  '

model.fit <- cfa(ED.model, data = data1)

summary(model.fit, fit.measures = TRUE)
