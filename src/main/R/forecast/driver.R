source("gbm.R")

# load, clean and cache the input data
cash <- cache("cash", {
  forecast.clean()
})

july <- gbm.forecast(cash, by="atm")
print(sum(july$totalScore))