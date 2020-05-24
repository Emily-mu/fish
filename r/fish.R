library(tidyverse)
library(haven)
library(AER)
read_data <- function(df)
{
  full_path <- paste("https://storage.googleapis.com/causal-inference-mixtape.appspot.com/", 
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

fish <- read_data("fish.dta")

lnq <- log(fish$quantity)
lnp <- log(fish$price)

lmfit <- lm(lnq ~ lnp + mon + tues + wed + thurs, data = fish)
stargazer::stargazer(lmfit, type = "html", out = "table/lmfit.htm")

ivfit1 <-ivreg(lnq ~ lnp + mon + tues + wed + thurs|mon + tues + wed + thurs + wave2, 
               data = fish)
ivfit2 <-ivreg(lnq ~ lnp + mon + tues + wed + thurs|mon + tues + wed + thurs + speed3, data = fish)
stargazer::stargazer(ivfit1, ivfit2, type = "html", out = "table/ivfit.htm")
stargazer::stargazer(summary(ivfit1, diagnostics = TRUE)$diagnostics, 
                     summary(ivfit2, diagnostics = TRUE)$diagnostics,
                     type = "html", out = "table/firststage.htm",
                     title = c("IV: wave2","IV: speed3"))
