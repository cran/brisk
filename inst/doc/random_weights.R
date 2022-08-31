## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(brisk)
library(dplyr)

benefit_fun <- approxfun(c(0, 0.5, 1), c(0, 0.2, 1))
risk_fun <- approxfun(c(0, 0.3, 0.6, 1), c(1, 0.9, 0.2, 0))

# weights
w1 <- runif(1e4, 0.6, 0.8)
w2 <- runif(1e4, 0.2, 0.3)

set.seed(1132)
out <- br(
  benefit("response", benefit_fun, weight = w1),
  risk("side_effect", risk_fun, weight = w2),
  br_group(
    label = "placebo",
    response = rbeta(1e4, 1 + 30, 1 + 70),
    side_effect = rbeta(1e4, 1 + 3, 1 + 97)
  ),
  br_group(
    label = "drug",
    response = rbeta(1e4, 1 + 60, 1 + 40),
    side_effect = rbeta(1e4, 1 + 40, 1 + 60)
  )
)

head(select(out, iter, response_weight, side_effect_weight))

## -----------------------------------------------------------------------------
w <- sim_weights(1e4, response = c(1, 1), side_effect = c(0.2, 0.3))
head(w)
summary(w$side_effect / w$response)

## -----------------------------------------------------------------------------
out2 <- br(
  benefit("response", benefit_fun, weight = w$response),
  risk("side_effect", risk_fun, weight = w$side_effect),
  br_group(
    label = "placebo",
    response = rbeta(1e4, 1 + 30, 1 + 70),
    side_effect = rbeta(1e4, 1 + 3, 1 + 97)
  ),
  br_group(
    label = "drug",
    response = rbeta(1e4, 1 + 60, 1 + 40),
    side_effect = rbeta(1e4, 1 + 40, 1 + 60)
  )
)

head(select(out2, iter, response_weight, side_effect_weight))

