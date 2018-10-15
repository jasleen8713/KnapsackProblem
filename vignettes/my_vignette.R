## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
set.seed(42)
n <- 2000
knapsack_objects <-
data.frame(
w=sample(1:4000, size = n, replace = TRUE), v=runif(n = n, 0, 10000)
)

## ------------------------------------------------------------------------
KnapsackProblem::brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

