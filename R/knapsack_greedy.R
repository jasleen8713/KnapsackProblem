#' Function to solve knapsack problem using brute force search
#' @param x as data.frame of weights and profits with
#' @param W as maximum kapsack weight
#' @return a list
#' @export
greedy_knapsack <- function(x,W){

  # Check for inputs
  stopifnot(is.data.frame(x))
  stopifnot(c("v", "w") %in% names(x))
  stopifnot(is.numeric(x$w)&&is.numeric(x$v))
  stopifnot(any(x$w <= W) || any(x$v > 0) )
  stopifnot(W > 0)

  # initialization
  total_weight <- 0
  total_value <- 0
  elements <- c()

  # Remove all the values before hand whose weights are greater than the given Max weight W
  discard <- which(x$w > W)
  if(length(discard)>0){
    x <- x[-1, ]
  }

  # Need ration of v/w
  x$p_val <- x$v/x$w
  x <- x[order(x$p_val, decreasing=TRUE), ]


  for(i in 1:nrow(x)){

    w <- total_weight + sum(x$w[i])

    if(w <= W){
      total_weight <- w
      total_value <- total_value + sum(x$v[i])
      elements <- c(elements, rownames(x[i, ]))
    }
    else if (w > W)(
      return(list(value = round(total_value, 0), elements = as.numeric(elements)))
    )
    if(i == nrow(x)) {
      return(list(value = round(total_value, 0), elements = as.numeric(elements)))
    }
  }

}


