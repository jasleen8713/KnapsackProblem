#' Function to solve knapsack problem using dynamic programming
#' @param x as data.frame of weights and value
#' @param W as maximum kapsack weight
#' @return a list
#' @export
knapsack_dynamic <- function(x,W){

  # Check for inputs
  stopifnot(is.data.frame(x))
  stopifnot(c("v", "w") %in% names(x))
  stopifnot(is.numeric(x$w)&&is.numeric(x$v))
  stopifnot(any(x$w <= W) || any(x$v > 0) )
  stopifnot(W > 0)

  # Remove all the values before hand whose weights are greater than the given Max weight W
  discard <- which(x$w > W)
  if(length(discard)>0){
    x <- x[-1, ]
  }
  # order them according to weight
  x <- x[order(x$w), ]

  main_matrix <- matrix(0, nrow=nrow(x)+1, ncol = W+1)
  w <- x$w
  v <- x$v
  elements <- c()

  for(i in 2:(nrow(x)+1)){
    for(j in 1:(W+1)){
      if(w[i-1] > j){
        main_matrix[i,j] <- main_matrix[i-1, j] # here j is W
      }
      else{
        main_matrix[i,j] <- max((v[i-1] + main_matrix[i-1, j-w[i-1]]), (main_matrix[i-1, j]))
      }
    }
  }

  i <- nrow(main_matrix)
  j <- ncol(main_matrix)
  value <- main_matrix[i , j]

  while(i > 1 & j >1){
    if(main_matrix[i,j] != main_matrix[i-1,j]){
      elements <- c(elements, rownames(x)[i-1])
      j <- j - x$w[i - 1]
    }
    i <- i - 1
  }

  return (list(value = round(main_matrix[nrow(main_matrix), ncol(main_matrix)]), elements = as.numeric(sort(elements))))
}

