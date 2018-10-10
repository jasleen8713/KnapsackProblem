#' Function to solve knapsack problem using brute force search
#' @param x as data.frame of weights and profits with
#' @param W as maximum kapsack weight
#' @return a list
#' @export

brute_force_knapsack <- function(x,W){

  #check if inputs are correct
  if(is.data.frame(x) && "w" %in% colnames(x) && "v" %in% colnames(x)
     && all(x$w > 0) && all(x$v > 0) && is.numeric(W))
  {
    totalitems <- nrow(x)
    choices <- numeric(totalitems) #vector of 0s
    value <- 0
    elements <- c()

    #Since for n items there can have 2^n possible comibinations.
    for (i in c(1:2^totalitems))
    {
      tempWeight <- 0
      tempValue <- 0
      index <- c()

      #update choices vector with 0-1s to create all possible combinations
      for(j in (1:totalitems))
      {
        if(choices[j]!=0)
        {
          choices[j] <- 0
        }
        else
        {
          break
        }
      }
      choices[j] <- 1

      for (k in c(1:totalitems))
      {
        if(choices[k]==1)
        {
          tempWeight <- tempWeight + x[k,]$w;
          tempValue <- tempValue + x[k,]$v;
          index[length(index)+1] <- k #capture the index combinations
        }
      }

      #check if value is better and total weight has still not crossed maximum limit
      if (tempValue > value && tempWeight <= W) {
        value <- tempValue
        elements <- index #capture the final indexes being considered
      }
    }
    return(list(value=value,elements=elements))
  }
  else
  {
    stop("Please check the input. It should have two variable x and W.
          x should be a data frame with two variables v and w for values and weight respectively and having only positive values.
          W should have maximum weight of knapsack.")
  }
}
