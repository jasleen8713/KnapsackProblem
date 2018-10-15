#' Function to solve knapsack problem using brute force search
#' @param x as data.frame of weights and profits with
#' @param W as maximum kapsack weight
#' @param parallel as boolean value
#' @return a list
#' @importFrom utils combn
#' @importFrom foreach "%dopar%" "%do%"
#' @export
brute_force_knapsack <- function(x,W,parallel=FALSE){

  #check if inputs are correct
  if(is.data.frame(x) && "w" %in% colnames(x) && "v" %in% colnames(x)
     && all(x$w > 0) && all(x$v > 0) && is.numeric(W) && W>0)
  {
    totalitems <- nrow(x)
    choices <- numeric(totalitems) #vector of 0s
    value <- 0
    elements <- c()

    if(!parallel)
    {
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
            tempWeight <- tempWeight + x[k,]$w
            tempValue <- tempValue + x[k,]$v
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
    else #parallelization
    {

      #detecting and using cores accordingly
      cores= parallel::detectCores()
      cl <- parallel::makeCluster(cores[1]-1) #avoid overloading
      doParallel::registerDoParallel(cl)

      sets <- foreach::foreach(i=1:totalitems) %dopar% {
        combn(totalitems, i, simplify = F)
      }
      finalsets <- unlist(sets, recursive = F)

      output <- foreach::foreach(k=1:length(finalsets)) %do% {
        tempWeight <- 0
        tempValue <- 0
        index <- c()
        foreach::foreach(j=1:length(finalsets[[k]])) %do% {
          item<-finalsets[[k]][[j]]
          tempWeight <- tempWeight + x[item,]$w
          tempValue <- tempValue + x[item,]$v
          index[length(index)+1] <- item
        }
        if (tempValue > value && tempWeight <= W) {
          value <- tempValue
          elements <- index #capture the final indexes being considered
        }
      }

      #stop cluster
      parallel::stopCluster(cl)
      return(list(value=value,elements=elements))
    }
  }
  else
  {
    stop("Please check the input. It should have two variable x and W.
          x should be a data frame with two variables v and w for values and weight respectively and having only positive values.
          W should have maximum weight of knapsack.")
  }
}
