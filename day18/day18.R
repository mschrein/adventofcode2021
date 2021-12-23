
# load packages
library(stringr)


# Part 1 ------------------------------------------------------------------

# read in data
num <- read.table("day18/data_day18.txt")[[1]]
num <- lapply(num, function(x){
  x <- str_replace_all(x, "\\[", "list(")
  x <- str_replace_all(x, "\\]", ")")
  x <- eval(str2expression(x))
})

# determine depth of values in a list
depth <- function(l, depth = 0){
  if(!(is.list(l))){
    return(depth)
  } else {
    unlist(sapply(l, depth, depth = depth + 1))
  }
}

# explode function
explode <- function(l){
  dp <- depth(l)
  ind <- which(dp > 4)[1:2]
  ul <- unlist(l)
  if(ind[1] > 1) ul[ind[1]-1] <- ul[ind[1]-1] + ul[ind[1]]
  if(ind[2] < length(ul)) ul[ind[2]+1] <- ul[ind[2]+1] + ul[ind[2]]
  ul[ind] <- NA
  l <- relist(ul, l)
  l <- explode_helper(l)
  return(l)
}

explode_helper <- function(l){
  if(!is.list(l)){
    return(l)
  } else if(all(is.na(l))){
    return(0)
  }
  lapply(l, explode_helper)
}

# split function
split_num <- function(l) {
  stopper <- FALSE
  rapply(l, function(x) {
    if (x <= 9 | stopper) return(x)
    stopper <<- TRUE
    list(floor(x / 2), ceiling(x / 2))
  }, how = 'replace')
}

# reduce function
redu <- function(l){
  res <- l[[1]]
  for(i in 2:length(l)){
    res <- list(res, l[[i]])
    while(any(depth(res) > 4) | any(unlist(res) > 9)){
      if(any(depth(res) > 4)){
        res <- explode(res)
        next
      }
      if(any(unlist(res) > 9) & !any(depth(res) > 4)){
        res <- split_num(res)
      }
    }
  }
  return(res)
}

# magnitude function
magnitude <- function(l){
  Reduce(function(left, right) 3 * magnitude(left) + 2 * magnitude(right), l)
}

res <- redu(num) # get result
magnitude(res) # determine magnitude


# Part 2 ------------------------------------------------------------------
num_cross <- cbind(combn(1:length(num),2), combn(length(num):1,2)) # create possible number combinations

# determine maximum magnitude
max_magnitude <- 0
for(i in 1:ncol(num_cross)){
  l <- list(num[[num_cross[1,i]]], num[[num_cross[2,i]]])
  re <- redu(l)
  m <- magnitude(re)
  max_magnitude <- max(max_magnitude, m)
}
max_magnitude
