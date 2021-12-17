
# load package
library(compositions)

# read in mapping
mapping <- read.table("day16/mapping.txt", sep = "=", colClasses = "character", strip.white = TRUE)

# read in data
transmission <- read.table("day16/data_day16.txt")[[1]]
transmission <- unlist(strsplit(transmission, ""))

# convert transmission to binary
binary <- sapply(transmission, function(x) mapping[which(mapping[,1] == x), 2])
binary <- paste(binary, collapse = "")
binary <- unlist(strsplit(binary, ""))
binary <- as.numeric(binary)

# function that converts binary to decimal
tonum <- function(bit){
  require(compositions)
  num <- paste(bit, collapse = "")
  num <- unbinary(num)
  return(num)
}

# function that performs operation based on type (for Part 2)
operator <- function(values, type){
  if(type == 0){
    val <- sum(values)
  } else if(type == 1){
    val <- prod(values)
  } else if(type == 2){
    val <- min(values)
  } else if(type == 3){
    val <- max(values)
  } else if(type == 5){
    val <- ifelse(values[1] > values[2], 1, 0)
  } else if(type == 6){
    val <- ifelse(values[1] < values[2], 1, 0)
  } else if(type == 7){
    val <- ifelse(values[1] == values[2], 1, 0)
  }
  return(val)
}

decode_transmission <- function(binary, remaining = Inf) {
  res <- list(version_sum = 0, expr = vector(), length = length(binary)) # set up storage
  while (sum(binary) > 0 & remaining > 0){
    remaining <- remaining - 1
    version <- tonum(binary[1:3])
    res$version_sum <- res$version_sum + version # update version sum
    type <- tonum(binary[4:6])
    binary <- tail(binary, -6)
    if(type == 4){ # literal value package
      ngroup <- which.min(binary[seq(1, length(binary), 5)]) # determine number of value groups
      value <- binary[1:(5*ngroup)]
      value <- value[-seq(1, length(value), 5)] # remove group identifier
      value <- tonum(value)
      res$expr <- c(res$expr, value) # update expression storage
      binary <- tail(binary, -5*ngroup)
    } else{ # operator package
      id <- binary[1] # length id
      binary <- tail(binary, -1)
      if(id == 0){
        len <- tonum(binary[1:15]) # determine length of sub package
        binary <- tail(binary, -15)
        sub_package  <- binary[1:len] # select sub package
        sub_res <- decode_transmission(sub_package) # decode subpackage
        res$version_sum <- res$version_sum + sub_res$version_sum # update version sum
        expr <- operator(sub_res$expr, type = type) # determine subpackage expression
        res$expr <- c(res$expr, expr) # update expression storage
        binary <- tail(binary, -len)
      } else{
        nsub <- tonum(binary[1:11]) # determine number of subpackages
        binary <- tail(binary, -11)
        sub_res <- decode_transmission(binary, remaining = nsub) # decode subpackages
        res$version_sum <- res$version_sum + sub_res$version_sum # update version sum
        expr <- operator(sub_res$expr, type = type) # determine subpackage(s) expression
        res$expr <- c(res$expr, expr) # update expression storage
        binary <- tail(binary, sub_res$length)
      }
    }
  }
  res$length <- length(binary) # update length storage
  return(res)
}

message <- decode_transmission(binary) # run function on binary
message$version_sum # Part 1
message$expr # Part 2

