
# Part 1 ------------------------------------------------------------------

# read in data
caves <- read.table("day12/data_day12.txt", sep = "-")

count_paths <- function(caves, path = "start"){
  pathcount <- 0 # create path counter
  from <- path[length(path)] # from cave
  selection <- caves[which(caves[,1] == from | caves[,2] == from),] # select relevant rows
  for(i in 1:nrow(selection)){ # iterate over rows
    to <- ifelse(selection[i,1] == from, selection[i,2], selection[i,1]) # to cave
    if(!(to %in% path & to == tolower(to))){ # check if to cave is not a lowercase cave that has been visited previously
      if(to == "end"){ # if end is reached
        pathcount <- pathcount + 1 # update path counter
      } else{
        update_path <- c(path, to) # update path with to (serves as new from in next function call)
        pathcount <- pathcount + count_paths(caves, path = update_path) # call function recursively using updated paths, add results to path counter
      }
    }
  }
  return(pathcount)
}

count_paths(caves) # run function and print path counter


# Part 2 ------------------------------------------------------------------

count_paths2 <- function(caves, path = "start"){
  criterion <- table(path[which(path == tolower(path))]) > 1 # criterion: more than 1 lowercase cave
  if(any(criterion)){
    return(count_paths(caves, path = path)) # if criterion applies, apply function from part 1 (i.e., lowercase caves cannot be visited twice anymore)
  }
  pathcount <- 0 # create path counter
  from <- path[length(path)] # from cave
  selection <- caves[which(caves[,1] == from | caves[,2] == from),] # select relevant rows
  for(i in 1:nrow(selection)){ # iterate over rows
    to <- ifelse(selection[i,1] == from, selection[i,2], selection[i,1]) # to cave
    if(to != "start"){ # not possible to go back to start
      if(to == "end"){ # if end is reached
        pathcount <- pathcount + 1 # update path counter
      } else{
        update_path <- c(path, to) # update path with to (serves as new from in next function call)
        pathcount <- pathcount + count_paths2(caves, path = update_path) # call function recursively using updated paths, add results to path counter
      }
    }
  }
  return(pathcount)
}

count_paths2(caves) # run function and print path counter
