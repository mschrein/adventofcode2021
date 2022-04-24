
# read in data
map <- read.table("day25/data_day25.txt")
map <- t(apply(map, 1, function(x) unlist(strsplit(x, ""))))
# map[map == "."] <- 0
# map[map == ">"] <- 1
# map[map == "v"] <- -1
# mode(map) <- "numeric"

#move_check <- function(v, herd){
  pos <- which(v == herd)
  able <- FALSE
  if(length(pos > 0)){
    able <- v[pos+1] == 0
    if(pos[length(pos)] == length(v)){
      able[length(able)] <- ifelse(v[1] == 0, TRUE, FALSE)
    }
  }
  return(list(pos = pos, able = able))
}

move <- function(map){
  # move to the right
  map_next <- map
  left  <- cbind(map[,ncol(map)], map[, -ncol(map)]) # put rightmost cucumbers to the left
  right  <- cbind(map[,-1], map[,1]) # put leftmost cucumbers to the right
  map_next[map == ">" & right == "."] <- "." # cucumber leaves location
  map_next[map == "." & left == ">"] <- ">" # cucumber enters location

  # move to the bottom
  map <- map_next
  top <- rbind(map[nrow(map), ], map[-nrow(map), ]) # put bottommost cucumbers to the top
  bottom <- rbind(map[-1, ], map[1, ]) # put topmost cucumbers to the bottom
  map_next[map == "v" & bottom == "."] <- "." # cucumber leaves location
  map_next[map == "." & top == "v"] <- "v" # cucumber enters location
  invisible(map_next)
}

no_move_step <- function(map){
  steps <- 0 # step count
  prev <- map # map from previous step
  repeat{
    steps <- steps + 1 # update step count
    curr <- move(prev) # map from current step
    if(identical(prev, curr)){
      break # break if map from previous and current step are identical (no cucumber moved)
    } else{
      prev <- curr
    }
  }
  return(steps)
}

no_move_step(map)
