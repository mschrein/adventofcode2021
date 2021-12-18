
target_area <- c(x1 = 150, x2 = 171, y1 = -129, y2 = -70) # define target area

# function that moves the probe and returns maximum y position and hit status
move <- function(velo, target_area){
  pos <- c(0,0) # probe position
  status <- "miss" # probe hit status
  max_y <- vector("numeric") # maximum y position
  repeat{
    pos <- pos + velo # increase probe position by velocity
    velo[1] <- velo[1] + ifelse(velo[1] < 0, 1, ifelse(velo[1] > 0, -1, 0)) # drag
    velo[2] <- velo[2] - 1 # gravity
    max_y <- max(max_y, pos[2]) # update maximum y position
    # set probe hit status
    if(pos[1] %in% target_area["x1"]:target_area["x2"] & pos[2] %in% target_area["y1"]:target_area["y2"]){
      status <- "hit"
      break
    } else if(pos[1] > max(target_area["x1"], target_area["x2"])){
      status <- "overshoot"
      break
    } else if(pos[2] < min(target_area["y1"], target_area["y2"])){
      status <- "undershoot"
      break
    }
  }
  return(list(status = status, max_y = max_y))
}

# set start values for x and y
starters_x <- max(target_area["x1"], target_area["x2"]):1
starters_y <- -max(abs(target_area)):max(abs(target_area))

# function that returns start velocity values and maximum y position for hits
find_starters <- function(starters_x, starters_y, target_area){
  hit <- matrix(ncol = 3)
  for(i in 1:length(starters_x)){
    for(j in 1:length(starters_y)){
      check_hit <- move(c(starters_x[i], starters_y[j]), target_area = target_area) # move probe
      if(check_hit$status == "overshoot") break # if probe overshoots, move to next x velocity value (improves performance)
      if(check_hit$status == "hit"){ # if probe hits, save velocity values and maximum y position
        hit <- rbind(hit, c(starters_x[i], starters_y[j], check_hit$max_y))
      }
    }
  }
  hit <- hit[-1,]
  return(hit)
}

hits <- find_starters(starters_x, starters_y, target_area) # find all hits
max(hits[,3]) # Part 1: maximum y position given a probe hit
nrow(hits) # Part 2: number of probe hits given distinct starting velocities
