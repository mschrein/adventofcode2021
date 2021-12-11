
# Part 1 ------------------------------------------------------------------

# read in data
energy_data <- read.table("day11/data_day11.txt", colClasses = "character")
energy_data <- t(apply(energy_data, 1, function(x) as.numeric(unlist(strsplit(x, "")))))

# add padding
energy_data <- rbind(-Inf, energy_data, -Inf)
energy_data <- cbind(-Inf, energy_data, -Inf)
energy <- energy_data

# function that finds adjacent elements
adjacent <- function(m, r, c){
  top <- c(r-1, c)
  topright <- c(r-1, c+1)
  right <- c(r, c+1)
  bottomright <- c(r+1, c+1)
  bottom <- c(r+1, c)
  bottomleft <- c(r+1, c-1)
  left <- c(r, c-1)
  topleft <- c(r-1, c-1)
  adj <- rbind(top, topright, right, bottomright, bottom, bottomleft, left, topleft)
  return(adj)
}

steps <- 100 # number of steps
flashcount <- 0 # initialize flash count
for(i in 1:steps){ # for each step
  energy <- energy + 1 # increase energy level of each octopus by 1
  if(!any(energy > 9)) next # if no octopus has energy level > 9 continue to next step
  while(any(energy[which(!is.na(energy))] > 9)){ # while there are octopuses with an energy level greater 9
    for(j in 2:(nrow(energy)-1)){ # iterate through matrix
      for(k in 2:(ncol(energy)-1)){
        if(energy[j,k] > 9 & !is.na(energy[j, k])){ # if given octopus has value greater 9
          flashcount <- flashcount + 1 # add 1 to flashcount
          energy[j,k] <- NA # set octopus NA to take him out of current step
          energy[adjacent(energy, j, k)] <- energy[adjacent(energy, j, k)] + 1 # increase energy level of all adjacent octopuses by 1
        }
      }
    }
  }
  energy[which(is.na(energy))] <- 0 # set energy level of each octopus that flashed to zero
}
flashcount # print flashcount


# Part 2 ------------------------------------------------------------------
energy <- energy_data # reset energy
stepcount <- 0 # set step counter

while(TRUE){ # run indefinite number of steps
  stepcount <- stepcount + 1 # increase step counter by 1
  energy <- energy + 1 # increase energy level of each octopus by 1
  if(!any(energy > 9)) next # if no octopus has energy level > 9 continue to next step
  while(any(energy[which(!is.na(energy))] > 9)){ # while there are octopuses with an energy level greater 9
    for(j in 2:(nrow(energy)-1)){ # iterate through matrix
      for(k in 2:(ncol(energy)-1)){
        if(energy[j,k] > 9 & !is.na(energy[j, k])){ # if given octopus has value greater 9
          energy[j,k] <- NA # set octopus NA to take him out of current step
          energy[adjacent(energy, j, k)] <- energy[adjacent(energy, j, k)] + 1 # increase energy level of all adjacent octopuses by 1
        }
      }
    }
  }
  if(all(is.na(energy) | is.infinite(energy))) break # stop if all octopuses flashed during the same step
  energy[which(is.na(energy))] <- 0 # set energy level of each octopus that flashed to zero
}
stepcount # print stepcount