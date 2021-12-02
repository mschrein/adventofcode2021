
# Part 1 ------------------------------------------------------------------
course <- read.table("day2/data_day2.txt") # read in data
names(course) <- c("direction", "distance") # rename variables

course$direction_general <- ifelse(course$direction == "forward", "horizontal", "vertical") # create general direction labels
course$distance <- ifelse(course$direction == "up", -1*course$distance, course$distance) # make "up" value snegative
pos_x <- sum(course$distance[which(course$direction_general == "horizontal")]) # sum horizontal distances
pos_y <- sum(course$distance[which(course$direction_general == "vertical")]) # sum vertical distances
pos_x*pos_y # multiply


# Part2 -------------------------------------------------------------------
# create intital objects
aim <- 0
x <- 0
y <- 0
# inititate random walk
for(i in 1:nrow(course)){
  if(course$direction_general[i] == "vertical"){
    aim <- aim + course$distance[i] # adjust aim if sub moves vertically
  } else {
    # update horizontal (x) and vertical position (y) if sub moves forward, considering aim
    x <- x + course$distance[i]
    y <- y + course$distance[i]*aim
  }
}
x*y # multiply
