# load packages
library(dplyr)

# Part 1 ------------------------------------------------------------------

# read in data
vents_data <- read.table("day5/day5_data.txt")
# modify data
vents_data <- vents_data[,-2] # remove second column
vents <- matrix(nrow = nrow(vents_data), ncol = 4) # specify vents matrix
vents[,1:2] <- matrix(unlist(strsplit(vents_data[,1], ",")), ncol = 2, byrow = TRUE) # split values for first point
vents[,3:4] <- matrix(unlist(strsplit(vents_data[,2], ",")), ncol = 2, byrow = TRUE) # split values for second point
vents <- apply(vents, MARGIN = 2, as.numeric) # make values numeric
vents <- data.frame(vents) # change to data frame
names(vents) <- c("x1", "y1", "x2", "y2") # set variable names

# determine direction
vents <- vents %>%
  mutate(direction = case_when(
    x1 == x2 ~ "vertical",
    y1 == y2 ~ "horizontal",
    x1 != x2 & y1 != y2 ~ "diagonal"
  ))

# filter vents only keeping vertical and horizontal directions
vents_ez <- filter(vents, direction %in% c("vertical", "horizontal"))

# prepare diagram
diagram <- matrix(data = 0, nrow = 1000, ncol = 1000)

# update diagram
for(i in 1:nrow(vents_ez)){
  if(vents_ez$direction[i] == "horizontal"){
    diagram[vents_ez$y1[i], vents_ez$x1[i]:vents_ez$x2[i]] <- diagram[vents_ez$y1[i], vents_ez$x1[i]:vents_ez$x2[i]] + 1
  } else{
    diagram[vents_ez$y1[i]:vents_ez$y2[i], vents_ez$x1[i]] <- diagram[vents_ez$y1[i]:vents_ez$y2[i], vents_ez$x1[i]] + 1
  }
}
length(diagram[diagram >= 2]) # count number of ares that are covered by at least 2 vents

# Part 2 ------------------------------------------------------------------

# reset diagram
diagram <- matrix(data = 0, nrow = 1000, ncol = 1000)

# update diagram
for(i in 1:nrow(vents)){
  if(vents$direction[i] == "horizontal"){
    diagram[vents$y1[i], vents$x1[i]:vents$x2[i]] <- diagram[vents$y1[i], vents$x1[i]:vents$x2[i]] + 1
  } else if(vents$direction[i] == "vertical"){
    diagram[vents$y1[i]:vents$y2[i], vents$x1[i]] <- diagram[vents$y1[i]:vents$y2[i], vents$x1[i]] + 1
  } else{ # for diagonal vents
    diag(diagram[vents$y1[i]:vents$y2[i], vents$x1[i]:vents$x2[i]]) <- diag(diagram[vents$y1[i]:vents$y2[i], vents$x1[i]:vents$x2[i]]) + 1
  }
}
length(diagram[diagram >= 2]) # count number of ares that are covered by at least 2 vents

