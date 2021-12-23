# load packages
library(stringr)
library(dplyr)
library(tidyr)

instructions <- read.table("day22/data_day22.txt")
instructions[,2:4] <- t(apply(instructions, 1, function(x) unlist(strsplit(x[2], ","))))
instructions[,2:10] <- t(apply(instructions[,-1], 1, function(x) unlist(strsplit(x, "[..]"))))
instructions <- instructions[,-c(3,6,9)]
instructions[,2:7] <- t(apply(instructions[,-1], 1, function(x) str_replace_all(x, c("x=" = "", "y=" = "", "z=" = ""))))
instructions[,2:7] <- apply(instructions[,-1], 2, as.numeric)
names(instructions) <- c("state", "x1", "x2", "y1", "y2", "z1", "z2")

# for Part 1: only consider regions x=-50..50,y=-50..50,z=-50..50
instructions <- instructions[-which(instructions$x1 < -50 | instructions$x2 > 50 | instructions$y1 < -50 | instructions$y2 > 50 | instructions$z1 < -50 | instructions$z2 > 50),]

on <- data.frame(matrix(ncol = 3))[-1,]
names(on) <- c("x", "y", "z")

for(i in 1:nrow(instructions)){
  grid <- crossing(x = instructions$x1[i]:instructions$x2[i], y = instructions$y1[i]:instructions$y2[i], z = instructions$z1[i]:instructions$z2[i])
  if(instructions$state[i] == "on"){
    on <- rbind(on, anti_join(grid, on, by = c("x", "y", "z")))
  } else{
    on <- anti_join(on, grid, by = c("x", "y", "z"))
  }
}
nrow(on)