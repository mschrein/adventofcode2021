
# read in data
fish <- as.numeric(read.table("day6/data_day6.txt", sep = ",")[1,])
days <- 256 # number of days, set to 80 for Part 1

fish <- factor(fish, levels = 0:8) # convert to factor with possible values of timer
count <- as.numeric(table(fish)) # create initial count --> number of fish per timer value

# iterate over days
for(i in 1:days){
  new = count[1] # number of new fish (number of fish with a timer value of 0)
  count[1:8] <- count[2:9] # shift number of fish per timer value (corresponds to subtracting 1 from timer of each fish)
  count[7] <- count[7] + new # update count for timer value 6 (by resetting timer of fish with value 0 to value 6)
  count[9] <- new # add new fish with timer 8
}

no_fish <- sum(count) # number of fish after days
sprintf("%0.f", no_fish) # switch from scientific to decimal notation
