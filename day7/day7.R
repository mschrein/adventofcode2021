
# Part 1 ------------------------------------------------------------------

# read in data
positions <- as.numeric(read.table("day7/data_day7.txt", sep = ",")[1,])
sum(abs(positions - median(positions))) # calculate sum of absolute differences from the median


# Part 2 ------------------------------------------------------------------
fuel <- Inf # intialize fuel object
# iterate over positions and calculate required fuel as the sum of the series (n = absolute difference between current position and test position), keep minimum fuel
for(i in min(positions):max(positions)){
  fuel <- min(sum(((abs(positions-i))*(abs(positions-i)+1))/2), fuel)
}
fuel
