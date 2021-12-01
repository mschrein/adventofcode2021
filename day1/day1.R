# Part 1 ------------------------------------------------------------------
report <- read.table("data_day1.txt")[,1] # read data and convert to vector
diffs <- diff(report) # compute differences
no_increasing <- sum(diffs > 0) # count positive differences (increasing depth)
paste("There are", no_increasing, "measurements larger than the previous measurement.")

# Part 2 ------------------------------------------------------------------
# function for collecting combinations according to a sliding size-measurement window
combis <- function(v, size){
  len <- length(v)
  comb <- NULL
  for(i in 1:len){
    if(i > len-size+1) break
    comb <- rbind(comb, v[i:(i+(size-1))])
  }
  return(comb)
}

windows <- combis(report, size = 3) # create combinations/windows
sums <- rowSums(windows) # compute sum per measurement window
sums_diff <- diff(sums) # compute differences
no_larger <- sum(sums_diff > 0) # count positive differences
paste("There are", no_larger, "sums larger than the previous sum.")
