
# read in data
instr <- read.table("day24/data_day24.txt", fill = TRUE, col.names = c("instr", "a", "b"))
# add input identifiers
instr$id <- NA
id <- 0
for(i in 1:nrow(instr)){
  if(instr$instr[i] == "inp") id <- id + 1
  instr$id[i] <- id
}
instr$id <- factor(instr$id)

find_num <- function(instr, max = TRUE){
  instr <- split(instr, instr$id)
  nums <- rep(0, times = 14)
  store <- vector()
  for(i in 1:length(instr)){
    check <- instr[[i]]$b[5] != 26 # checker
    x <- as.numeric(instr[[i]]$b[6]) # add b to x
    y <- as.numeric(instr[[i]]$b[16]) # add b to y
    if(check){
      store <- c(list(c(y = y, id = i)), store) # store y value and id
    } else{
      y <- store[[1]]["y"] # reset y
      id <- store[[1]]["id"] # reset id
      store <- store[-1] # remove from store
      w <- 1:9 + x + y
      w <- (1:9)[w < 10 & w > 0]
      w <- ifelse(max,  max(w), min(w)) # determine max (Part 1) or min (Part 2) w
      nums[id] <- w # update nums
      nums[i] <- w + x + y # update nums
    }
  }
  nums <- paste(nums, collapse = "") # paste nums
  return(nums)
}

find_num(instr) # Part 1
find_num(instr, max = FALSE) # Part 2
