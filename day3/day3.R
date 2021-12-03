
# Part 1 ------------------------------------------------------------------

report <- read.table("day3/data_day3.txt", colClasses = "character") # read in data
report_split <- matrix(data = unlist(strsplit(report$V1, "")), ncol = nchar(report[1,1]), byrow = TRUE) # create matrix with one column for each character
epsilon <- apply(report_split, MARGIN = 2, function(x) which.max(table(x))-1) # get char with maximum occurence for each column to create epsilon
gamma <- 1-epsilon # get gamma as complement to epsilon
epsilon <- asbio::bin2dec(epsilon) # convert to decimal
gamma <- asbio::bin2dec(gamma)
epsilon*gamma # multiply


# Part 2 ------------------------------------------------------------------

# prepare objects
ox <- matrix(as.numeric(report_split), ncol = ncol(report_split)) # initially same as report_split
co2 <- ox

# filter subsequently across columns for oxygen rating
for(i in 1:ncol(ox)){
  if(is.vector(ox)) break # stop if only one number is left
  if(var(ox[,i]) == 0) next # if all the numbers in a column are the same, the column is not informative and can be skipped
  freq <- table(ox[,i]) # compute frequencies
  if(freq[1] == freq[2]){
    ox <- ox[which(ox[,i] == 1),] # if frequencies are identical filter for 1
  } else {
    ox <- ox[which(ox[,i] == which.max(freq)-1),] # else filter for most common value
  }
}

# repeat for co2 rating
for(i in 1:ncol(co2)){
  if(is.vector(co2)) break
  if(var(co2[,i]) == 0) next
  freq <- table(co2[,i])
  if(freq[1] == freq[2]){
    co2 <- co2[which(co2[,i] == 0),] # if frequencies are identical filter for 0
  } else {
    co2 <- co2[which(co2[,i] == which.min(freq)-1),] # else filter for least common value
  }
}

ox <- asbio::bin2dec(ox) # convert to decimal
co2 <- asbio::bin2dec(co2)
ox*co2 # multiply
