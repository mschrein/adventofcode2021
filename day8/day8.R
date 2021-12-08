
# Part 1 ------------------------------------------------------------------
# read in data
data <- read.table("day8/data_day8.txt")
output <- data[,12:15] # select output patterns (after |)
no_segments <- as.vector(apply(output, MARGIN = 2, FUN = nchar)) # compute length of each segment
sum(no_segments %in% c(2:4, 7)) # count occurrence of unique numbers of segments


# Part 2 ------------------------------------------------------------------

input <- data[,1:10]

# function that splits character into vector
strSplit <- function(string){
  return(unlist(strsplit(string, "")))
}
# function that sorts letters in a string alphabetically
strSort <- function(string){
  return(paste(sort(strSplit(string)), collapse = ""))
}
# function that checks whether strings contain (all) specified letters
strContains <- function(strings, letters, reverse = FALSE){
  strings_split <- strsplit(strings, "")
  letters_split <- strSplit(letters)
  return(sapply(strings_split, function(x) all(letters_split %in% x)))
}

# sort each element in input and output alphabetically
input <- apply(input, c(1,2), strSort)
output <- apply(output, c(1,2), strSort)

segments_length <- c(6, 2, 5, 5, 4, 5, 6, 3, 7, 6) # number of characters for each segment
names(segments_length) <- as.character(0:9) # assign numbers as names
unique_numbers <- as.character(c(1,4,7,8)) # specify unique numbers

output_numbers <- vector("numeric", nrow(data))

for(i in 1:nrow(data)){
  inp <- input[i,] # inoput row
  out <- output[i,] # output row
  numbers <- setNames(vector("character", 10), 0:9) # set up vector for matching strings and numbers
  numbers[unique_numbers] <- inp[match(segments_length[unique_numbers], nchar(inp))] # match strings for unqiue numbers
  numbers["6"] <- inp[nchar(inp) == 6 & !strContains(inp, numbers["1"])] # 6 consists of six letters and does not contain all the letters from 1
  numbers["3"] <- inp[nchar(inp) == 5 & strContains(inp, numbers["1"])] # 3 consists of five letters and contains the letters from 1
  inp <- setdiff(inp, numbers) # filter out numbers that have been figured out
  numbers["0"] <- inp[nchar(inp) == 6 & !strContains(inp, numbers["4"])] # 0 consists of six letters and does not contain all the letters from 4
  inp <- setdiff(inp, numbers)
  numbers["9"] <- inp[nchar(inp) == 6] # input is last remaining segment of length 6
  inp <- setdiff(inp, numbers)
  numbers["5"] <- inp[sapply(strsplit(inp, ""), function(x) all(x %in% strSplit(numbers["6"])))] # all letters of five are contained in 6
  numbers["2"] <- setdiff(inp, numbers) # remaining number is 2
  out_num <- as.numeric(paste(match(out, numbers)-1, collapse = "")) # create output number
  output_numbers[i] <- out_num # add to output_numbers
}
sum(output_numbers) # sum output numbers
