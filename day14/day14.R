# load packages
library(stringr)

# Part 1 ------------------------------------------------------------------

template <- "SHHNCOPHONHFBVNKCFFC" # template
# prepare template
template <- unlist(strsplit(template, "")) # split
template <- data.frame(first = template[1:(length(template)-1)], # first part of pair
                       second = template[2:length(template)], # second part of pair
                       freq = 1) # frequency of pair
# read in rules data and format rules
rules <- read.table("day14/rules.txt")[,-2]
names(rules) <- c("pattern", "insert")
rules$first <- substr(rules$pattern, 1, 1)
rules$second <- substr(rules$pattern, 2, 2)
rules$pattern <- NULL

create_poly <- function(poly, rules, steps = 1){
  for(i in 1:steps){
    temp <- merge(poly, rules) # merge poly and rules
    # add insertions
    poly <- data.frame(first = c(temp$first, temp$insert),
                       second = c(temp$insert, temp$second),
                       freq = rep(temp$freq, 2)) # add insertions
    poly <- aggregate(freq ~ first + second, data = poly, FUN = "sum") # aggregate pairs (sum frequency of pairs that are more than once in poly)
  }
  return(poly)
}

count_poly <- function(poly){
  # create table of all combination of elements in poly with associated frequencies
  counts <- table(poly$first, poly$second)
  for(i in rownames(counts)){
    for(j in colnames(counts)){
      if(counts[i,j] > 0){
        counts[i,j] <- poly$freq[which(poly$first == i & poly$second == j)]
      }
    }
  }
  sums <- pmax(rowSums(counts), colSums(counts)) # determine total frequencies of each element
  value <- max(sums) - min(sums) # maximum - minimum
  return(value)
}

poly <- create_poly(template, rules, steps = 10)
count_poly(poly)


# Part2 -------------------------------------------------------------------

poly_long <- create_poly(template, rules, steps = 40)
format(count_poly(poly_long), scientific = FALSE)