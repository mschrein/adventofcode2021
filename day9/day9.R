
# Part 1 ------------------------------------------------------------------

# read in data
heights <- read.table("day9/data_day9.txt", colClasses = "character")
heights <- apply(heights, 1, function(x) unlist(strsplit(x, "")))
heights <- matrix(as.numeric(heights), nrow = nrow(heights), byrow=FALSE)

# pad heights with large number
heights_padded <- rbind(Inf, heights, Inf)
heights_padded <- cbind(Inf, heights_padded, Inf)

# prepare comparison matrix
low <- matrix(data = FALSE, nrow = nrow(heights_padded), ncol = ncol(heights_padded))

# find low points
for(i in 2:(nrow(heights_padded)-1)){
  for(j in 2:(ncol(heights_padded)-1)){
    if(heights_padded[i,j] < heights_padded[i,j-1] & 
       heights_padded[i,j] < heights_padded[i,j+1] & 
       heights_padded[i,j] < heights_padded[i-1,j] & 
       heights_padded[i,j] < heights_padded[i+1,j]){
      low[i,j] <- TRUE
    }
  }
}

low <- low[-c(1, nrow(low)), -c(1, ncol(low))] # remove padding from low
risk_val <- heights[low] + 1 # match low with heights and add 1 to compute risk value
sum(risk_val) # sum risk values

# Part 2 ------------------------------------------------------------------

# basin map: set all values except 9 to NA and set 9 to Inf
basins <- heights
basins <- ifelse(basins < 9, NA, Inf)
basins[low] <- 1:sum(low) # number low points
# add padding
basins <- rbind(Inf, basins, Inf)
basins <- cbind(Inf, basins, Inf)

# iterate over basins until all values are filled or Inf
while(anyNA(basins)){
  for(i in 2:(nrow(basins)-1)){
    for (j in 2:(ncol(basins)-1)){
      if(is.na(basins[i,j])){
        # select top, bottom, left, right values
        pos <- rbind(c(i-1,j), c(i+1,j), c(i,j-1), c(i, j+1))
        n <- basins[pos] 
        basins[i,j] <- n[is.finite(n)][1] # fill in value (except Inf)
      }
    }
  }
}

basins <- basins[-c(1, nrow(basins)), -c(1, ncol(basins))] # remove padding
basin_sizes <- table(basins[is.finite(basins)]) # determine basin sizes
largest_sizes <- sort(basin_sizes, decreasing = TRUE)[1:3] # select three largest basins
prod(largest_sizes) # multiply sizes
