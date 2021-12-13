
# Part 1 ------------------------------------------------------------------

# read in data
dots <- read.table("day13/dots.txt", sep = ",")
instructions <- read.table("day13/instructions.txt")[3]
instructions <- data.frame(t(apply(instructions, 1, function(x) unlist(strsplit(x, "=")))))
instructions[,2] <- as.numeric(instructions[,2])

# create dot map
map <- matrix(data = 0, nrow = max(dots[,1]) + 1, ncol = max(dots[,2]) + 1)

# add 1 to all dots and instruction values so top left corner is 1,1
dots <- dots + 1
instructions[,2] <- instructions[,2] + 1

# add dots to map
for(i in 1:nrow(dots)){
  map[dots[i,1], dots[i,2]] <- 1
}

# folding function
fold <- function(map, instructions, nfold){
  for(i in 1:nfold){
    if(instructions[i,1] == "x"){
      map_part1 <- map[1:(instructions[i,2]-1),]
      map_part2 <- map[(instructions[i,2]+1):nrow(map),]
      # add padding in case of differing dimensions
      rowdiff <- nrow(map_part1) - nrow(map_part2)
      if(rowdiff < 0){
        for(j in 1:abs(rowdiff)){
          map_part1 <- rbind(0, map_part1)
        }
      } else if(rowdiff > 0){
        for(j in 1:abs(rowdiff)){
          map_part2 <- rbind(map_part2, 0)
        }
      }
      map_part2 <- map_part2[nrow(map_part2):1,] # reverse row order of second part
      map <- map_part1 + map_part2 # create map_fold by summing parts
    } else{
      map_part1 <- map[,1:(instructions[i,2]-1)]
      map_part2 <- map[,(instructions[i,2]+1):ncol(map)]
      # add padding in case of differing dimension
      coldiff <- ncol(map_part1) - ncol(map_part2)
      if(coldiff < 0){
        for(j in 1:abs(coldiff)){
          map_part1 <- cbind(0, map_part1)
        }
      } else if(coldiff > 0){
        for(j in 1:abs(coldiff)){
          map_part2 <- cbind(map_part2, 0)
        }
      }
      map_part2 <- map_part2[,ncol(map_part2):1] # reverse column order of second part
      map <- map_part1 + map_part2 # create map_fold by summing parts
    }
  }
  return(map)
}

sum(fold(map, instructions, nfold = 1) > 0) # number of visible dots after 1 fold

# Part 2 ------------------------------------------------------------------

max_fold <- fold(map, instructions, nfold = nrow(instructions))
max_fold <- t(max_fold) # transpose
# improve readability
mode(max_fold) <- "character"
max_fold[which(max_fold == "0")] <- " "
View(max_fold) # View result and read letters
