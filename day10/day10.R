
# Part 1 ------------------------------------------------------------------

# read in data
lines <- read.table("day10/data_day10.txt")[,1]

opening <- c("(", "[", "{", "<") # opening characters
closing <- c(")", "]", "}", ">") # closing characters
score <- c(3, 57, 1197, 25137) # scores
corrupt_score <- 0 # specify corruption score, starting with zero

for(i in 1:length(lines)){ # for each line
  store <- vector("character", length = 0) # storage of opening characters
  str <- unlist(strsplit(lines[i], "")) # split string
  for(j in 1:length(str)){ # for each position in string
    if(str[j] %in% opening){
      store <- c(store, str[j]) # add opening character to store
    } else{
      if(which(closing == str[j]) == which(opening == store[length(store)])){
        store <- store[-length(store)] # if closing character matches last opening character, remove last character from store
      } else { # otherwise it is a corruption character
        corrupt_score <- corrupt_score + score[which(closing == str[j])] # add corresponding score to corrupt_score
        break
      }
    }
  }
}
corrupt_score # print corrupt_score

# Part 2 ------------------------------------------------------------------
score <- 1:4 # scores
completion_score <- vector("numeric", 0) # prepare completion_score object

for(i in 1:length(lines)){ # for each line
  store <- vector("character", length = 0) # storage of opening characters
  str <- unlist(strsplit(lines[i], "")) # split string
  corrupt <- FALSE # check if line is corrupt
  for(j in 1:length(str)){ # for each position in string
    if(str[j] %in% opening){
      store <- c(store, str[j]) # add opening character to store
    } else{
      if(which(closing == str[j]) == which(opening == store[length(store)])){
        store <- store[-length(store)] # if closing character matches last opening character, remove last character from store
      } else { # otherwise it is a corrupted line
        corrupt <- TRUE # mark line as corrupt
        break
      }
    }
  }
  if(length(store) > 0 & !corrupt){ # if incomplete (and not corrupt) line
    complete <- rev(closing[match(store, opening)]) # determine completion characters (reverse order to store)
    comp_score <- 0 # completion score starts with zero
    for(k in 1:length(complete)){ # for each completion character
      comp_score <- comp_score*5 # multiply score by 5
      comp_score <- comp_score + score[which(closing == complete[k])] # add score corresponding to completion character
    }
    completion_score <- c(completion_score, comp_score) # add comp_score to completion_score
  }
}
median(completion_score) # get middle completion score

                