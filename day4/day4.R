
# Part 1 ------------------------------------------------------------------
# read in data (numbers and board data were saved in separate files)
numbers <- as.numeric(read.table("day4/numbers.txt", sep = ",")[1,])
boards_data <- read.table("day4/boards.txt")
# assign board ID
boards_data$id <- as.factor(rep(1:(nrow(boards_data)/5), each = 5))

boards <- boards_data # copy

# play the game
for(i in 1:length(numbers)){
  selection <- numbers[i]# draw numbers
  boards[,-6][boards[,-6] == numbers[i]] <- NA # if selected number exists in boards, replace by NA
  boards_split <- split(boards, f = boards$id) # split boards by id
  row_check <- sapply(boards_split, FUN = function(x) 5 %in% rowSums(is.na(x))) # check if 5 numbers have been selected for each row of each board
  col_check <- sapply(boards_split, FUN = function(x) 5 %in% colSums(is.na(x))) # check if 5 numbers have been selected for each column of each board
  winners <- row_check | col_check # for each board check whether it won
  if(TRUE %in% winners){ # check if winner exists
    winner_id <- names(winners)[winners] # select id of winning board
    winner <- boards[boards$id == winner_id, -6] # select winning board
    break
  }
}
sum(winner, na.rm = TRUE)*selection # sum non-selected values of winning board and multiply with last selected number


# Part 2 ------------------------------------------------------------------

# reset boards
boards <- boards_data

for(i in 1:length(numbers)){
  selection <- numbers[i]# draw numbers
  boards[,-6][boards[,-6] == selection] <- NA # if selected number exists in boards, replace by NA
  boards_split <- split(boards, f = boards$id) # split boards by id
  row_check <- sapply(boards_split, FUN = function(x) 5 %in% rowSums(is.na(x))) # check if 5 numbers have been selected for each row of each board
  col_check <- sapply(boards_split, FUN = function(x) 5 %in% colSums(is.na(x))) # check if 5 numbers have been selected for each column of each board
  winners <- row_check | col_check # for each board check whether it won
  if(length(winners[!winners]) == 1){ # check if only one board is left --> has to be last winner
    last_id <- names(winners[!winners]) # select id of last winning board
    break
  }
}

# reset boards and select last winning board based on previously determined last_id
boards <- boards_data
last <- boards[boards$id == last_id, -6]

# play the game from the beginning for last board and determine selection with which last board wins
for(i in 1:length(numbers)){
  selection <- numbers[i]
  last[last == selection] <- NA
  row_check <- rowSums(is.na(last))
  col_check <- colSums(is.na(last))
  if(5 %in% row_check | 5 %in% col_check) break
}
sum(last, na.rm = TRUE)*selection # sum non-selected values of last winning board and multiply with last selected number
