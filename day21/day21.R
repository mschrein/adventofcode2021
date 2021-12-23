
# Part 1 ------------------------------------------------------------------

pos <- c(player1 = 10, player2 = 6) # starting positions
scores <- c(player1 = 0, player2 = 0) # starting scores
dice <- 1 # start value of dice
throwcount <- 0

# function for rolling the deterministic dice three times
roll_deterministic <- function(dice){
  roll <- dice:(dice+2)
  if(any(roll > 100)){
    roll[which(roll > 100)] <- 1:length(roll[which(roll > 100)])
  }
  return(roll)
}

repeat{
  rolls1 <- roll_deterministic(dice) # roll dice
  dice <- rolls1[3] + 1 # update dice value
  throwcount <- throwcount + 3 # update throw count
  pos["player1"] <- (pos["player1"] + sum(rolls1) - 1) %% 10 + 1# update position
  scores["player1"] <- scores["player1"] + pos["player1"] # update score
  if(scores["player1"] >= 1000) break
  rolls2 <- roll_deterministic(dice)
  dice <- rolls2[3] + 1
  throwcount <- throwcount + 3
  pos["player2"] <- (pos["player2"] + sum(rolls2) - 1) %% 10 + 1
  scores["player2"] <- scores["player2"] + pos["player2"]
  if(scores["player2"] >= 1000) break
}

min(scores)*throwcount # score of losing player times throw count


# Part 2 ------------------------------------------------------------------

dice_throws <- expand.grid(1:3, 1:3, 1:3) # unique sums of dice throws given three-sided dice
dice_throws$sum <- rowSums(dice_throws)
dice_throws <- table(dice_throws$sum)
dice_throws <- data.frame(sum = as.numeric(rownames(dice_throws)), freq = as.vector(dice_throws)) # unique dice sums with associated frequency
dice_throws <- split(dice_throws, dice_throws$sum)

pos <- c(player1 = 10, player2 = 6) # starting positions

dirac_game <- function(pos1, pos2, score1 = 0, score2 = 0){
  Reduce(function(w, r){
    pos1 <- (pos1 + r$sum - 1) %% 10 + 1 # update position
    score1 <- score1 + pos1 # update score
    if(score1 >= 21){
      w <- w + c(r$freq, 0) # update wins (corresponding to frequency of winning sum)
      return(w)
    }else{
      w <- w + r$freq * dirac_game(pos2, pos1, score2, score1)[c(2,1)] # call for player 2
      return(w)
    }
    pos2 <- (pos2 + r$sum - 1) %% 10 + 1
    score2 <- score2 + pos2
    if(score2 >= 21){
      w <- w + c(0, r$freq)
      return(w)
    }else{
      w <- w + r$freq * dirac_game(pos1, pos2, score1, score2) # call for player 1
      return(w)
    }
  },
  dice_throws,
  init = c(0, 0))
}

wins <- dirac_game(pos["player1"], pos["player2"])
format(max(wins), scientific = FALSE)
