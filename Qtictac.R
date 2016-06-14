#Q learning tic tac toe

#we will represent games as a vector of 9 element(easier than matrix representation)
#The first three numbers correspond to the first row. 0 represents no play, 1 represents
#X (first player) and -1 represents Y. #using those numbers helps check for wins easier

#We need to set up tictactoe game
generateGame <- function()
{
  game <- c(0,0,0,0,0,0,0,0,0)
  assign("board",game,envir = .GlobalEnv)
}

#check for win, return either a win or a loss or a true (win for X)
score <- function(board)
{
  possibles <- numeric()
  possibles[1] <- sum(board[1:3]) #top row
  possibles[2] <- sum(board[4:6]) #mid row
  possibles[3] <- sum(board[7:9]) #bottom row
  possibles[4] <- board[1] + board[4] + board[7] #first col
  possibles[5] <- board[2] + board[5] + board[8] #second col
  possibles[6] <- board[3] + board[6] + board[9] #third col
  possibles[7] <- board[1] + board[5] + board[9] #true diag
  possibles[8] <- board[3] + board[5] + board[7] #back diag
  if(any(possibles == 3))
  {
    return(100)
  }
  else if(any(possibles == -3))
  {
    return(-100)
  }
  else
  {
    return(0)
  }
}

#machinery used to find new moves
showPossibleMoves <- function(board)
{
  out <- list()
  cnt <- 0
  for(i in 1:9)
  {
    if(board[i]==0)
    {
      board[i] <- 1
      cnt <- cnt + 1
      out[[cnt]] <- board
      board[i] <- 0
    }
  }
  return(out)
}

Qlib <- data.frame(q11=0,q12=0,q13=0,q21=0,q22=0,q23=0,q31=0,q32=0,q33=0,r=0)


