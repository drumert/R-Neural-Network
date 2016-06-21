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

#check for win, return either a win or a loss (win for X = TRUE) #this is the Reward function
reward <- function(board)
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

opPossibleMoves <- function(board)
{
  out <- list()
  cnt <- 0
  for(i in 1:9)
  {
    if(board[i]==0)
    {
      board[i] <- -1
      cnt <- cnt + 1
      out[[cnt]] <- board
      board[i] <- 0
    }
  }
  return(out)
}
Qindex <- function(board,Qlib)
{
  asis <- 1:9
  rot180 <- 9:1
  rot90 <- c(7,4,1,8,5,2,9,6,3)
  rot270 <- c(3,6,9,2,5,8,1,4,7)
  horFlip <- c(7,8,9,4,5,6,1,2,3)
  vertFlip <- c(3,2,1,6,5,4,9,8,7)
  diagFlip <- c(1,4,7,2,5,8,3,6,9)
  crossdiagFlip <- c(9,6,3,8,5,2,7,4,1)
  I <- 0
  for(i in 1:length(Qlib))
  {
    if(Qlib[[i]]$brd == board[asis]){return(i)} #as is
    else if(Qlib[[i]]$brd == board[rot180]){return(i)} #180 degree rotate
    else if(Qlib[[i]]$brd == board[rot90]){return(i)} #90 degree rotate right
    else if(Qlib[[i]]$brd == board[rot270]){return(i)}
    else if(Qlib[[i]]$brd == board[horFlip]){return(i)}
    else if(Qlib[[i]]$brd == board[vertFlip]){return(i)}
    else if(Qlib[[i]]$brd == board[diagFlip]){return(i)}
    else if(Qlib[[i]]$brd == board[crossdiagFlip]){return(i)}
    I <- i #if we get all the way here then we need to add the board configuration to Qlib
  }
  Qlib[[I+1]] <- list(brd = board, Q = 0)
  assign("Qlib",Qlib,envir = .GlobalEnv)
  return(I+1)
}

Qlib <- list()
Qlib[[1]] <- list(brd = c(0,0,0,0,0,0,0,0,0),Q=0)

learnTicTac <- function(Qlib,ep=.1,lrate=.1,disc=1,games)
{
  for(i in 1:games)
  {#Here is where we iterate a ton of games
    generateGame()
    while(TRUE) #this loop goes on for the duration of one game
    {
      poss <- showPossibleMoves(board)#To start, computer goes first and assesses possibilities
      #Now we decide how to chooce a move:
      if(runif(1) < ep)#random option
      {
        choice <- sample(1:length(poss),1)
        boardCom <- poss[choice]
      }
      else #greedy option
      {
        possQ <- numeric()
        for(i in 1:length(poss))
        {#here we grab the Q values for each possible move
          possQ[i] <- Qlib[[Qindex(poss[[i]],Qlib)]]$Q
        }
        boardCom <- poss[[match(max(possQ),possQ)]] #then we take the highest one
      }
      #Before we go on we check for a win
      if(reward(boardCom) > 0)
      {
        Qlib[[Qindex(boardCom)]]###########
        break
      }
      #Now that a move is chosen, the oponent picks randomly
      opPoss <- opPossibleMoves(boardCom)
      boardOp <- sample(1:length(opPass),1)
      boardRand <- opPoss[[boardOp]]
      
      #now we check for a loss
      if(reward(boardRand) < 0)
      {
        break
      }
      #Now we update
      
    }
  }
}


