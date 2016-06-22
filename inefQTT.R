#Here we build an inefficient Q learning setup where we don't check for symetry

#note that Q values usually are part of state-action pairs, but in our case, we consider a state to state
#model where the Q value represents the value of ARRIVING there
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

addtoQ <- function(board,Q)
{
  I <- 0
  for(i in 1:length(Q))
  {
    if(all(Q[[i]]$board == board))
    {
      return(Q)
    }
    I <- i
  }
  Q[[I+1]] <- list(board = board, q = 0)
  return(Q)
}

Qind <- function(board,Q)
{
  for(i in 1:length(Q))
  {
    if(all(Q[[i]]$board == board))
    {
      return(i)
    }
  }
}

random.choice <- function(poss)
{
  choice <- sample(1:length(poss),1)
  return(poss[[choice]])
}

getQ <- function(board,Q)
{#future effeciency idea is to merge this function with the Q adder function
  for(i in 1:length(Q))
  {
    if(all(Q[[i]]$board == board))
    {
      return(Q[[i]]$q)
    }
  }
}

randomStart <- function()
{
  choice <- sample(1:3,1)
  board <- c(0,0,0,0,0,0,0,0,0)
  
  if(choice == 1)
  {
    ind <- sample(1:9,4)
    board[ind] <- sample(c(1,1,-1,-1),4)
  }
  if(choice == 2)
  {
    ind <- sample(1:9,2)
    board[ind] <- sample(c(1,-1),2)
  }
  if(choice == 3)
  {
    board <- board
  }
  return(board)
}

Q <- list()
Q[[1]] <- list(board = c(0,0,0,0,0,0,0,0,0), q = 0)
learnTTinef <- function(Q,ep=.999,lrate=.1,disc=1,games)
{
  for(i in 1:games)#play many rounds
  {
    cat("Game ",i," of ",games,"\n")
    board <- randomStart() #newgame
    #board <- c(0,0,0,0,0,0,0,0,0)
    Q <- addtoQ(board,Q)
    while(TRUE)#Play a single game
    {#computer goes first
      poss <- showPossibleMoves(board) #Observe the possible moves
      if(runif(1) > ep) #decide how to play
      {#greedy play
        #check to see if the possibilities have Q values and add them if they don't
        for(i in 1:length(poss))
        {
          Q <- addtoQ(poss[[i]],Q) #if already contained then it returns original Q
        }
        #Now obtain the Q values
        possQs <- vector()
        for(i in 1:length(poss))
        {
          possQs[i] <- getQ(poss[[i]],Q)
        }
        boardCom <- poss[[match(max(possQs),possQs)]] #Choose highest board Q value
      }
      else
      {#random exploration play
        boardCom <- random.choice(poss)
        Q <- addtoQ(boardCom,Q)
      }
      #Before going on we need to check for a win OR A DRAW
      if(reward(boardCom) == 100)
      {
        cat("Computer Wins!!\n")
        loc <- Qind(boardCom,Q)
        Q[[loc]]$q <- Q[[loc]]$q + lrate*(reward(boardCom)+disc*(100)-Q[[loc]]$q)
        break
      }
      if(all(boardCom != 0))
      {
        cat("Cats Game...\n")
        loc <- Qind(boardCom,Q)
        Q[[loc]]$q <- Q[[loc]]$q + lrate*(0-Q[[loc]]$q)
        break
      }
      #Now that board is chosen we have the oponent choose randomly
      opPoss <- opPossibleMoves(boardCom)
      boardOp <- opPoss[[sample(1:length(opPoss),1)]]
      for(i in 1:length(opPoss))
      {
        Q <- addtoQ(opPoss[[i]],Q) #if already contained then it returns original Q
      }
      #Now we need to check if the oponent has won
      if(reward(boardOp) == -100)
      {
        cat("Random Wins!!\n")
        loc <- Qind(boardCom,Q)
        Q[[loc]]$q <- Q[[loc]]$q + lrate*(reward(boardOp)+disc*(-100)-Q[[loc]]$q)
        break
      }
      #Now that the oponent has chosen, we learn
      loc <- Qind(boardCom,Q)
      #get max Q from boardOp
      tempPoss <- showPossibleMoves(boardOp)
      for(i in 1:length(tempPoss))
      {
        Q <- addtoQ(tempPoss[[i]],Q) #if already contained then it returns original Q
      }
      tempQs <- vector()
      for(i in 1:length(tempPoss))
      {
        tempQs[i] <- getQ(tempPoss[[i]],Q)
      }
      Q[[loc]]$q <- Q[[loc]]$q + lrate*(reward(boardCom)+disc*max(tempQs)-Q[[loc]]$q)
      board <- boardOp #now we begin again with the oponents board chocie as our current board
    }
    ep <- ep*.999
  }
  assign("Q",Q,envir=.GlobalEnv)
}

learnTTinef(Q,ep=.999,lrate=.1,disc=1,games=2000) ######################################

printBoard <- function(board)
{
  char <- character()
  for(i in 1:9)
  {
    if(board[i]==1)
    {
      char[i] <- "X"
    }
    else if(board[i] == -1)
    {
      char[i] <- "O"
    }
    else
    {
      char[i] <- " "
    }
  }
  
  cat(char[1],"|",char[2],"|",char[3],"\n_________\n")
  cat(char[4],"|",char[5],"|",char[6],"\n_________\n")
  cat(char[7],"|",char[8],"|",char[9],"\n")
}

playTT <- function(Q)
{
  board <- c(0,0,0,0,0,0,0,0,0)
  while(TRUE)
  {
    poss <- showPossibleMoves(board) #Observe the possible moves
    possQs <- vector()
    for(i in 1:length(poss))
    {
      possQs[i] <- getQ(poss[[i]],Q)
    }
    board <- poss[[match(max(possQs),possQs)]] #Choose highest board Q value
    
    cat("Computers Move\n")
    printBoard(board)
    if(reward(board) > 0)
    {
      cat("Computer Wins!!\n")
      break
    }
    if(all(board != 0))
    {
      cat("Cats Game...\n")
      break
    }
    
    cat("Player Move\n")
    choice <- as.numeric(readline("Select the number of the slot you wish to play: "))
    board[choice] <- -1
    printBoard(board)
    if(reward(board) < 0)
    {
      cat("Player Wins!!\n")
      break
    }
  }
}