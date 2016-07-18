#Connect Four

board <- matrix(1:42,nrow=6,ncol=7)
board #this is how we represent the board visually and computationally


returnVal <- function(val)
{
  if(val == 4)
  {
    return(100)
  }
  if(val == -4)
  {
    return(-100)
  }
  return(0)
}
reward <- function(board)
{
  #check rows
  for(i in 1:6)
  {
    for(j in 1:4)
    {
      rw <- sum(board[i,(j:(j+3))])
      if(returnVal(rw) != 0)
      {
        return(returnVal(rw))
      }
    }
  }
  #check columns
  for(i in 1:7)
  {
    for(j in 1:3)
    {
      cl <- sum(board[(j:(j+3)),i])
      if(returnVal(cl) != 0)
      {
        return(returnVal(cl))
      }
    }
  }
  #check diagonals
  for(i in 1:4)
  {
    for(j in 1:3)
    {
      tempB <- board[j:6,i:7]
      dg <- sum(diag(tempB)[1:4])
      if(returnVal(dg) != 0)
      {
        return(returnVal(dg))
      }
    }
  }
  #check antidiagonals
  for(i in 1:4)
  {
    for(j in 1:3)
    {
      tempB <- board[6:1,]
      tempB <- tempB[j:6,i:7]
      adg <- sum(diag(tempB)[1:4])
      if(returnVal(adg) != 0)
      {
        return(returnVal(adg))
      }
    }
  }
  return(0)
}

move <- function(slot, board, player)
{
  cl <- board[6:1,slot]
  ind <- 7-match(0,cl)
  board[ind,slot] <- player
  return(board)
}

isvalid <- function(slot,board)
{
  if(is.na(match(slot,1:7)))
  {
    return(FALSE)
  }
  return(any(board[,slot]==0))
}

printBoard <- function(board)
{
  cat("\t  1   2   3   4   5   6   7 \n\n")
  for(i in 1:6)
  {
    cat("\t| ")
    for(j in 1:7)
    {
      char <- ifelse(board[i,j] == 1,"X",ifelse(board[i,j] == -1,"O"," "))
      cat(char,"| ")
    }
    cat("\n")
  }
}






b <- rbind(c(0,0,0,0,0,0,0),
           c(0,0,0,0,0,0,0),
           c(0,0,0,0,0,0,0),
           c(0,0,0,0,0,0,0),
           c(0,0,0,0,0,0,0),
           c(0,0,0,0,0,0,0))

c4_2p <- function()
{
  board <- rbind(c(0,0,0,0,0,0,0),
             c(0,0,0,0,0,0,0),
             c(0,0,0,0,0,0,0),
             c(0,0,0,0,0,0,0),
             c(0,0,0,0,0,0,0),
             c(0,0,0,0,0,0,0))
  
  while(TRUE)
  {
  printBoard(board)
  if(reward(board) == -100)
  {
    print("Player 2 wins!!!")
    break
  }
  if(reward(board) == -10)
  {
    print("Its a draw...")
    break
  }
  
  p1m <- as.numeric(readline("Player 1 choose a column : "))
  if(!isvalid(p1m,board))
  {
    while(TRUE)
    {
      print("Error, must select an open, valid slot")
      p1m <- as.numeric(readline("Player 1 choose a column : "))
      if(!isvalid(p1m,board))
      {
        next
      }
      break
    }
  }
  board <- move(p1m,board,1)
  printBoard(board)
  if(reward(board) == 100)
  {
    print("Player 1 wins!!")
    break
  }

  p2m <- as.numeric(readline("Player 2 choose a column : "))
  if(!isvalid(p2m,board))
  {
    while(TRUE)
    {
      print("Error, must select an open, valid slot")
      p2m <- as.numeric(readline("Player 2 choose a column : "))
      if(!isvalid(p2m,board))
      {
        next
      }
      break
    }
  }
  board <- move(p2m,board,-1)
  }
}
