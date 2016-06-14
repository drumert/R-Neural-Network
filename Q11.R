#Q learning on 22 count down
options(scipen=999)
Q1 <- matrix(0,nrow=11,ncol=3) #matrix initializing long term values for player 1
Q2 <- matrix(0,nrow=11,ncol=3)
r <- rbind(
  c(100,-100,-100), #if 1 remains then playing 1 wins and otherwise loses
  #c(-100,100,-100), #if 2 remain then playing 2 wins and otherwise loses
  c(0,100,-100) #test 2
  #c(-100,-100,100), #if 3 remain then playing 3 wins and otherwise loses
  c(0,100,-100), #test 3
  #c(-100,-100,-100), #if 4 remain then a loss is inevitable
  c(0,0,0),#test for 4
  #c(0,-100,-100), #if 5 remain then playing 2 or 3 leads to loss
  c(0,0,0),#test for 5
  #c(0,0,-100), #if 6 remain then playing 3 leads to loss
  c(0,0,0),#test for 6
  c(0,0,0), #beyond this point nothing is obviously deterministic (but can be derived)
  c(0,0,0),
  c(0,0,0),
  c(0,0,0),
  c(0,0,0),
)

Qchoice <- function(total,Q,ep)
{
  if(runif(1)>ep) #equivalent to flipping a coin. This is the greedy choice
  {
    choice <- match(max(Q[total,]),Q[total,]) #makes the choice with the highest Q value given the current state
  }
  else
  {
    choice <- sample(1:3,1) #makes random choice
  }
  return(choice)
}

is.special.choice <- function(choice,total)
{
  if(total-choice <= 0) {return(TRUE)}
  else {return(FALSE)}
}
learn22 <- function(Q1,Q2=NULL,r,ep,lrate=.9,disc=.5,games,humanP2)
{
  for(i in 1:games)
  {
  cat("Game ",i," of ",games,"\n")
  total <- 11
  while(TRUE)
  {
    choice <- Qchoice(total,Q1,ep) #player1 go
    reward <- r[total,choice]
    if(is.special.choice(choice,total)) 
    {
      signshift <- (2)*sign(total-choice)+1#differentiates between winning at 0 and losing
      Q1[total,choice] <- Q1[total,choice]+lrate*(reward+signshift*disc*100-Q1[total,choice])
      cat("Player 1 (computer) has chosen: ",choice," The total remaining is: ",total-choice,"\nPlayer 1 (computer) wins!\n")
      break
    }
    else
    {#this is the case when the game is played on
      Q1[total,choice] <- Q1[total,choice]+lrate*(reward+disc*max(Q1[total-choice,])-Q1[total,choice])
    }
    total <- total - choice
    cat("Player 1 (computer) has chosen: ",choice," The total remaining is: ",total,"\n")
    
    #now player two goes
    if(humanP2 == TRUE)
    {
      choice <- as.numeric(readline("Player 2 select 1, 2, or 3: "))
      total <- total - choice
      cat("The total remaining is: ",total,"\n")
      if(total == 0)
      {
        cat("Player 2 wins!\n")
        break
      }
    }
    else
    {
      choice <- Qchoice(total,Q2,ep) #computer [player 2 goes]
      reward <- r[total,choice]
      if(is.special.choice(choice,total)) 
      {
        signshift <- (2)*sign(total-choice)+1#differentiates between winning at 0 and losing
        Q2[total,choice] <- Q2[total,choice]+lrate*(reward+signshift*disc*100-Q2[total,choice])
        cat("Player 2 (computer) has chosen: ",choice," The total remaining is: ",total-choice,"\nPlayer 2 (computer) wins!\n")
        break
      }
      else
      {#this is the case when the game is played on
        Q2[total,choice] <- Q2[total,choice]+lrate*(reward+disc*max(Q2[total-choice,])-Q2[total,choice])
      }
      total <- total - choice
      cat("Player 2 (computer) has chosen: ",choice," The total remaining is: ",total,"\n")
    }
  }
  }
  assign("Q1",Q1,envir=.GlobalEnv)
  assign("Q2",Q2,envir=.GlobalEnv)
}

#need to resolve this. It has converged properly in the past, but not now.


