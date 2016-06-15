options(scipen=999)
Q <- matrix(0,nrow=21,ncol=3)#,nrow=11)
r <- rbind(
  c(100,-100,-100),
  c(0,100,-100),
  c(0,0,100),
  c(0,0,0),
  c(0,0,0),
  c(0,0,0),
  c(0,0,0),
  c(0,0,0),
  c(0,0,0),
  c(0,0,0),
  c(0,0,0) , c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0),c(0,0,0)
)



legalRandom <- function(total)
{
  if(total <=3)
  {
    return(sample(1:total,1))
  }
  else
  {
    return(sample(1:3,1))
  }
}

checkGameOver <- function(total)
{
  if(total==0)
  {
    return(TRUE)
  }
  else
  {
    return(FALSE)
  }
}

learn11 <- function(Q,r,ep,lrate,disc,games,start)
{
  for(i in 1:games)
  {
    cat("\nGame ",i," of ",games,"\n")
    total <- start
    while(TRUE)#This plays one game
    {
      if(runif(1)>ep)#greedy step
      {
        choiceCom <- match(max(Q[total,]),Q[total,]) #finds the highest valued next state
      }
      else #epsilon random step
      {
        choiceCom <- legalRandom(total) #randomly selects the next legal state
      }
      cat("Qcom chooses: ",choiceCom," total remaining: ",total-choiceCom,"\n")
      reward <- r[total,choiceCom]
      if(checkGameOver(total-choiceCom))
      {#if the game is over immediatly it means the computer won and learns positivity immediatly
        Q[total,choiceCom] <- Q[total,choiceCom]+lrate*(reward+disc*(100)-Q[total,choiceCom])
        cat("Q wins!\n")
        break
      }

      #before, we had set things up so that it would learn right after it played,
      #but in fact you can only learn something in this game after your opponent plays.
      #Therefore we will include another step to change state again before it learns.
      #We will have it play a random player to start
      else
      {
        choiceRand <- legalRandom(total-choiceCom)
        cat("Rand chooses: ",choiceRand," total remaining: ",total-choiceRand-choiceCom,"\n")
        if(checkGameOver(total-choiceCom-choiceRand))
        {#if the opponent ends the game, the computer learns negativity immediatly
          Q[total,choiceCom] <- Q[total,choiceCom]+lrate*(reward+disc*(-100)-Q[total,choiceCom])
          cat("Rand wins!\n")
          break
        }
        else
        {#otherwise it will learn about its choice based on where it ended up 
          Q[total,choiceCom] <- Q[total,choiceCom]+lrate*(reward+disc*max(Q[total-choiceCom-choiceRand,])-Q[total,choiceCom])
        }
        total <- total-choiceCom-choiceRand #now we end and repeat till the game is over
      }
    }
  }
  assign("Q",Q,envir=.GlobalEnv)
}

learn11(Q,r, ep=.1,lrate=.1,disc=1,games=20000,start=21)

play11 <- function(Q,comFirst=TRUE,start=21)
{
  total <- start
  cat("Total: ", total,"\n")
  if(comFirst)
  {
    choice <- match(max(Q[total,]),Q[total,])
    total <- total - choice
    cat("Computer chooses: ",choice," Total remining: ",total,"\n")
  }
  while(TRUE)
  {
    while(TRUE)
    {
      pchoice <- as.numeric(readline("Player Select 1, 2, or 3: "))
      if((pchoice != 1 && pchoice != 2 && pchoice !=3) || total-pchoice < 0)
      {
        pchoice <- as.numeric(readLines("Invalid Entry, Please select a legal move: "))
      }
      else {break}
    }
    total <- total - pchoice
    cat("Total remaining: ",total,"\n")
    if(checkGameOver(total))
    {
      cat("Player Wins!!!\n")
      break
    }
    choice <- match(max(Q[total,]),Q[total,])
    total <- total - choice
    cat("Computer chooses: ",choice," Total remining: ",total,"\n")
    if(checkGameOver(total))
    {
      cat("Computer Wins!!!\n")
      break
    }
  }
}
