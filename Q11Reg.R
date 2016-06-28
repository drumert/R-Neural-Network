createR <- function(Nstate,Naction){
  matr <- matrix(0, ncol=Naction, nrow=Nstate)
  for(i in 1:Naction)
  {
    for(j in i:Naction)
    {
      if(i == j) {matr[i,j] <- 100}
      else {matr[i,j] <- -100}
    }
  }
  return(matr)
}

legalRandom <- function(total,nA)
{
  if(total <= nA)
  {
    return(sample(1:total,1))
  }
  else
  {
    return(sample(1:nA,1))
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

convTime <- function(nS,nA,ep=.1,lrate=.1,disc=1)
{
  Q <- matrix(0,nrow = nS, ncol = nA)
  time <- 0
  r <- createR(nS,nA)
  while(abs(max(Q[nS,])- 200) > .000001)
  {
    time <- time + 1
    total <- nS
    while(TRUE)
    {
      if(runif(1)>ep)
      {
        choiceCom <- match(max(Q[total,]),Q[total,])
      }
      else
      {
        choiceCom <- legalRandom(total,nA)
      }
      reward <- r[total,choiceCom]
      if(checkGameOver(total-choiceCom))
      {
        Q[total,choiceCom] <- Q[total,choiceCom]+lrate*(reward+disc*(100)-Q[total,choiceCom])
        break
      }
      else
      {
        choiceRand <- legalRandom(total-choiceCom,nA)
        if(checkGameOver(total-choiceCom-choiceRand))
        {
          Q[total,choiceCom] <- Q[total,choiceCom]+lrate*(reward+disc*(-100)-Q[total,choiceCom])
          break
        }
        else
        {
          Q[total,choiceCom] <- Q[total,choiceCom]+lrate*(reward+disc*max(Q[total-choiceCom-choiceRand,])-Q[total,choiceCom])
        }
        total <- total-choiceCom-choiceRand
      }
    }
  }
  #print(Q)
  return(time)
}

#convTime(11,3)

Qdata <- data.frame(matrix(ncol=3))
colnames(Qdata) <- c("States","Actions","Time")
Qc <- 0
for(i in 11:30)
{
  print(i)
  for(j in 2:6)
  {
    if(i%%(j+1) == 0){next}
    for(k in 1:10)
    {
      Qc <- Qc + 1
      Qdata[Qc,] <- c(i,j,convTime(i,j))
    }
  }
}


