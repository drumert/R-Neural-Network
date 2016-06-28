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
for(i in 11:50)
{
  print(i)
  for(j in 2:8)
  {
    if(i%%(j+1) == 0){next}
    for(k in 1:10)
    {
      Qc <- Qc + 1
      Qdata[Qc,] <- c(i,j,convTime(i,j))
    }
  }
}
save.image("Q11RegData2")


load("Q11RegData")
library(R2jags)
mdl <- '
  model{
    for(i in 1:2270){
      logTime[i] ~ dnorm(mu[i],1/vv)
      mu[i] <- b0 + bstates*states[i]+bactions*actions[i]+bsa*states[i]*actions[i]
    }
    vv ~ dgamma(1,.0008)
    b0 ~ dnorm(0,.0000001)
    bstates ~ dnorm(0,.0000001)
    bactions ~ dnorm(0,.0000001)
    bsa ~ dnorm(0,.0000001)
  }
'
states <- Qdata$States
actions <- Qdata$Actions
logTime <- log(Qdata$Time)

#states <- (states - mean(states))/sd(states)#
#actions <- (actions-mean(actions))/sd(actions)#

data.jags <- c("states","actions","logTime")
parms <- c("vv","b0","bstates","bactions","bsa")
writeLines(mdl,"QjagsModel.txt")

Qfit <- jags(data=data.jags,inits = NULL,parameters.to.save = parms,
             model.file = "QjagsModel.txt",n.chains = 4,
             n.iter = 12000,n.burnin = 2000,n.thin = 1)
Qfit
sims <- as.mcmc(Qfit)
plot(sims)
par(mfrow=c(1,1))
plot(states,logTime)
plot(actions,logTime)

