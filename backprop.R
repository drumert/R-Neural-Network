##Back propogation

#The general form to update a bias or a weight is as follows

#where wb represents a single weight or bias, eta is the lerning rate, and grad is the gradient of the respective parameter

##In our learning process we will use stochastic gradient descent, therefore one of the
#first things we do to begin is to break our training data into clusters of the same size
  #note, data must be in a vector form


sigmoid <- function(z)
{
  return(1/(1+exp(-z)))
}

d_sig <- function(z)
{
  sigmoid(z)*(1-sigmoid(z))
}


updateNetworkMini <- function(net, trainData, eta)
{
  #Feedforward, saving the layer's activations
  layers <- length(net$weights)
  activations <- list()
  activations[[1]] <- trainData$input
  z <- list()
  inreps <-dim(trainData$input)[2]
  if(inreps >1)
  {
    copybiases <- list()
    for(blayer in 1:layers)
    {
      bcopy <- net$biases[[blayer]]
      copybiases[[blayer]] <- bcopy
      for(brep in 2:inreps)
      {
        copybiases[[blayer]] <- cbind(copybiases[[blayer]],bcopy)
      }
    }
  }
  else
  {
    copybiases <- net$biases
  }
  for(i in 1:layers)
  {
    z[[i]] <- t(net$weights[[i]]) %*% activations[[i]] + copybiases[[i]]
    activations[[i+1]] <- sigmoid(z[[i]])
  }
  
  #Now we compute layer deltas
  deltas <- list()
  deltas[[layers]] <- (activations[[layers+1]]-trainData$output) * d_sig(z[[layers]])
  for(j in (layers-1):1)
  {
    deltas[[j]] <- (net$weights[[j+1]]%*%deltas[[j+1]]) * d_sig(z[[j]])
  }
  
  #now we compute gradients for biases and weights
  biasgrad <- list()
  for(k in 1:layers)
  {
    sum_matrix <- matrix(1, ncol = 1, nrow = inreps)
    biasgrad[[k]] <- (1/inreps)*(deltas[[k]]%*%sum_matrix)
  }
  
  weightgrad <- list()
  for(l in 1:layers)
  {
    weightgrad[[l]] <- (1/inreps)*(activations[[l]]%*%t(deltas[[l]]))
  }

  
  #now we calculate the updated weights for each input.
  
  newbiases <- list()
  newweights <- list()
  for(m in 1:layers)
  {
    net$biases[[m]] <- net$biases[[m]] - eta * biasgrad[[m]]
    net$weights[[m]] <- net$weights[[m]] - eta * weightgrad[[m]]
  }
  #now we would average all the proposals and update the weights
  #however because we took a linear approach, this has been done already in the 
  #gradient computation section 
  return(net)
  #the way this works is you have to assign the output of this function to a net
  #it makes one step of gradient descent. 
}

#the only step left now is to partition the training data. This program runs a mini
#batch of any size given the weights, biases, and inputs.
#we will construct an outer function to pass in a complete data set with inputs,
#outputs, and partitioning guides.

trainNetwork <- function(trainDataTotal, net, eta, epochs, mini_size)
{
  totalIN <- dim(trainDataTotal$input)[2]
  nbatches <- round(totalIN/mini_size)
  
  for(i in 1: epochs)
  {
    shuffled <- sample(1:totalIN,totalIN)
    shuffDataIN <- rbind(trainDataTotal$input[,shuffled])
    shuffDataOUT <- rbind(trainDataTotal$output[,shuffled])
    for(j in 1:nbatches)
    {
      if(j == nbatches)
      {
        minibatchIN <- shuffDataIN[,(mini_size*(nbatches-1)+1):totalIN]
        minibatchOUT <- shuffDataOUT[,(mini_size*(nbatches-1)+1):totalIN]
      }
      else
      {
        minibatchIN <- shuffDataIN[,(mini_size*(j-1)+(1:mini_size))]
        minibatchOUT <- shuffDataOUT[,(mini_size*(j-1)+(1:mini_size))]
      }
      tdata <-list(input = minibatchIN, output = minibatchOUT)
      net <- updateNetworkMini(net,tdata, eta)
    }
    ##Here we will include a test data check for each epoch, giving an accuracy report
  }
  return(net)
}

#Training Data needs to be formatted as a 2 element list with input matrix and output matrix
  #the inputs and outputs for an individual case are aligned column wise, keeping
  #consistent with a typical mapping of a neural network where the nodes are stacked.
#Represent a neural network as a list with the following elements in the list
  #weights <- list(matrix()) matrix for each layer(starting with 1-2) where row is source node and column is receiving node
  #input <- vector() The results from the pervious layer's nodes
  #biases <- list(vector()) list elements are layers, vector elements are node biases, again start the list with layer 2, ignoring layer 1 as it has nothing to do with it.

#net <- list(
#  weights = list(
#    rbind(c(0,0),c(1,1)),
#    rbind(c(1,1),c(0,0))
#  ),
#  biases = list(
#    cbind(c(-1,-1)),
#    cbind(c(0,0))
#  )
#)
#trainData <- list(
#  input = cbind(c(-1,-1),c(0,0)),
#  output = cbind(c(0,0),c(1,1))
#)
#eta <- .5
#############################
#innn <- sample(c(0,1),3,TRUE)
#outt <- sum(innn)/3

#for(i in 2:10000)
#{
#  innnt <- sample(c(0,1),3,TRUE)
#  outtt <- sum(innnt)/3
#  innn <- cbind(innn,innnt)
#  outt <- cbind(outt,outtt)
#}
#trainData <- list(
#  input = innn,
#  output = outt
#)
#
#
#eta <- .005
#epochs <- 300
#mini_size <- 10
##NOTE to resume: can't get the shuffDataIN to work.

#net <- trainNetwork(trainData, net, eta, epochs, mini_size)


#nnpass(c(0,0,0), net)
