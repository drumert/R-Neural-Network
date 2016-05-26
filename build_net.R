##Network Creator

#Represent a neural network as a list with the following elements
  #layer_sizes <- vector() not including input layer
  #weights <- list(matrix()) matrix for each layer(starting with 1-2) where row is source node and column is receiving node
  #input <- vector() The results from the pervious layer's nodes
  #biases <- list(vector()) list elements are layers, vector elements are node biases, again start the list with layer 2, ignoring layer 1 as it has nothing to do with it.


##pseudo code##

#We will build a function to make setting up a neural network easier.

#Prompt the user to specify the number of layers in their network INCLUDING the input layer
#Propmot the user to specify the number of nodes per layer INCLUDING the input layer
#Generate random normals for the biases vectors and weights matrices
#globally assign the list of these elements, named after what they want it named


BuildNet <- function()
{
  nlay <- as.numeric(readline(prompt = "Enter the number of layers in your network (including input layer) : "))
  ninlay <- numeric()
  for(i in 1:nlay)
  {#Get layer sizes
    n <- as.numeric(readline(prompt = paste("Enter the number of nodes in layer ", i, " : ")))
    ninlay[i] <- n
  }
  #create biases list (not including first layer)
  biases <- list()
  for(j in 1:(nlay-1))
  {
    biases[[j]] <- rnorm(ninlay[j+1],0,.1)
  }
  
  weights <- list()
  for(k in 1:(nlay-1))
  {
    weights[[k]] <- matrix(rnorm(ninlay[k]*ninlay[k+1],0,.1), nrow = ninlay[k], ncol = ninlay[k+1])
  }
  name <- readline(prompt = "Enter the name of your network : ")
  assign(name, list(weights = weights,biases = biases), envir = .GlobalEnv)
}




