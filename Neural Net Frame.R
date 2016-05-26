#Neural Network Framework

#Represent a neural network as a list with the following elements
  #layer_sizes <- vector() not including input layer
  #weights <- list(matrix()) matrix for each layer(starting with 1-2) where row is source node and column is receiving node
  #input <- vector() The results from the pervious layer's nodes
  #biases <- list(vector()) list elements are layers, vector elements are node biases, again start the list with layer 2, ignoring layer 1 as it has nothing to do with it.



###pseudo code###

#In order to make a neural network operate, given that you have a valid network as described above,
#you need to have a looping mechanism that goes from layer to layer, node to node, doing calculations.

#Begin by determining the number of layers you need to pass through, not including the input layer.
#For every Layer, do the following:
  #Create a place to store the outputs from the current layer's nodes.
  #For each node, do the following:
    #take the vector of inputs from the previous layer, and multiply it element-wise by the weights that correspond to the current node.
    #add the corresponding bias from the current node to each of those products
    #sum up all the elements in the resulting vector
    #apply the sigmoid function (or other activation method) to the result
    #save that result as input for the next layer in the previously defined vector
  #Once the all the node outputs in a layer have been computed, redefine the input vector as these outputs and free up their previous temorary holding place
  #If you are the last layer, you need to decide what kind of activation function you will use. Clasification is fine to use the sigmoid, but if you need answers outside of 0 to 1 then a different function must used. Make adjustments in the code accordingly.
#The output of the last layer is the final product of the network. Use as you have trained it to.


#Running a Neural Network

sigmoid <- function(z)
{
  return(1/(1+exp(-z)))
}


RunNN <- function(input, layer_sizes, weights, biases) #Inputs include all elements of a network list.
{
  layers <- length(layer_sizes) #begin by determining the number of layers, not including input layer
  for(curr_layer in 1:layers) #for every layer, follow this procedure
  {
    temp_in <- NULL #we need a place to store the output of each neuron in the current layer
    for(node in 1:layer_sizes[curr_layer]) #for each node in a layer follow this procedure
    {#sum up the weights multiplied by their inputs. Add biases then apply sigmoid function.
      temp_in[node] <- sigmoid(sum(weights[[curr_layer]][,node]*input) + biases[[curr_layer]][node])
    }
    input <- temp_in #set temp_in as input for the next layer
  }#if you were to change the output activation function, it would be around here.
  return(input)
}


###Alternatively, we can use matrix operations to speed up the run.
#Specifically, this allows us to create input matrices and run several rounds at once

#Begin with input in the form of a row vector. If you have three input nodes, then you
#need three inputs lined up in the same row. Doing many runs corresponds to many rows.
#a 4x3 matrix for input corresponds to three inputs, run 4 times.
#another name for these inputs is the activations for layer 0.

#the next thing to consider is the activations for layer 1, the first hidden layer.
#an activation is the output for a single node, a signal strength if you will.
#activations are calculated by multiplying the inputs to that node by their corresponding 
#weights, adding them up, adding in a bias, and then taking the sigmoid of that result.

#to calculate the activations for layer 1, you need to have the weights. Weights are also
#represented as a matrix, denoted as the weights for layer 1, ie the weights used
#in calculating the activations for layer 1, even though the weights symbolically
#exist between the two layers. the rows correspond to the nodes in the previous layer
#which I call the source nodes. The columns correspond to the nodes in the current layer.
#ie, all the weights in column 2 are the weights used in calculating the activation of 
#node 2 in the current layer.
#A column vector of biases for the current layer is also kept, corresponding to the
# nodes in the current layer. There is no bias vector for layer 0 (the inputs)

#To calculate the activations for layer 1, matrix multiply the weights for layer 1
# by the activations for layer 0, then add the bias vector, then element wise apply the
# sigmoid function. To get the activations for layer 2, matrix multiply the activation
#(order matters) for layer 2 by the weights for layer 1, plus layer 2 biases, 
#element wise sigmoid function. And so on for subsequent layers.

#just something to keep track of. Before we apply the sigmoid function, we have a
# weighted input, we denote that value as Z.

#a quirk in R is that the biases MUST be standard vectors, not arrays or matrices
#with a dimension of 1. It must be a regular, non rbind or rbind vector of values.
#this ensures that the biases distribute properly when multiple input sets are 
#introduced.

#also, when we rbind the inputs, it makes sure we have a row vector for a single
#input, but it does nothing to a matrix of multiple inputs, so its fine to keep

###These are just notes to remember the structure of the network inputs
#input <- matrix()
#weights <- list(matrix())
#biases <- list(vector())

nnpass <- function(input, weights, biases)
{
  layers <- length(weights)
  activations <- rbind(input)

  for(i in 1:layers)
  {
    z <- activations%*%weights[[i]]+biases[[i]]
    activations <- sigmoid(z)
  }
  return(activations)
}

#remember. you can save a neural network into a list to make it more consise.

