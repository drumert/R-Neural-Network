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


###Alternatively, and possibly faster, we can use matrix operations to speed up the run.
#however, the above solution works fine for now. Later we may update this section.
