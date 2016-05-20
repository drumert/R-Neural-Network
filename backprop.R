##Back propogation

#in order to back propogate we use the delta method. Finding a delta value for every node in
#a layer. We begin by calculating delta values, and subsequent partial derivatives of overall cost relative to 
#a particular weight.

#recall
sig <- function(x)
{
  1/(1+exp(-x))
}

d_sig <- function(s)
{#the derivative of sigmoid is the sigmoid times 1 - sigmoid.
  #just remember that s = sig(x), so we would replace it.
  s*(1-s)
}

#in the 11 lines of code thing, input is a row vector. Multiple sets of inputs corresond
#to the many rows in a matrix.
#these are your activations for layer 0 (input)
#activations for layer 1 is the sigmoid of the matrix multiplication of syn0, the weights
#after input layer 0, where the row corresponds to one source node.
#then to get l2 matrix, matrix multiply the l2 matrix with the syn1 weights matrix. And so on

#back propogating starts with the last layer. calculate the error in that layer
#by doing the output - L2
#then get the delta with l2 error * derivative of sigmoid of layer 1
#then the error of layer 1 is layer2 delta matrix mult by syn1 transpose 
#then the delta of layer 1 is l1 error times d_sig(l1)
#then the weight updates are plues equal that layer's transpose activations matrix multiplied by the next layer's delta

