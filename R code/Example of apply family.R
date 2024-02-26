# Example of apply in the iris dataset
apply(iris, 1, function(x){
  x
})
# The class of this output is a matrix

apply(iris, 1, function(x){
  x[1:4]
})
# The output is still a matrix and we want to fix that

apply(iris[1:4], 1, function(x){
  x
})
# Now the output is still a matrix, but of numbers

#The second and third seem pretty similar to me, I don't understand
#------------------------------------------------------------------------------#
#Example of lapply
lapply(1:10, sqrt) |> class() #returns a list 
sapply(1:10, sqrt) |> class() #returns numeric (like different observations of
#a numeric variable)
sapply(as.list(1:10), sqrt, simplify=FALSE) #returns a list

lapply(1:3, function(x) c(x,10)) #returns a list
sapply(1:3, function(x) c(x,10)) #returns a matrix
