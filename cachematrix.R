## In this assignment, the function creates a matrix and in which it can cashe matrix's inverse and save it in cache memory.
## m is set for NULL. The m is an empty variable for inversion of the matrix.
## The function makeCacheMatrix calculates the inverse of the matrix and save in m.
## cachesolve function retrive the inverse of matrix.
## set function takes the argument to parent enviroment (new different environment).
makeCacheMatrix <- function(X = matrix()) {
  m<- NULL
  set<- function(y) {
    x<<- y
    m<<- NULL
  }              
  ## get function make an access to matrix X and later an access to variable m.
  
  get<- function() X
  setinverse<- function(solve) m<<-solve 
  getinverse<- function() m
  list(set=set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}                        
## The function cacheSolve calculates the inverse of matrix that is saved already in cache memory.
## Return a message "Getting cached data".
cacheSolve <- function(X, ...) {
  m<- X$getinverse()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  ## get function is used to access matrix X.  
  data<- X$get()
  m<- solve(data, ...)
  X$setinverse(m)
  m
}
#An example of the run in R for matrix X and its inverse
#	  X= rbind (c(1, -5), c(-5, 1)
#	  n= makeCacheMatrix(X)
#	  n$get()
#	        [,1] [,2]
#	  [1,]    1   -5
#	  [2,]   -5    1
#The values for inverse of matrix X
#	    [,1]    		[,2]
# 	[1,] -0.04166667 -0.20833333
#	  [2,] -0.20833333 -0.04166667          