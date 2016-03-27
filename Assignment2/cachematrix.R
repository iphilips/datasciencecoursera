## iphilips Coursera Assignment #2 Started  27/03/2016
##
##
##
##

makeCacheMatrix <- function(x = matrix()) {
  # A function that takes  a matrix as input and returns a vector with a list of four fuctions
  # that are applied to the input matrix (x). The set function stores a matrix (x) in the vector,
  # the get function returns the matrix (x), the setinverse function computes the matrix inverse
  # and the getinverse function returns the inverse of matrix x
  
  # set  m to null to start the function
  
  m <- NULL
  
  # set the value of the matrix, x to what was provided as an arugment tot he calling function
  # set the value of m in this funcion to NULL.

  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Return the matrix X   
  get <- function() x
  
  # Set the intervse of the matrix
  setinverse <- function(inverse) m <<- inverse
  
  # Retrieve the inverse
  getinverse <- function() m
  
  # Return a vector witht the four functions (set, get, setinverse, getinverse)
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  # Compute the inverse of a matrix or retrieve it from the cache, input is a vector
  
  m <- x$getinverse()
  
  # if there is a computed inverse then return it, else continue
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # Retrieve the matrix from vector X
  data <- x$get()
  
  # Compute the inverse of data and store in m
  m <- solve(data, ...)
  
  # Set the vector X inverse matrix to m
  x$setinverse(m)
  
  # return m, the inverse of the matrix
  m
}
