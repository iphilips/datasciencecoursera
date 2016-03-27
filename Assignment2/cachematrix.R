## iphilips Coursera Assignment #2 Started  27/03/2016


makeCacheMatrix <- function(x = matrix()) {
  
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
  
  # Return a list
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  
  # Compute the inverse of a matrix or retrieve it from the cache
  
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
