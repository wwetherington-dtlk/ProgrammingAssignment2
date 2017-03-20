## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function - creates a Matrix object with a cacheable inverse calculation

makeCacheMatrix <- function(x = matrix()) {
  # initialize variable
  matrixinverse <- NULL
  # set function - assigns the matrix values and returns inverse to NULL
  set <- function(y)
  {
    
    x <<- y
    matrixinverse <<- NULL
  }
  # get function - returns the matrix
  get <- function() x
  # setinverse function - assigns the inverse value to the cache
  setinverse <- function(minverse) matrixinverse <<- minverse
  getinverse <- function() matrixinverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## cacheSolve function - returns a cached inverse to a matrix if it exists or generates the inverse and stores the result in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  # Retrieve value of inverse stored in x
  minverse <- x$getinverse()
  # If minverse is not NULL we have a stored inverse - hooray!
  if(!is.null(minverse))
  {
    # Notify and return cached inverse
    message("getting cached inverse of matrix")
    return(minverse)
  }
  # else compute and store the inverse
  message("no cached inverse of matrix, calculating")
  data <- x$get()
  minverse <- solve(data, ...)
  x$setinverse(minverse)
  minverse
  
}