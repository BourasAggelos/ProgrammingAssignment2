## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## set the inverse property
  invProp <- NULL
  
  ## set the matrix
  setMatrix <- function(y){
    matrix <<- y
    inv <<- NULL
  }
  
  ## get the matrix
  get <- function(){

    matrix
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {

    invProp <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## returns the inverse
    invProp
  }
  
  ## Returns the list of methods
  list(setMatrix = setMatrix, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' ##
  
  ## get a matrix (the inverse of 'x')
  inv <- x$getInverse()
  
  ## returns if the inverse has already been calculated
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  
  ## get the matrix
  data <- x$get()
  
  ## calculate the inverse
  m <- solve(data) %*% data
  
  ## store the inverse
  x$setInverse(m)
  
  ## returning a matrix (the inverse of 'x')
  m 
  
}
