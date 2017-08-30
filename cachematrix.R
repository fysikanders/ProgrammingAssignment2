## Functions to cache results from computational expensive operations.
## The cacheSolve function  will calculate the inverse of the matrix the first time it is called. 
## The result will be cached and any subsequent calls cacheSolve will return the cached value.
##
## First create a cachable matrix with makeCacheMatrix and then call cacheSolve
##
## Example usage:
##
## m <- matrix(c(1,2,3,4,5,6,9,8,9), nrow=3, ncol=3)
## cachedMatrix <- makeCacheMatrix(m)
## cachedInverse <- cacheSolve(cachedMatrix)
##
## Errors: cacheSolve will stop with an error in case the matrix is not a square matrix. 


## Takes an ordinary function and turns it into a matrix wich will cache the inverse.
## The function makeCacheMatrix will return a list of functions:
## makeCacheMatrix$get - Get the underlying matrix 
## makeCacheMatrix$set - Set new data for the underlying matrix. Any cached inverse will be reset. 
## makeCacheMatrix$getinverse - Get the cached inverse matrix.
## makeCacheMatrix$setinverse - Set the cached inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {  # Set new matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x

  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## Calculated the inverse of a cahced matrix and updates the cached value.
## Input MUST be a cachesMatrix, created by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(is.null(inv)) {
    data_matrix <- x$get()
    dims <- dim(data_matrix)
    if(dims[1] != dims[2]) {
      stop('The matrix must be square to be able to find an inverse.')
    }
    identitymatrix <- diag(dims[1])      
    inv <- solve(data_matrix, identitymatrix)
    x$setinverse(inv)
  }
  else {
    message('Using cached data.')
  }
  inv
}
