##  Assignment: Caching the Inverse of a Matrix

##  makeCacheMatrix function
##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  ## Step 0: Initialize the inverse property
  inverse <- NULL
  
  ## Step 1: Method to set the matrix
  set <- function(x) { mtx <<- x; inverse <<- NULL; }
  
  ## Step 2: Method to get the matrix
  get <- function() return(mtx);
  
  ## Step 3: Method to set the inverse of the matrix
  setinv <- function(inv) inverse <<- inv;
  
  ## Step 4: Method to get the inverse of the matrix
  getinv <- function() return(inverse);
  
  ## Step 5: Returns the list of methods
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
  
}


## cacheSolve function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## getting a matrix that is the inverse of 'x'
  inverse <- mtx$getinv()
  
  ## If the inverse has already been calculated,returning a matrix that is the inverse of 'x' 
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  ## If the inverse has not been calculated,returning a matrix that is the inverse of 'x'
  data <- mtx$get()
  invserse <- solve(data, ...)
  mtx$setinv(inverse)
  return(inverse)
}
