## The following functions uses result caching technique to improve matrix 
## inverse calculation. Inversing matrix is costly process. Using these 
## functions to inverse same matrix repeatedly will result in better 
## performance.
##
## Example:
##  a <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
##  
##  for(i in 1:10 ) {
##     cacheSolve(a)
##  }
## 
##  a$getinverse()


## This function creates a special "matrix" object that can cache 
## it's inverse

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL

  set <- function(y) {
    x <<- y
    inv <<- NULL  ## remove cached result
  }
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix function. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve will retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...) ## perform inverse
  x$setinverse(inv)      ## update cache
  
  inv
}
