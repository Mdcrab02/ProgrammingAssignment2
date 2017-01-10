## makeCacheMatrix will take in a matrix and compute the inverse to be stored in cache.
## cacheSolve will load the matrix stored in cache or compute the inverse of the matrix passed into
## makeCacheMatrix.

## makeCacheMatrix takes a matrix and computes the inverse of that matrix then stores its inverse
## in cache to be used later.

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  
  store <- function(n) {
    x <<- n
    mat <<- NULL
  }
  
  grab <- function() x
  
  storeinverse <- function(solve) mat <<- solve
  
  retrieveinverse <- function() mat
  
  list(store = store, grab = grab,
       storeinverse = storeinverse,
       retrieveinverse = retrieveinverse)
}


## cacheSolve will check the cache and load up the matrix returned by makeCacheMatrix or compute
## the inverse of that matrix based on the information passed into makeCacheMatrix.

cacheSolve <- function(x, ...) {
  mat <- x$retrieveinverse()
  
  if(!is.null(mat)) {
    
    message("Retrieving stored data...")
    return(mat)
    
  }
  
  data <- x$grab()
  
  mat <- solve(data, ...)
  
  x$storeinverse(mat)
  mat
}


#Quick test
test <- diag(7,3)
mkc <- makeCacheMatrix(test)
cacheSolve(mkc)