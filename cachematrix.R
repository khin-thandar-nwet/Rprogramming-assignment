## makeChaceMatrix and cacheSolve allow to store in memory a matrix and its inverse 
## in order to subsequentelly accelerate accessing the data. 

## makeCacheMatrix creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x 
  setinv <- function(z) inv_matrix <<- z
  getinv <- function() inv_matrix
  list(set = set, get = get,
       setinv  = setinv ,
       getinv = getinv)
}

## cacheSolve computes the inverse of the object returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
