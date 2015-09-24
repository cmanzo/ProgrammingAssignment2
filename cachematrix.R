## Calculate the 'in_x', i.e. the inverse of a matrix 'x'.
## The inverse matrix is created in the cache, so that if it is needed to be computed again,
## it can be looked up in the cache rather than recomputed. 
## Allows faster calculation for repetead calculation of inverse of large matrices


## 'makeCacheMatrix' creates a list containing a function to set/get the matrix 'x' 
## and its inverse 'in_x' 
makeCacheMatrix <- function(x = matrix()) {
  in_x <- NULL
  set <- function(y) {
    x <<- y
    in_x <<- NULL
  }
  get <- function() x
  setinv <- function(solve) in_x <<- solve
  getinv <- function() in_x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## 'cacheSolve'function (equivalent of the 'solve' function) 
## returns a matrix 'in_x' that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## First checks if 'in_x' has already been calculated. 
  ## If so, it gets it from the cache and skips the computation.
  in_x <- x$getinv()
  if(!is.null(in_x)) {
    message("getting cached data")
    return(in_x)
  }
  ## Otherwise, it calculates 'in_x' using 'solve' and sets it in the cache via the setinv function.
  data <- x$get()
  in_x <- solve(data, ...)
  x$setinv(in_x)
  in_x
}
