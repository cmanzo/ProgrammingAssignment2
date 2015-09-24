## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix 'in_x' that is the inverse of 'x'
  in_x <- x$getinv()
  if(!is.null(in_x)) {
    message("getting cached data")
    return(in_x)
  }
  data <- x$get()
  in_x <- solve(data, ...)
  x$setinv(in_x)
  in_x
}
