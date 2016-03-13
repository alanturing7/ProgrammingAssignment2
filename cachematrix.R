## Functions below are supposed to cache the inverse of a matrix, and then return the value of the inversed matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) a <<- inverse
  getinverse <- function() a
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  t <- x$getinverse()
  if(!is.null(a)) {
    message("getting inversed matrix")
    return(a)
  }
  data <- x$get()
  t <- solve(data, ...)
  x$setinverse()
  t       ## Return a matrix that is the inverse of 'x'
}
