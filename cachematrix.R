## cacheMatrix is created to caching the inverse of a matrix
## rather than compute it repeatedly.

## makeCacheMatrix returns a list of functions:
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheMatrix is used to  to caching the inverse of a matrix
## rather than compute it repeatedly.

## cacheSolve sets the mean of the created vector.
## (1) If the mean has been calculated, the (2) step is skipped
## (2) Calculates the mean of the data and sets the value of the
## mean in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}