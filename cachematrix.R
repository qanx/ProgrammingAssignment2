## 'makeCacheMatrix' creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  matrixCache <- NULL
  set <- function(y) {
    x <<- y
    matrixCache <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) matrixCache <<- solve
  getMatrix <- function() matrixCache
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}

## The following function finds the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it gets the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the 'setMatrix'
# function.

cacheSolve <- function(x = matrix(), ...) {
  matrixCache <- x$getMatrix()
  if(!is.null(matrixCache)) {
    message("getting cached data")
    return(matrixCache)
  }
  data <- x$get()
  matrixCache <- solve(data, ...)
  x$setMatrix(matrixCache)
  matrixCache
}