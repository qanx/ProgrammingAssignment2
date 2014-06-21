## 'makeCacheMatrix' creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cacheMatrix <- NULL
  set <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(solve) cacheMatrix <<- solve
  getInverseMatrix <- function() cacheMatrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}

## The following function finds the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it gets the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the 
#'setInverseMatrix' function.

cacheSolve <- function(x = matrix(), ...) {
  cacheMatrix <- x$getInverseMatrix()
  if(!is.null(cacheMatrix)) {
    message("getting cached data")
    return(cacheMatrix)
  }
  data <- x$get()
  cacheMatrix <- solve(data, ...)
  x$setInverseMatrix(cacheMatrix)
  cacheMatrix
}