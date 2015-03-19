## makeCacheMatrix returns the following list of functions:
## 1. setMat: sets the value of a matrix.
## 2. getMat: gets the value of a matrix.
## 3. cacheinv: gets the cached value of the inverse of the matrix.
## 4. getinv: gets the cached value of the inverse of the matrix.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
  cache <- NULL
  setMat <- function(newValue) {
    x <<- newValue
    cache <<- NULL
  }
  getMat <- function() {
    x
  }
  cacheinv <- function(solve) {
    cache <<- solve
  }
  getinv <- function() {
    cache
  }
  list(setMat = setMat, getMat = getMat, cacheinv = cacheinv, getinv = getinv)
}
}


## The cacheSolve functioncomputes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$getMat()
  inverse <- solve(data)
  x$cacheinv(inverse)
  inverse
}
