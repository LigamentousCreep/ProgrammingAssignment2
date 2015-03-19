## makeCacheMatrix returns the following list of functions:
## 1. setMat: sets the value of a matrix.
## 2. getMat: gets the value of a matrix.
## 3. cacheinv: gets the cached value of the inverse of the matrix.
## 4. getinv: gets the cached value of the inverse of the matrix.

## makeCacheMatrix is used to to store a martix and a cached value of the inverse of the matrix.

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


## cacheSolve calculates the inverse of the matrix created by makeCacheMatrix

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
