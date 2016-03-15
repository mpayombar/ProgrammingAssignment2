## This function takes a Matrix and returns Inverse of the Matrix 
## This function creates a special "matrix" object that can cache its inverse. 
makeCachevMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve<- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}

