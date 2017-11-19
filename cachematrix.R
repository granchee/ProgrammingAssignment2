## A pair of functions which allow the caller to save time by caching the
## inverse of a square 2D matrix.
##
## First call makeCacheMatrix() with a matrix argument to get a special
## 'matrix' object; then call cacheSolve() with the object as its argument to
## get the matrix inverse, which will be calculated and cached. Further calls
## of cacheSolve() will skip the calucation and simply return the cached
## inverse.

## Return a special 'matrix' object: a list of functions allowing a caller
## to set/get the matrix value and set/get the matrix inverse. If x is not a
## square 2D matrix return NA.
makeCacheMatrix <- function(x = matrix()) {

  ## Fail gracefully for non-square, non-2D matrices.
  dim_x <- dim(x)
  dims <- length(dim_x)
  if((dims != 2) || (dim_x[1] != dim_x[2])) {
    message("matrix is not square 2D - returning NA")
    return(NA)
  }
  
  ## Clear cached matrix inverse when object is created.
  i <- NULL
  
  ## Set matrix value and clear cached matrix inverse.
  set <- function(y) {
    x <<- y
    i <<- NULL ## New matrix means new inverse will have to be calculated. 
  }
  
  ## Get matrix value.
  get <- function() x
  
  ## Set cached matrix inverse.
  setinv <- function(inv) i <<- inv
  
  ## Get cached matrix inverse.
  getinv <- function() i

  ## Return special 'matrix object.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Return a matrix that is the inverse of the matrix in the special 'matrix'
## object x. Save time by returning the cached inverse, if it has been set.
cacheSolve <- function(x, ...) {

  ## If cached inverse has been set: print message; return inverse.
  i <- x$getinv()
  if(!is.null(i)) {
    message("returning cached inverse")
    return(i)
  }
  
  ## If cached inverse has not been set: print message; get matrix value;
  ## invert matrix; cache and return inverse.
  message("calculating, caching and returning inverse of matrix")
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
