## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve shows the inverse of a matrix if is in memory or computes the inverse and then shows the inverse



## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(Y) {
    X <<- Y
    inverse <<- NULL
  }
  get <- function() X
  setinverse <- function(Inverse) inverse <<- Inverse
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  if(require("corpcor")){
    print("corpcor is installed")
  } else {
    print("attempting to install corpcor")
    install.packages("corpcor")
    if(require("corpcor")){
      print("corpcor is installed and loaded")
    } else {
      stop("Error: couldn't install corpcor")
    }
  }
  inverse <- X$getinverse()
  if(!is.null(inverse)){
    message("matrix is in memory")
    return(inverse)
  }
  message("inverse is not in memory so the inverse (if it exists) is going to be computed")
  data <- X$get()
  inverse <- pseudoinverse(data, ...)
  X$setinverse(inverse)
  inverse
}
