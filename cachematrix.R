## This function returns an obj of "a list of functions" 

makeCacheMatrix <- function(x = matrix()) {
  M <- NULL;  ##initialize
  
## - set(), set new parameters into makeCacheMatrix, and set M to NULL
  set <- function(y) {
    x <<- y
    M <<- NULL
  }
## - get(), return the original parameters fed into makeCacheMatrix
  get <- function() {x}
## - setMatrix(), set the M variable inside makeCacheMatrix
  setMatrix <- function(matrix) {M <<- matrix}
## - getMatrix(), returns the M variable inside makeCacheMatrix
  getMatrix <- function() {M}
##Returns an obj of "a list of functions" 
  list(set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)  
  
}

## This function computes inverse of x, if M in the given object is NULL
## or just retrieves previous computed result

cacheSolve <- function(x, ...) {
  ##check whether M in the given x object isn't NULL (x was created by makeCacheMatrix)
  m<-x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")  ##if previous result exists,no computation is needed
    return(m)
  }
  
  ##if m is NULL, then compute inverse of x, and then store it in M of the x object
  ##in order to compute, we need to retrieve the parameters fed into makeCacheMatrix
  dataFed <- x$get()
  m <- solve(dataFed)  ## update our m variable
  x$setMatrix(m)       ## store the computed result in M of the x object
  return(m)
}
