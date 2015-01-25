## The following two functions are used to cache a matrix and its inverse. 
## By calling the second function (cacheSolve), the matrix inverse is returned without performing the 
## calculation again simply by using the cached results from the makeCacheMatrix function. 

## The makeCacheMatrix function is essentially a list to: 
### a.) set and get the value of a matrix, 
### b.) set and get the value of the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function () x
  setinverse <- function(solve) i <<- solve 
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}

## The cacheSolve function is used to return the inverse of a matrix. However, 
## if the inverse has already been calculated and cached using the MakeCacheMatrix 
## function, the cacheSolve function simply returns the cached result rather 
## than recompute the result. 


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached matrix inverse")
    return(i)
  }
  data <- x$get()
  i <- inverse(data, ...)
  x$setinverse(i)
  i   ## This returns a matrix that is the inverse of 'x' using the cached version,
      ### if it is available, or recomputing, if theh original matrix has changed.
}

