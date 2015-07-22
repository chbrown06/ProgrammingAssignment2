## These functions cache a square matrix and its inverse.  If the inverse has been
## calculated then it will be retrieved from the cache using getinverse. Otherwise,
## the inverse will be calculated and cached using setinverse.  The inverse will
## be returned in both cases.

## This function, makeCacheMatrix, creates an object to store the martrix
## and cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  t <- NULL
  set <- function(y){
    x <<- y
    t <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) t <<- solve
  getinverse <- function() t
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This functions, cacheSolve, first checks to see if the inverse of the matix
## passed to it has been calculated.  If the inverse has been calculated it
## returns the inverse.  Otherwise, the inverse is calculated and cached via
## setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  t <- x$getinverse()
  if(!is.null(t)){
    message("getting cached data")
    return(t)
  }
  data <- x$get()
  t <- solve(data, ...)
  x$setinverse(t)
  t
}
