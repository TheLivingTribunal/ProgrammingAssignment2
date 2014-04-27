## This set of functions stores a given square matrix , calculates it's inverse and caches it in a list. 
## When the solve function is called for a stored matrix, the inverse is picked from the same list instead of calculating again.

## This function creates a special list which can get a matrix (square) and calulate and cache it's inverse. 
## It can also be used to overwrite/set a matrix and then calculate and cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function's input is the special list generated from the previous function (makeCacheMatrix) .
## This function returns the cached Inverse of a matrix, if present in the list. Otherwise it calculates the Inverse afresh. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  return(m)
  
}
