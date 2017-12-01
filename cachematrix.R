## To avoid repeated computation of the inverse of a matrix. This pair of functions
## chaches the inverse, so that it can be retrieved when needed

## makeCacheMatrix works analogously to the make_vector function, 
## except that the inverse of a matrix rather than the mean of a vector is cached.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve  <- function(solve ) m <<- solve 
  getsolve  <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The following function checks if the matrix has already been inversed ("solve"d), 
## if this is the case, it returns the cached value with a message "getting cached data". 
## Otherwise it computes the inverse from the matrix and sets it in the cached data.     

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data  <- x$get()
  m     <- solve(data, ...)
  x$setsolve(m)
  m
}


