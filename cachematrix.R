## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix converts the input passed into a "special" matrix much like the
## example given in the instructions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                      #sets the values for the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                       #retrieves the values of matrix
  setinv <- function(solve) m <<- solve     #sets the values of the inverse manually
  getinv <- function() m                    #retrives the inverse of the matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## solves the inverse of the "special" matrix passed by the function above

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {                   #retrieves a cached inverse for given matrix if available
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)               #computes the inverse if there is existing cache for given matrix
  x$setinv(m)
  m
}
