## Nicholas Stine
## 07/27/2020
## Programming Assingment 2

## Generate the "special" matrix in which the value and result of inversion can be cached
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(arg_inv) inv <<- arg_inv
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return the cached result of inversion, or (if the result has not yet been calculated) then solve, cache and return
## the newly inverted matrix
cacheSolve <- function(x, ...) {
  data <- x$get()
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  inv <- solve(data)
  x$setinv(inv)
  inv
}
