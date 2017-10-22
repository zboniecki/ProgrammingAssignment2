## MakeCacheMatrix creates a matrix
## cacheSolve calculates an inverse of that matrix and caches the result
## if run a second time it just returns the cached result

## makeCacheMatrix creates a matrix; default is a 2 by 2 matrix with 4, 2, 7, 6. Then sets inv_cache as NULL

makeCacheMatrix <- function(x = matrix(c(4, 2, 7, 6),2,2)) {
  inv_cache <- NULL
  set <- function(y) {
    x <<- y
    inv_cache <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv_cache <<- solve
  getsolve <- function() inv_cache
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve returns inverse of matrix created by makeCacheMatrix then stores value in inv_cache, when run a second time it 
## will just retrieve the already calculated value from inv_cache
##Note: save makeCacheMatrix to a value first eg: x <- makeCacheMatrix((matrix(c(8,3,2,4,5,6,7,8,5),3,3))) then run cacheSolve(x)

cacheSolve <- function(x, ...) {
  inv_cache <- x$getsolve()
  if(!is.null(inv_cache)) {
    message("getting cached data")
    return(inv_cache)
  }
  data <- x$get()
  inv_cache <- solve(data, ...)
  x$setsolve(inv_cache)
  inv_cache
}
