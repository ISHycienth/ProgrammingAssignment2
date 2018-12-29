## CACHING INVERSE OF A MATRIX:
## Matrix inversion is usually a costly computation, and there may be some benefits to 
## caching the inverse of a matrix rather than compute it repeatedly. Below is a pair 
## of functions that are used to create a special object that stores a matrix and caches its inverse.

## The following function creates a special "matrix" object, which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_Inv <- function(inv2) inv <<- inv2
  get_Inv <- function() inv
  list(set = set,
       get = get,
       set_Inv = set_Inv,
       get_Inv = get_Inv)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_Inv()
        if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$set_Inv(inv)
        inv
}
