## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  my_inverse <- NULL
  set_Matrix <- function(y) {
    x <<- y
    my_inverse <<- NULL
  }
  get_Matrix <- function() x
  setInverse <- function(inverse) my_inverse <<- inverse
  getInverse <- function() my_inverse
  list(set_Matrix = set_Matrix,
       get_Matrix = get_Matrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  my_inverse <- x$getInverse()
  if (!is.null(my_inverse)) {
    message("getting cached data")
    return(my_inverse)
  }
  mat <- x$get_Matrix()
  my_inverse <- solve(mat, ...)
  x$setInverse(my_inverse)
  my_inverse
}
