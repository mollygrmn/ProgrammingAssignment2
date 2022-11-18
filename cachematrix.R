## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function that set the matrix and its inverse in its environment
## they are a pair of functions that are used to cache the matrix and its inverse
## makecachematrix is used to pass cachesolve
## x <- makeCacheMatrix(matrix(1:8, 2, 4))

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x                 ##function to get matrix
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)       ##function to get inverse
}


## Write a short comment describing this function
##compute and cache the inverse of the matrix
## additional arguments can pass through the solve function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {                 ##checks to see if inverse is null
    message("getting cached data")
    return(inv)               ##return inverse value
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
