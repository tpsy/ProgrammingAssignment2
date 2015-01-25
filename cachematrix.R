## Matrix inversion is usually a costly computation and their may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. So this code
## uses two functions to do that. When we have a matrix whose the inverse we want, we
## just call the function cacheSolve() with argument being the function makeCacheMatrix().
## The code will check if the inverse of that matrix already exists in the cache,
## and if so it will return the inverse restored. If the inverse is not stored in the
## cache, the code will calculate the inverse of the matrix entered, will store it
## in the cache, then will return the inverse.


## The makeCacheMatrix code is a function that creates a special "matrix" object
## that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inversa) m <<- inversa
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve() function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.


cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
