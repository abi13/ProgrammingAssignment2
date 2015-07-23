## Put comments here that give an overall description of what your
## functions do

## this fuction creates a cache (implemented internally as a list)
##   of a matrix and a matrix inverse
## the following methods are implemented:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## compute a matrix inverse by taking advantage of a cache:
## if the function is called repetitevely on the same matrix, the cache is being used to retrieve 
##   the already computed matrix inverse from the cache;
##   this could save us a lot of time esp. for big matrices

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
