# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of matrix.

#makeCacheMatrix: This function creates a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inversed matrix
#4. get the value of the inversed matrix


makeCacheMatrix <- function(x = matrix()) {
  inversion <- NULL
  set <- function(y) {
    x <<- y
    inversion <- NULL
  }
  
  get <- function() x
  
  setInversion <- function(inverse) inversion <<- inverse
  getInversion <- function() inversion
  
  list(set = set, get = get, setInversion = setInversion, getInversion = getInversion)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inversion <- x$getInversion()
  
  if(!is.null(inversion)) {
    message("Getting cached data")
    return(inversion)
  }
  
  data <- x$get()
  inversion <- solve(data, ...)
  x$setInversion(inversion)
  inversion
}

## Sample run:
##> source("cachematrix.R")
##>
## > matrix(1:4, nrow = 2, ncol = 2)
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1] [,2]
##[1,]    1    3
##[2,]    2    4

## No cache in the first run
## > cacheSolve(m)
##  [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## Retrieving from the cache in the second run
## > cacheSolve(m)
## Getting cached data
# [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
## > 