##  Name    : cachematrix.R
##  Purpose : The following functions:
##            1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##            2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##               If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##
## makeCacheMatrix: This function retures a list containing a function to 
##                   1. set the value of the vector
##                   2. get the value of the vector
##                   3. set the value of the mean
##                   4. get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  set = function(y) {
    x <<- y
    m <<- NULL
  }
  get = function() x
  setmean <- function(mean) m <<- mean 
  getmean <- function() m
  list(set=set, get=get, 
       setmean=setmean, getmean=getmean)
}

## cacheSolve: The following function calculates the mean of the special "vector" created with the above function. 
##             Check cache to see whether it has already been calculated - 
##             if yes - get it from cache 
##             else - calculate and set the value using setmean function()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
   data <- x$get()
   m <- solve(data, ...)
   x$setmean(m)
   return(m)
}
