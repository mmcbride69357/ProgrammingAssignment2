#####################################################################################################
## Purpose:  Returns an inverted matrix given a square invertible matrix, and caches the result.
##
##  Example:
##
##  > source("cachematrix.R")
##  > m = matrix(1:4, 2, 2)
##  > cacheSolve(makeCacheMatrix(m))
##
##  Result:
##
##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

# -----------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix())
{
     ## Creates an object that contains a list and functions that:
     # - Set the value of the matrix
     # - Get the value of the matrix
     # - Set the value of the inverted matrix
     # - Get the value of the inverted matrix     
     
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setmatrix <- function(matrix) m <<- matrix
     getmatrix <- function() m
     list(set = set, get = get,
          setmatrix = setmatrix,
          getmatrix = getmatrix)     
}

# -----------------------------------------------------------------------------

cacheSolve <- function(x, ...) 
{
     ## Creates an object that:
     # - Takes a matrix and returns the matrix inverted.
     # - Stores the inverted matrix in cache.
     # - Returns the cached matrix the next time the function is called to avoid recomputation.
     
     m <- x$getmatrix()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setmatrix(m)
     m     
}

# -----------------------------------------------------------------------------
