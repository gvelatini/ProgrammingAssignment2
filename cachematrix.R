## The following two functions create a matrix that has an inverse, calculates the inverse matrix, 
## and caches the inverse matrix so that the inverse does not hyave to be computed ecah time it is needed.

# The operators <<- and ->> are normally only used in functions, 
# and cause a search to be made through parent environments for an existing 
# definition of the variable being assigned. If such a variable is found 
# (and its binding is not locked) then its value is redefined, otherwise 
# assignment takes place in the global environment.

## The function makeCacheMatrix  creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  minv <- NULL
  
  
  set <- function(y) {                    ## store the input matrix and sets the inverse to NULL
    x <<- y
    minv <<- NULL
  }
  get <- function() x                     ## get the input matrix
  setinv <- function(inv) minv <<- inv    ## store the inverse matrix
  getinv <- function() minv               ## get the inverse matrix
  list(set = set, get = get,              ## put all 4 functions in a list
       setinv = setinv,
       getinv = getinv)
}


## The function cacheSolve computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
       

  minv <- x$getinv()                     ## get inverse matrix
  
  if(!is.null(minv)) {
    message("getting cached data")       ## check that inverse matrix is not empty
    return(minv)
  }
  mat <- x$get()                         ## get input matrix
  minv <- solve(mat)                     ## calculate matrix inverse
  x$setinv(minv)                         ## cache inverse
  minv
}