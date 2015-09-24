## The following two functions create a matrix that has an inverse, calculates the inverse matrix, 
## and caches the inverse matrix so that the inverse does not have to be computed ecah time it is needed.
##
## The function makeCacheMatrix  creates a special "matrix" object that can cache its inverse. 
## The object cretaed by makeCacheMatrix contains## 4 functions. 
## "$set()" sets the input matrix values, 
## "$get()" retrieves the input matrix values, 
## "$setinv()" sets the inverse matrix values, 
## "$getinv()" retrieves the inverse matrix values, 

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