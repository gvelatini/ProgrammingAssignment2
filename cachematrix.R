## The following two functions create a matrix object for a matrix that has an inverse.  They calculate the inverse matrix, 
## and cache the inverse matrix so that the inverse does not have to be computed ecah time it is needed.
##
## The function makeCacheMatrix  creates a special "matrix" object that can cache its inverse. 
## The object created by makeCacheMatrix contains 4 functions: 
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


## The function cacheSolve checks to see if an inverse matrix
## has already been cached.   If it has alreday been cached, it 
## simply returns the cached inverse matrix. If not, it computes 
## the inverse of the matrix returned by makeCacheMatrix 
## above and caches the new inverse.

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