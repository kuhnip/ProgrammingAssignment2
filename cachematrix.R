## The following functions allow the inverse of a matrix to be cached when
## first computed, and retrieved when needed rather than a calculated
## repeatedly. (Assumes matrix is always invertible.)

## makeCacheMatrix(x): creates special "matrix" object to store the matrix 'x'
##   and the inverse matrix of 'x'.
##   Object has methods to set and retrieve both the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL         # matrix inverse not calculated at object creation
  
  # set value of matrix
  set <- function(y) {
    x <<- y           # store matrix 'y' as 'x' in matrix object environment
    inv <<- NULL      # reset matrix inverse as matrix has changed
  }
  
  # retrieve the value of matrix
  get <- function() x
  
  # set (cache) value of matrix inverse in matrix object 
  setinv <- function(inverse) inv <<- inverse   # store 'inverse' as variable
                                                # 'inv' of matrix obj. env.
  
  # retrieve the value of matrix inverse
  getinv <- function() inv
  
  # return matrix object as list of these four functions above
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve(x): computes the inverse of the special "matrix" object returned
##   by makeCacheMatrix above. If the inverse has already been calculated,
##   then the cached inverse is retrieved.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()     # retrieve value of inverse cached in matrix object
  
  if(!is.null(inv)) {   # if the inverse is cached... (i.e. not NULL)
    message("getting cached inverse")
    return(inv)         # ...return the cached value for the inverse
  }
  
  # inverse is not cached - calculate the inverse and cache in the matrix obj
  mat <- x$get()        # retrieve the matrix from the matrix object
  inv <- solve(mat, ...)  # calculate the inverse of the matrix
  x$setinv(inv)         # cache the inverse in the matrix object
  inv                   # return the inverse
}