## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function.

# This function creates a special "matrix" object that can cache its inverse.

# So, we have a matrix x. We need to keep a cache of its inverse. In keeping
# with the example given for vectors, we'll make four functions, for
# setmat, getmat, setinv, and getinv.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  mat <- x
  setmat <- function(y){
    mat <<- y
    inv <<- NULL
  }
  getmat <- function(){
    mat
  }
  setinv <- function(passedinv){
    inv <<- passedinv
  }
  getinv <- function(){
    inv
  }
  list(setinv = setinv, 
       getinv = getinv,
       setmat = setmat,
       getmat = getmat)
}


## Write a short comment describing this function.

#  This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then the cachesolve should retrieve the inverse 
# from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    return(inv)
  }
  mat <- x$getmat()
  inv <- solve(mat, ...)
  x$setinv(inv)
  
  inv
}

# And because the rest of the assignment was kinda bland, here's a 
# (=◕ᆽ◕ฺ=)
