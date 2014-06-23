## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  # should not call the setinv() function explicitly as it will give erronious outcome on cacheSolve
  
  setinv <- function(inverse){
    inv <<- inverse
    inv <- NULL
  } 
  
  getinv <- function() inv
  list(x=x, inv=inv, set = set, get = get,
       setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}

# ================================================================
                
                  # USAGE of the code ##

# source("<path to the downloaded source file>")
# x <- matrix(c(4,2,4,3,8,4,6,2,5,32,5,8),ncol=3,nrow=3); // sample matrix
# a <- makeCacheMatrix(x);
# cacheSolve(a);
# a$get()
# a$getinv()
# cacheSolve(a);
# cacheSolve(a);
# setinv(a);
# cacheSolve(a); 

## ==============================================================

