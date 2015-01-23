## A set of functions to solve inverse matrix. Uses caching mechanism
## for a sake of performance
##
## Example of use:
##
##    # create a matrix m and make a cached matrix mat out of it
##
##    > m <- matrix(1:4, nrow = 2, ncol = 2)
##    > mat <- makeCacheMatrix(m)
##
##    # to solve inverse matrix
##
##    > cacheSolve(mat)
##         [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5
##

## function: makeCacheMatrix(x)
##
## Creates a cache matrix, which can store the inverse matrix as well
##
## parameters: x = matrix to be cached. If omitted, empty matrix is created
##
## return value: a list of functions (set, get, setinverse, getinverse) to
##               to access cache
##
## NOTE: while you can access local functions (functions defined inside this 
##       function) one should note that e.g. getinverse() does  not return 
##       inverse matrix unless it has been solved separately by cacheSolve()
##
makeCacheMatrix <- function(x = matrix()) {
  
  # cache object to store inverse matrix
  c_inv <- NULL
  # cache object to store matrix
  c_mat <- x
  
  # set matrix into cache, reset inverse matrix
  set <- function(y) {
    c_mat <<- y
    c_inv <<- NULL
  }
  
  # get cached matrix
  get <- function() {
    c_mat
  }
  
  # set inverse matrix into cache
  setinverse <- function(inv) {
    c_inv <<- inv
  }
  
  # get cached inverse matrix
  getinverse <- function() {
    c_inv
  }
  
  # create a list functions and return it
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## function: cacheSolve(x)
##
## Solves inverse matrix of x.
##
## parameters: x = cached matrix created by makeCacheMatrix()
##
## return value: inverse matrix of x
##
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  
  # get inverse matrix of x
  m <- x$getinverse()
  
  if(!is.null(m)) {
    # return pre calculated inverse matrix if it has been cached
    return(m)
  }
  
  # get non inverted matrix
  data <- x$get()
  
  # solve inverse matrix
  m <- solve(data)
  
  # store solved inverse matrix in to cache for later use
  x$setinverse(m)
  
  # return inverse matrix
  m
}
