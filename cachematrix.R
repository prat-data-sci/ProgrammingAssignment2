## Here we will be computing the inverse matrix of an invertible matrix. 
## However, Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of 
## a matrix rather than computing it repeatedly. 
## We shall write a pair of functions that will be able to cache these potentially time-consuming computations. 


## This following function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {              # input x will be a vector
  
  invmat<- NULL         #  invmat will be our 'inverse matrix' and it's reset to NULL every 
  #  time makeCacheMatrix is called
  
  #  Note these next four functions are not run when makeCacheMatrix is called.
  #  instead, they will be used by cacheSolve() to get values for x or for
  #  invmat (solveinvmat) and for setting the values of x or for inverse matrix.  
  #  These are usually called object 'methods'
  
  set <- function(y) {          # takes an input matrix
    x<<-y                       # saves the input matrix
    invmat<<-NULL               # resets the inverse matrix to NULL, basically what happens when a new object is generated.
  }
  
  get <- function() x           # this function returns the value of the original vector
  
  setsolveinvmat <- function(solveinvmat) {invmat<<-solveinvmat}
  # this is called by cacheSolve() during the first cacheSolve()
  # access and it will store the value using superassignment
  
  getsolveinvmat <- function() invmat
  # this will return the cached value to cacheSolve() on
  # subsequent accesses
  
  list(set=set, 
       get=get, 
       setsolveinvmat=setsolveinvmat, 
       getsolveinvmat=getsolveinvmat)
  #  OK, this is accessed each time makeCacheMatrix() is called, 
  #  that is, each time we make a new object.  This is a list of 
  #  the internal functions ('methods') so a calling function
  #  knows how to access those methods.
  
}


## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {            # the input x is an object created by makeCacheMatrix
  
  
  invmat <- x$getsolveinvmat()              # accesses the object 'x' and gets the value of the matrix
  
  if(!is.null(invmat)) {                    # if matrix was already cached (not NULL) ...
    message("getting cached data")          # ... send this message to the console
    return(invmat)                          # ... and return the inverse matrix ... "return" ends
  }
  
  data <- x$get()                           # we reach this code only if x$getsolveinvmat() returned NULL
  
  invmat <- solve(data, ...)                # if invmat was NULL then we have to calculate the inverse matrix
  
  x$setsolveinvmat(invmat)    # store the calculated inverse matrix value in x (see setsolveinvmat() in makeCacheMatrix
  
  invmat         ## Return a matrix that is the inverse of 'x' to the code that called this function
  
}
