## Below are two functions that are used to create a special object that stores a matrix 
## and cache's its inverse. 

## This function, makeCacheMatrix creates a special "Matrix", which is really a list containing 
## a functins to
## 1. set the value of the matrix 
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

  xInv <- NULL
  
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(inverse) xInv <<- inverse
  
  getInv <- function() xInv
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The following function calculates inverse  of the special "Matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the invers from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the invers in the cache 
## via the setInv function.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  xInv <- x$getInv()
  
  if(!is.null(xInv)) {
    message("getting cached Matrix Inverse")
    return(xInv)
  }
  
  data <- x$get()
  xInv <- solve(data, ...)
  x$setInv(xInv)
  
  xInv
}
