##
##
##  RPROG-030 Programming Assignment # 2
##	P.Boissiere
##	July 24, 2015
##


## makeCacheMatrix: creates a vector/list of functions and matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {          
    x <<- y                       
    inv <<- NULL                
  }
  get <- function() x               
  
  setsolve <- function(solve) inv <<- solve      
  
  getsolve <- function() inv      
  
  list(set = set, get = get, setsolve = setsolve,  getsolve = getsolve)
}

## cacheSolve: computes the inverse of the matrix, or returns existing inverse
cacheSolve <- function(x, ...) {
  
  inv <- x$getsolve()              
  
  if(!is.null(inv)) {                  
    message("getting cached data")      
    return(inv)                  
  }
  
  data <- x$get()              
  
  inv <- solve(data, ...)       
  
  x$setsolve(inv)
  
  inv                                   
}
