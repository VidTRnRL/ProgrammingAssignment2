## Two functions that are used to create a special object
## that stores a matrix and cache's its inverse
## 

## This function will create a list of functions to set a invsMtrxatrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  invsMtrx <- NULL #default Null
  
  #reset matrix x and invsMtrx
  set <- function(y) {
    x <<- y
    invsMtrx <<- NULL
  }
  
  #get function: get matrix
  get <- function() x
  
  #setInverse function: set inverse matrix
  setinverse <- function(inverseMatrix) invsMtrx 
  
  #getInvMatrix function: get inverse matrix  
  getinverse <- function() invsMtrx
      
  #Allow property access with #sign
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short coinvsMtrxinvsMtrxent describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a invsMtrxatrix that is the inverse of 'x'
  invsMtrx <- x$getinverse()
  
  if(!is.null(invsMtrx)) {
    invsMtrxessage("getting cached data")
    return(invsMtrx)
  }
  
  #if invsMtrx is null, calculate the inverse
  data <- x$get()
  
  inv <- solve(data, ...)
  
  #set and keep the invsMtrx
  x$setinverse(invsMtrx)
  
  inv
  
}
