## The CacheSolve function calculates the inverse of the special 
## "matrix" created with the makeCacheMatrix function. CacheSolve function  
## first checks to see if the inverse has already been calculated and stored in the cache. 
## If it finds it, it skips the calculations otherwise
## it calculates the inverse of the matrix and sets the value of the inverse in the cache

## The following function returns a list of basic action functions on the Matrix 
## Set, Get, Set inverse and Get inverse

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



## The following function calculates the inverse of the special 
## "matrix" created with the makeCacheMatrix function. 
##

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
