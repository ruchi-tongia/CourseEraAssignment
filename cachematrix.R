git## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## set initial value of inv
  inv <- NULL
  
  set <- function(B) {
    
    ## assign the value of B to A (stored in the parent environment)
    A <<- B
    
    ## assign NULL in inv (stored in the parent environment)
    inv <<- NULL
  }
  ## return the matrix A
  get <- function() A
  
  ## sets the value of the inversed matrix to the inv variable (stored in the parent environment)
  setinverse <- function(inverse) inv <<- inverse
  
  ## return the cached value of inversed matrix A
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Create a function getting matrix A as argument along with other prossible arguments
cacheSolve <- function(A, ...) {
  
  ## assign the value of the cached inverse matrix from cacheMatrix to inv variable
  inv <- A$getinverse()
  
  ## if the value is different than null(meaning it was computated already),return the value of inversed matrix A
  if(!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)
  }
  
  ## assign the value of A to a matrix named mymatrix 
  mymatrix <- A$get()
  
  ## store the value of inversed matrix A to inv variable
  inv <- solve(mymatrix)
  
  ## cache the inv variable for further usage and printing inv
  A$setinverse(inv)
  inv
}
