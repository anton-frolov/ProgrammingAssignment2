## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize properties
  inv <- NULL
  
  ## set and get of matrix 
  set <- function(matix) {
    x <<- matrix
    inv <<- NULL
  }
  get <- function(){ 
    x 
  }
  
  ## set and get inverse of matrix
  setInverse <- function(inverse){
    inv <<- inverse
  }
  getInverse <- function(){
    inv
  }
  
  ## list of the methods
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## check into cache 
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    ## if there is, then return it
    return(m)
  }
  ## if "m" is NULL, get original matrix
  data <- x$get()
  ## calculate inverse  
  m <- solve(data, ...)
   
  ## set inverse to cache
  x$setInverse(m)
  
  ## return result
  m
}
