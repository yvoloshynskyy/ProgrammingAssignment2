## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix stores a matrix X in memory
## cacheSolve shows the inverse of a matrix if is in memory or computes the inverse and then shows the inverse

## makeCacheMatrix uses scoping rules and stores matrices in memory

makeCacheMatrix <- function(x = matrix()) {
  # Values of the inverse matrix
  inv <- NULL
  
  # The function sets values of the matrix
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  # The function returns values of the matrix
  get <- function()
  {
    x
  }
  
  # The function stores values of the inverse matrix 
  setinverse <- function(e)
  {
    inv <<- e
  }
  
  # The function returns the inverse matrix
  getinverse <- function()
  {
    inv
  }
  
  # This list represents a special "matrix" object that can cache its inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve uses corpcor, a library that avoids determinants and uses orthogonal descomposition

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(class(x) != "list")
  {
    stop("An argument of the function must be a list!")
  }
  
  inv <- x$getinverse()
  
  if(!is.null(inv))
  {
    message("Getting cached data...")
    return(inv)
  }
  
  # Let's obtain values of the matrix 'x'
  data <- x$get()
  
  # Let's calculate the inverse matrix...
  inv <- solve(data)
  # and store values
  x$setinverse(inv)
  
  ## Let's return a matrix that is the inverse of 'x'
  inv
}
