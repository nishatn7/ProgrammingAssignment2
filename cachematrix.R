## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The makeCacheMatrix function creates a  special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
  inv <- NULL ##initializing inverse as null
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv ##get the inverse of the matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...){ 
  inv <- x$getInverse() ## Return a matrix that is the inverse of 'x'
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv  #calculate inverse
}
