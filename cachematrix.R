## Put comments here that give an overall description of what your
## functions do

## takes and store a matrix and its inversed for the second function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    y <<- x
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## First search if an inversed matrix already exist. If not, inverse the matrix using solve function.

cacheSolve <- function(x, ...) {
  inv<- x$getinverse()
  if(!is.null(inv)){
    message("getting cache data")
    return (inv)
  }
  matrix <- x$get()
  inv <- solve(matrix,...)
  x$setinverse(inv)
  inv
}
