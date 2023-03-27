## To write a pair of functions that cache the inverse of a matrix.


## To create a special "matrix" object that can cache its inverse which will be used later

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inver <<- solve
  getInverse <- function() inver
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## To compute the inverse of the outcome created by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inver <- x$getInverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setInverse(inver)
  inver
}
