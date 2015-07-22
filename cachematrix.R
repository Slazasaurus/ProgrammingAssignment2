## Creates an ADT of matrix with cacheing of inverse
## Takes a matrix, returns a list object

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse 
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function checks if inverse has been cached,
## If not, it calculates it and stores it
## Else, simply returns stored. Must take special matrix object as supra

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}
