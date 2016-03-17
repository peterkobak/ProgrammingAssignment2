## The functions in cachematrix.R work together to cache the
## inverse of matrix, so that the inverse does not need to be
## taken more than once.

## makeCacheMatrix prepares a matrix to be cached by associating
## it with several functions, namely set, get, setinverse,
## and getinverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve first checks to see if the inverse of the matrix
## that is being fed to it exists in the cache. If so, it 
## returns said inverse. If the inverse does not exist in the
## cache, it will calculate the inverse of the given matrix
## and store it in the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
