## Creates a special list to be consumed by cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) { x <<- y; i <<- NULL }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Returns a matrix that is the inverse of 'x' by either 
##   Calculating it or retrieving from cache
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
