makeCacheMatrix <- function(x = matrix()) {
  int <- NULL
  set <- function(y) {
    x <<- y
    int <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) int <<- inv
  getinverse <- function() int
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


cacheSolve <- function(x) {
  int <- x$getinverse()
  if(!is.null(int)) {
    message("getting cached data")
    return(int)
  }
  m <- x$get()
  int <- solve(m)
  x$setinverse(int)
  int
}