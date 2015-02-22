## Caching the Inverse of a Matrix

## Creates a list of functions that can cache the inverse of a matrix

makeCacheMatrix <- function (x = matrix ()){
  m <- NULL
  
  set <- function (y){
    x <<- y
    m <<- NULL
  }
  
  get <- function () x
  setInverse <- function (solve) m <<- solve
  getInverse <- function () m
  list (set = set, get = get, 
        setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of the matrix returned
## by makeCacheMatrix(), unless the inverse has already been calculated
## in which case it retrieves it from the cache.

cacheSolve <- function (x = matrix (), ...){
  m <- x$getInverse ()
  if (!is.null(m)){
    print ("getting cached data")
    return (m)
  }
  
  matrix <- x$get()
  m <- solve (matrix, ...)
  x$setInverse (m)
  m
}
