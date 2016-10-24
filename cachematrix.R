# this program to claculate and save in cache the Inverse of complicated matrices

# We have to create a caching function to pass a matrix as a argument. it Returns a vector 
# of getinverse(s) and setinverse(s) for the matrix to be stored in the cache 
# to serve as a placeholders for the matrix inverse calculation.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Calculates the inverse of a matrix, or retrieves from cache
# the previously calculated inverse 
# passed as an argument from the function makeCacheMatrix().

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setinverse(m)
  m
}