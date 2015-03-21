## functions allow user to cache the inverse of a square matrix

## functions based on Roger Peng's Caching the Mean of a Vector

## function "makeCacheMatrix" allows user to set & get  the values of square matrix
## and set & get the value of the inverse of the initially set square matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## function "cacheSolve" checks if the inverse of the square matrix set in "makeCacheMatrix"
## is already calculated. If it is, it get the inverse, else it calculates the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
