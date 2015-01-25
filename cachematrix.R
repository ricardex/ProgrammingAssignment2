## This function creates a special matrix which is really a list containing a function to 
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3  set the value of the inverse 
## 4 get the value of the inverse

makeInverseMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(z) {
    x <<- z
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function does the actual computation of the inverse, and returns it
## every time it just doesnt calculate it every time only the first time then
## it caches it and returns the result from the cache for every subsequent call

cachesolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
