## The first function, makecahematrix creates a matrix , which is a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makecachematrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function calculates the inversr of the matrix created with the above function. First of all
## it checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and
## skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setsolve function.


cachesolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
