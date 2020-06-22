#formation of empty matrix
makeCacheMatrix <- function(x = matrix()) {
  #assignment of y to x in this environment
i <- NULL
set <- function(y) {
  x <<- y
  i <<- NULL
}
get <- function() x
#inverse is i in this environmental context
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
#lists the set, get, setinverse, getinverse values defined previously
list(set = set,
     get=get,
     setinverse = setinverse,
     getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  #assign i to getinverse from x
  if (!is.null(i)) {
    message("retrieving cached data")
    return(i)
    #if i already has a value, retrieve it and print
  }
  data <- x$get()
  #otherwise, retrieve get from x
  i <- solve(data, ...)
  #solve i from data
  x$setinverse(i)
  #report i from solve
  i
}
