## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      ## We initialize the value of m to null
      ## We set the value of the matrix

      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      ## we get the value of the matrix
      get <- function() x
      ## we set the value of the inverse matrix
      setInverse <- function(solve) m <<- solve
      ## we get the value of the inverse matrix
      getInverse <- function() m
      ## we make a list that caches all of these values
      list(set = set, get=get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      ## we try to get m the inverse of x
      m <- x$getInverse
      ## if we m has a value (i.e. we had previously cached the value of the inverse)
      if(!is.null(m)) {
            message("getting cached matrix")
            ## we return the cached value
            return(m)
      }
      ## if the value of m is null then we simply compute it
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m 
}
