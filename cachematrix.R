## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	  if (!is.matrix(x)) {
			message("The argument of makeCacheMatrix is not a matrix")
			return(NULL)
	  }
        m <- NULL
        get <- function() x
  	setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(get = get,
             getinverse = getinverse,
             setinverse = setinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        data <- x$get()
        p <- x$getinverse()
        if(!is.null(p)) {
                message("Getting cached data")
                return(p)
        }
        p <- solve(data)
        x$setinverse(p)
        ## Return a matrix that is the inverse of 'x'
        p
}
