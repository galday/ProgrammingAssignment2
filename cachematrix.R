## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix receives a matrix as an argument.
## Then it checks if the argument is a matrix. Otherwise the function ends.
## makeCacheMatrix returns a list with three functions: get (returns the matrix passed as an argument),
##							setinverse (receives a matrix and sets the object m)
##							getinverse (returns m, the inverse)
##cachesolve, receives an object created from last function (a list with the three functions previously described)
## Then it calls the function getinverse. If a NULL value is not returned, it means that the inverse has already been obtained
## and it is in memory so the value returned by this function is returned as the inverse matrix. 
## However if a NULL value is returned, the function calculates the inverse with solve() and calls the function setinverse to 
## store its value in the cache. Following the instructions it is not checked that the original matri has an inverse matrix.

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
        p <- x$getinverse()
        if(!is.null(p)) {
                message("Getting cached data")
                return(p)
        }
        data <- x$get()
        p <- solve(data)
        x$setinverse(p)
        ## Return a matrix that is the inverse of 'x'
        p
}
