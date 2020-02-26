## The two functions, "makeCacheMatrix" and "cacheSolve" take an matrix input 
## create functions used to set and retrieve the matrix and it's inverse.
## "makeCacheMatrix" must be run to create the functions, then cacheSolve solves 
## for the matrix inversion and caches it for later retrieval.

## The function "makeCacheMatrix" takes an argument of a matrix and creates a 
## list of four functions and two objects used in caching a matrix and 
## retrieving.

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setinverse <- function(solve) m <<- solve
                getinverse <- function() m
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## This function cache's the matrix inverse so that it is retrievable with the
## function getinverse(). It also returns the matrix inverse.

cacheSolve <- function(x, ...) {
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
