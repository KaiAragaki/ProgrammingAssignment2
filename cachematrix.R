# Contains a function that create a special matrix object with functions for setting and getting contents and inverses. Also contains a function that can retrieve a cached inverse, or, if unavailable, can solve for one.

# Takes a matrix and produces an object with several functions available to it: - Set, to set matrix contents; Get, to get matrix contents; getInverse, to get the inverted matrix; and setInverse, to set the inveted matrix
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
        }
        get <- function() x
        getInverse <- function() i <<- solve(x)
        setInverse <- function(inv) i <<- inv
        list(set = set, get = get, getInverse = getInverse)
}

# Solves for the inverse of the matrix, unless there is already a cached inverse - at which point it retrieves it without solving redundantly.
cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("Data was cached, retrieving")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
