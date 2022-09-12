## Programming Assignment No. 2: Catching the Inverse of a Matrix

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # Set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #Get the matrix
        get <- function() x
        #Set the inverse of a matrix
        setinverse <- function(solve) inv <<- solve
        # Get the inverse of a matrix
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("retrieving the inverse from the cache")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
