## The following two functions is to cache the inverse of a matrix.

## My makeCacheMatrix_function creates a special "matrix", which is a 
## list containing a function to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## My cacheSolve_function computes the inverse of the special "matrix" 
## returned by my makeCacheMatrix_function above. It first checks if 
## the inverse has already been calculated, then cacheSolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
