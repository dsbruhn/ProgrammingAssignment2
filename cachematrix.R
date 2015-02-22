## These functions enable you to cache the inverse of a matrix
## instead of computing the same inverse process multiple times.
## Basis for this solution from mean example in assignment by Dr. Peng.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Initialize matrix cache
        m <- NULL

        # define functions to set and get values in the cache
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m

        # returns list of functions to interact with the matrix cache
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        #Attempt to get the inversed matrix from the cache
        m <- x$getinverse()

        # if a cached version of the matrix hasn't been calculated already
        # then calculate the inverse and store it in the matrix cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)

        #return the cached inverted matrix
        m
}

