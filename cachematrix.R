## This program contains functions to calculate the inverse of a matrix and then 
## cache it so that it can be used without recomputing again and again.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ## Method to set the matrix
        set <- function( matrix ) {
                x <<- matrix
                i <<- NULL
        }
        
        ## Method the get the matrix
        get <- function() {
                x
        }
        
        ## Method to set the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        ## Method to get the inverse of the matrix
        getInverse <- function() {
                i
        }
        
        ## Return a list of the methods
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## Just return the inverse if its already set
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculate the inverse using solve
        m <- solve(data)
        
        ## Set the inverse to the object
        x$setInverse(m)
        
        ## Return the matrix
        m
}
