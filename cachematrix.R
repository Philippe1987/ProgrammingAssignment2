## Functions that invert a non singular square matrix but saves time by looking for an existing calculation on the cache memory

## List of useful functions to set and get a matrix and its inverse from the cache memory

makeCacheMatrix <- function(x = matrix()) {
      
        x_inverse <- NULL
        
        set <- function(y) {
                x <<- y
                x_inverse <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) x_inverse <<- inverse
        
        getinverse <- function() x_inverse
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of a matrix but firt looks into the cache memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        x_inverse <- x$getinverse()
        if(!is.null(x_inverse)) {
                message("getting cached data")
                return(x_inverse)
        }
        data <- x$get()
        x_inverse <- solve(data, ...)
        x$setinverse(x_inverse)
        x_inverse
}
