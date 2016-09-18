## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
