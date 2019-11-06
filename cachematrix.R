## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function creates an object that stores a matrix and caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y) {
                
                x <<- y
                
                inverse <<- NULL
                
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inverse <<- inverse
        
        getinverse <- function() inverse
        
        list(set = set, get = get,
             
             setinverse = setinverse,
             
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function returns the inverse of the matrix using the solve function with the difference that it uses the object's cached inverse.
## If there was no cache, the result is saved in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        
        if(!is.null(inverse)) {
                
                message("getting cached data")
                
                return(inverse)
                
        }
        
        data <- x$get()
        
        inverse <- solve(data,...)
        
        x$setinverse(inverse)
        
        inverse
}
