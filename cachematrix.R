
## The set of functions try to calculate the inverse of a matrix. If the inverse is already calculated previously, the value is retreived from the cache memory.
## makeCacheMatrix sets the list in place

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL                     
        set <- function(y) {                      
                x <<- y
                inverse <<- NULL              
        }
        get <- function() x                           
        setinverse <- function(solve) inverse <<- solve 
        getinverse <- function() inverse             
        list(set = set, get = get,                    
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve matrix calls the inverse function - if null, calculates the inverse - else returns the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inverse <- x$getinverse()
        if(!is.null(inverse)) {                 
                message("getting cached data - matrix inverse")
                return(inverse)
        }
        data <- x$get()                               
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
        
}
