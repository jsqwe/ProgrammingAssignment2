## Create a special object makeCacheMatrix that stores an invertible matrix
## and cache its inverse

## Returns a list containing a function to set the value of a matrix, get the
## value of a matrix, set the value of the inverse, get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Solve the inverse of the special matrix object, makeCacheMatrix. 
## If the inverse has already been calculated, return the cached inverse. 
## Otherwise, solve for the inverse and cache the value of the inverse 
## with setinverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
