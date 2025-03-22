## In this excercise we have two functions: makeCacheMatrix and cachesolve
## these functions are used to obtain the inverse of a matrix. to avoid 
## recalculating it and spending computational resources, we store the inverse
## in a cache memory, wich can be retrieved when needed. 

## the makeCacheMatrix functions creates a structue that stores a matrix 
## and its inverse in cache memory 

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


## cacheSolve calculates the inverse of the matrix if it's not already cached 



cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    
    ## Return a matrix that is the inverse of 'x
    inv
}
