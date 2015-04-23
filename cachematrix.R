## Two functions will be created
## makeCacheMatrix will take a matrix and cache the results
## cacheSolve will return the cache if already solved or solve it

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    # inv used to store the cached inverse
    inv <- NULL
    
    # set will set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # get will get the matrix
    get <- function() x
    
    # setinverse will set the inverse
    setinverse<- function(inverse) inv <<-inverse
    
    # getinverse will get the inverse
    getinverse <- function() inv
    
    # list of available options
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache.  This is noted by the message.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # do a get on the matrix to find cached inverse store it in inv
    inv <- x$getinverse()
    
    # check to see if inv is not null meaning cached inverse exists
    if (!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    } 
    
    # since cached inverse does not exist, we must calc it
    data <- x$get()
    
    # use sovlve to actually get the inverse
    inv <- solve(data, ...)
    
    # make sure to cache the results
    x$setinverse(inv)
    
    #return results
    inv
}