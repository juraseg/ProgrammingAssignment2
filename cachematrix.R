## These functions perform caching of results of matrix inverse operation

## The function creates special "matrix" object which caches it's inverse

makeCacheMatrix <- function(x = matrix()) {
    # initialize inverse as NULL
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        # set inverse to NULL when changing value of x
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inv) {
        inverse <<- inv
    }
    getinverse <- function() {
        inverse
    }
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The function returns the inverse of given matrix,
## if the inverse was already calculated before - it returns it from cache, 
## otherwise calculates it, saves to cache, and returns the result

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting from cache")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
