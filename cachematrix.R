## The functions "makeCacheMatrix" and "cacheSolve" help 
## to cache inverse matrix to avoid duplicated computation.

## makeCacheMatrix return an object which support cache
## an inversed version of matrix.

makeCacheMatrix <- function(x = matrix()) {
    inversed_m <- NULL
    set <- function(y) {
        x <<- y
        inversed_m <<- NULL
    }
    get <- function() x
    setinverse <- function(i_m) {
        inversed_m <<- i_m
    }
    getinverse <- function() inversed_m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## cachSolve returns the cached inversed matrix if it exist.
## otherwise calculate it and cache it in parent env.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i_m <- x$getinverse()
    if(!is.null(i_m)) {
        return(i_m)
    }
    data <- x$get()
    i_m <- solve(data)
    x$setinverse(i_m)
    i_m
}

