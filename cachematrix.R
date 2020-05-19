## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to set and get the value of the matrix
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setminverse = setinverse,
         getinverse = getinverse)
}

## The function calculates the inverse of the special "matrix"
## created with the above function. It first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- inverse(data, ...)
    x$setinverse(m)
    m
}
