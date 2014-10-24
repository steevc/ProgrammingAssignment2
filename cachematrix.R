## Set of functions to cache the inverse of a matrix
## Test with following
## make a simple matrix
## c=rbind(c(1,-0.25),c(-0.25,1)) 
## Get the cache vector
## q <- makeCacheMatrix(c)
## Get the inverse. After first run will use cache
## cacheSolve(q)

## Makes a list containing functions and the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Gets inverse of matrix using cached version if available

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
