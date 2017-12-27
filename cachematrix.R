## The following two functions return an inverse matrix of a given matrix. Calculating an inverse matrix is a complicated procedure,
## so caching results of calculating may help save time. The two functions are intended to cache the calculated result on the way in the first function,
## save the calculating time, and return the inverse matrix faster.

## The "makeCacheMatrix" function calculates and caches an inverse of a given matrix.

makeCacheMatrix <- function(x = matrix()) {
    r <- NULL
    set <- function(y){
        x <<- y
        r <<- NULL
    }
    get <- function()x
    setreverse <- function(reverse) r <<- reverse
    getreverse <- function() r
    list(set = set, get = get, setreverse = setreverse, getreverse = getreverse)

}

## The "cacheSolve" function calculate an inverse of the matrix returned by the "makeCacheMatrix" function.
## If the inverse has already been calculated, this retrieves the inverse rom the cache and saves time.

cacheSolve <- function(x, ...) {
    r <- x$getreverse()
    if(!is.null(r)){
        message("getting cashed data")
        return(r)
    }
    data <- x$get()
    r <- solve(data,...)
    x$setreverse(r)
    r
}
