## The following two functions calculate and return an inverse of a given matrix.
## The first function cache the calculation and this helps the second function calculate less in returning the inverse.

## The "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse.

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


## The "cacheSolve" function calculates an inverse of the matrix returned by the "makeCacheMatrix". 
## If the inverse has already been calculated, this retrieves the inverse form that function.

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
