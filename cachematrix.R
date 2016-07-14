## Cache the Inverse of a Matrix
## The functions store a matrix and caches its invers.

## The function makes a matrix object so the inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
        }
get <- function() x
setInverse <- function(inverse) m <<- inverse
getInverse <- function() m
list(set = set, 
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}


## The function computes the inverse of the matrix create by the makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        mat <- x$get()
        m <- solve(mat, ...)
        x$setInverse(m)
}

