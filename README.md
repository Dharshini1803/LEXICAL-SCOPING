## This function creates a sppecial "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
       x <<- y
       inv <<- NULL 
}
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by makeCacheMatrix above.
## If the inverse has already been calculated, then it should retrieve the inverse from the cache.

cacheSolve <- functiom(x, ...) {
## return a matrix that is inverse of 'x'
inv <- x$getInverse()
if(!is.null(inv)) {
       return(inv)
}
mat <- x$get()
inv <- solve(mat, ...)
x$setInverse(inv)
inv
}

