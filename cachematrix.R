## The pair of makeCacheMatrix and cacheSolve functions
## optimize the calculation of the inverse of a matrix by storing the result
## for later use. This avoids calculating the same result twice, which may be
## time consuming for large matrices.
##
## Example
##
## cm <- makeCacheMatrix(matrix(rnorm(9), 3, 3))
## cacheSolve(cm) # calculation performed
## cacheSolve(cm) # cached value returned

## makeCacheMatrix takes a matrix as its input parameter
## and returns a special matrix object composed of four
## functions in a list:
## get() : returns the underlying matrix
## set(y) : sets or updates the underlying matrix
## getinverse() : returns the inverse of the matrix
## setinverse(inverse) : sets or updates the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes a special matrix created by makeCacheMatrix
## and returns its inverse. If the inverse has been previously
## calculated, the cached value is returned. Otherwise the calculation
## is performed and cached for future use.
## Per the assignment instructions, this assumes a square matrix.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setinverse(inverse)
    inverse
}
