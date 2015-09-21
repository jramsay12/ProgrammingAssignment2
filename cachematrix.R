## Matrix inversion is usually computationally costly and there may be
## benefit to caching the inverse of a matrix rather than computing
## it repeatedly. These functions cache the inverse of a matrix

## makeCacheMatrix function creates a "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    get <- function() x
    set_matrix_inverse <- function(matrix_inverse) i <<- matrix_inverse
    get_matrix_inverse <- function() i
    list(get = get, 
         set_matrix_inverse = set_matrix_inverse,
         get_matrix_inverse = get_matrix_inverse)
}

## cacheSolve function computes the inverse of the "matrix" returned
## by the makeCacheMatrix function - if the inverse has already been
## calculated and the matrix is unchanged, then cacheSolve retrieves
## the inverse from cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$get_matrix_inverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$set_matrix_inverse(i)
    i
}
