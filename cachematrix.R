##  Pair of functions that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object
# that can cache its inverse, which is really a list
# containing a function to:
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<-y
        inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(new_inverse) inverse <<- new_inverse
    get_inverse <- function() inverse
    list(set = set, get = get,
         set_inverse = set_inverse, get_inverse = get_inverse)
}

## cacheSolve: This function computes the inverse of the special
# "matrix" returned by makeCacheMatrix above. If the inverse has
# already been calculated (and the matrix has not changed), then
# the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(x, ...)
    x$set_inverse(inverse)
    inverse
}
