## Two functions, one is to create the cache and 
## the other is to populate and/or fetch the inverse from the cache

## The function below creates a strucute to hold the matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL #initialize inverse with null
        set <- function(y) {
                x <<- y
                mat_inv <<- NULL #re-initialize inverse on change
        }
        get <- function()
                x
        set_inverse <- function(x_inverse)
                mat_inv <<- x_inverse
        get_inverse <- function()
                mat_inv
        list(
                set = set,
                get = get,
                set_inverse = set_inverse,
                get_inverse = get_inverse
        )
}


## The function below given the matrix with cache returns the cached inverse
## when inverse is not cached it comutes the inverse and cache it for future calls

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$get_inverse()
        if (!is.null(mat_inv)) {
                #when cached inverse is present
                message("retrieve cached inverse")
                return(mat_inv)
        }
        mat <- x$get()
        mat_inv <- solve(mat) #comute the inverse
        x$set_inverse(mat_inv) #put the computed inverse in cache
        message("inverse added to cache")
        mat_inv
}
