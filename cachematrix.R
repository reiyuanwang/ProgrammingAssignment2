## Function to create a special "matrix" object with caching
## The function creates a special matrix object with methods
#to set and get the matrix data,
## set and get the cached inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(
            set = set,
            get = get,
            setInverse = setInverse,
            getInverse = getInverse
    )

## Function to compute and cache the inverse of the matrix
## The function computes the inverse of the matrix and 
# caches it to avoid recomputation.
## If the cached inverse already exists, it is retrieved from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

