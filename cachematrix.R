## Put comments here that give an overall description of what your
## functions do

## makeCachematrix creates a list which contains a function
#that sets a value of a matrix, gets the value of the matrix
#sets the value of the inverse and gets the value of the inverse
makeCacheMatrix<- function(x = is.matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve solves the inverse of a matrix.
# if the inverse already been found it gets it from the cache
# and skips the calculation if not it solves for it a sets it in the cache with
# the setinv function
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}



