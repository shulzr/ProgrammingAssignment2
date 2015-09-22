##  1) create a cached matrix by makeCacheMatrix()
##  2) solve it by cacheSolve()

## creates a list with cached inversed matrix inside

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinv <- function(invertedMatrix) {
			m <<- invertedMatrix
		}
            getinv <- function() m
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)

}


## Input - list created by makeCacheMatrix. Output - inverted matrix.
## If inverted matrix already cached -- uses cache.

cacheSolve <- function(x, ...) {
            m <- x$getinv()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, )
            x$setinv(m)

            return (m)
}
