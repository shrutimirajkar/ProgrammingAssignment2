

## Function to create cache matrix

makeCacheMatrix <- function(x = matrix()) {m <- NULL
                                           set <- function(y) {
                                                   x <<- y
                                                   m <<- NULL
                                           }
                                           get <- function() x
                                           setsolve <- function(solve) m <<- solve
                                           getsolve <- function() m
                                           list(set = set, get = get,
                                                setsolve = setsolve,
                                                getsolve = getsolve)

}


## This function gives inverse of matrix

cacheSolve <- function(x, ...) {m <- x$getsolve()
                                if(!is.null(m)) {
                                        message("getting inverse matrix")
                                        return(m)
                                }
                                data <- x$get()
                                m <- solve(data, ...)
                                x$setsolve(m)
                                m
}
