## makeCacheMatrix() creates a special "matrix" and defines some 
## functions used by cacheSolve(), and caches its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # Function resets cache variables if original matrix changes
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get  <- function() x
    # setinverse() function will store the inverse of the matrix in a
    # cache.
    # to be called in cacheSolve() once inverse is solved.
    setinverse <- function(solve) m <<- solve
    # Function returns a null or something depending on state of matrix
    getinverse <- function() m    
    # Returns a list of available functions needed by cacheSolve()
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## cacheSolve() takes  a list functions  returned by makeCacheMatrix()
## makeCacheMatrix() should be instantiated first with a square 
## invertible matrix before calling it with cacheSolve()

cacheSolve <- function(x, ...) {
    # Populate m with the cached inverse matrix, if it exists
    m <- x$getinverse()
    # Check if a matrix is solved/cached
    if(!is.null(m)) {
        # A cached inverse matrix exists, returns matrix and exits
        # function
	message("retrieving cached data")
        return(m)
    }
    ## If no cache exists solve and cache inverse matrix
    
    # Get matrix needed to be solved
    data <- x$get()
    # Solve the inverse of the matrix into m
    m <- solve(data)
    # Store the inverse into the cache
    x$setinverse(m)
    # Return a  solved matrix
    m
}
