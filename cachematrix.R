## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

		m <- NULL
		setmatrix <- function(y = matrix()) {
			x <<- y
			m <<- NULL
		}
		
		getmatrix <- function() x
		
		##solve() function gives you the inverse of a matrix
		setinverse <- function(solve) m <<- solve
		
		getinverse <- function() m
		
		list(setmatrix = setmatrix,
		getmatrix = getmatrix,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached matrix data")
                return(m)
        }
		
	##data not cached, compute new matrix inverse
        data <- x$getmatrix()
        m <- solve(data, ...)
		
        x$setinverse(m)
        m
}
