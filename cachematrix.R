## Matirx inversion is potentially a time-consuming computation. Below are two functions that are designed to
## create a special object that stores a matrix and cache's its mean.

## This function creates a special "matrix" object that can cache its inverse.
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix
## get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        # test if the inverse exists
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}

#second version of cacheSolve where a test on whether the matrix has changed will be performed

cacheSolve <- function(x, matrix, ...) {
        inv <- x$getinverse()
        # test if the inverse exists and if the matrix has changed
        data <- x$get()
	if(data==matrix & !is.null(inv)) {
		message("getting cached data")
                return(inv)
        }
	else if (data==matrix & is.null(inv)) {
		inv <- solve(data, ...)
		x$setinverse(inv)
	}
	else {
		x$set(matrix)
		data <- x$get()
		inv <- solve(data, ...)
		x$setinverse(inv)
	}
	        inv
                ## Return a matrix that is the inverse of 'x'
}	
