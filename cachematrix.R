## Matrix inversion is usually a costly computation and 
## caching the inverse of a matrix rather than compute it repeatedly could be beneficial.
## The following pair of functions cache the inverse of a matrix.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## Usage
## splMatrix = makeCacheMatrix(aMatrix)
## cacheSolve(splMatrix)
## the first time cacheSolve is called on splMatrix, the inverse is returned. When cacheSolve is called on splMatrix once again,
## the cached inverse is returned and the message "getting cached data" is printed along with the inverse

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
		if(!is.null(m)){
			message("getting cached data")
			return(m)
		}
		data <- x$get()
		m <- solve(data, ...)
		x$setinverse(m)
		m
}