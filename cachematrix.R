## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix takes a matrix as input and stores it and cache's its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list( set = set , get = get , setinverse = setinverse , getinverse = getinverse)

}


## Write a short comment describing this function
## The following function is used to get the inverse of a matrix:
## -It first checks to see if the inverse has been calculated before.
## - if not, it calculates the inverse and also sets the inverse to be stored in
## -the cache and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	## first check if the inverse has been calculating by checking the cache
        m <- x$getinverse()

	## if the inverse is already in cache, return the inverse from cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

	## if the inverse is not in cache get the matrix and solve for inverse
        data <- x$get()
        m <- solve(data)

	## set the inverse to be stored in cache for future use
        x$setinverse(m)

	## return the inverse
        m
}
