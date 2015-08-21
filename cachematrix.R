


## The function create a matric that kan keep cache of the matrices and its inverse

## Creating a matric than can cache its inverse. It is a list that get
##set the value of the matrix, get the value, then set and get the
##value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	k <- NULL
	set <- function(y){
		x <<- y
		k <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) k <<- k
	getinverse <- function() k
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)

}


## Computes the inverse, if the case is that the inverse have been
##done, it gets thei nverse from the cache and skip the computation

cacheSolve <- function(x, ...) {
	k <- x$getinverse()
	if (!is.null(k)){
		message("getting cached data")
		return(k)
	}
	final <- x$get()
	k <- solve(final, ...)
	x$setinverse(k)
	k
      
}
