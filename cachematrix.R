## Second Programming Assignment
## Unit Test
## a=matrix(c(1,2,3,1,2,0,1,0,0), nrow=3, ncol=3)
## b<-makeCacheMatrix(a)
## c<-cacheSolve(b)
## c<-cacheSolve(b)
## the 2nd invoke of cacheSolve() function returns the cached data

## Make a list of functions for caching a matrix
## input: a matrix
## output: a list of the following functions
## set(amatrix), which sets a matrix
## get(), which returns a matrix. If the matrix is not set, an empty matrix will be returned.
## setinverse(inverse), which sets a matrix inverse for cache
## getinverse(), which returns a chached matrix. If no matrix is cached, an empty matrix will be returned.

makeCacheMatrix <- function(x = matrix()) {

	  ## initialize the inv variable
	  inv <- NULL
	  ## define set(amatrix) function
          set <- function(y) {
        	   x <<- y
        	   inv <<- NULL
          }
	  ## define get() function
	  get <- function() x
	  ## define set(innverse) function
	  setinverse <- function(inverse) inv <<- inverse
	  ## define getinverse() function
          getinverse <- function() inv

	  ## returns the list of functions
	  list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## solve the inverse of a matrix made by makeCacheMatrix(amatrix) function
## if the inverse is not cached before, calculate it, cache it, and return it
## if it is cached before, return the cached inverse 
## input: an invertible matrix
## output: the inverse of the matrix
cacheSolve <- function(x, ...) {
		
	  ## get the inverse of the matrix
	  inv <- x$getinverse()
          ## if the inverse is cached, return it directly 
          if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
          }
          ## if the inverse is not cached, solve the inverse of the matrix
          ## get the matrix
	  data <- x$get()
          ## solve the inverse of the matrix
          inv <- solve(data, ...)
	  ## cache the inverse of the matrix
          x$setinverse(inv)
          ## Return a matrix that is the inverse of 'x'
	  inv
}
