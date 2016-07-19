## This file contains the assignment submission for Coursera R Programming Peer Graded Assignment: Programming Assignment 2: Lexical Scoping
## There is two function in here:-
## 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        #By default inv are set to NULL
	inv <- NULL
	#Allow user to set data without calling makeCacheMatrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
	#store the data used for calculation
        get <- function() x
	#Perform inv calculation
        setInverse <- function(solve) inv <<- solve
        #Caching the result
	getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## 2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	#get cached if available
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
	#else calculate the inverse
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

