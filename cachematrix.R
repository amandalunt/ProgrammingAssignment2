## Solution for Assignment 2, R Programming, to Cache the Inverse of a Square
## Matrix
## Solution based on provided code for makeVector() and cachemean()
##
## Author: Amanda Lunt
## Date: 19th June, 2014


#makeCacheMatrix()
#This function creates a special "matrix" object that can cache its inverse.
#It takes a numeric square matrix and uses the solve() function to create the
#inverse of the matrix
makeCacheMatrix <- function(x = numeric()){
	
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
    
}

#cacheSolve()
#On first call on a matrix, this function creates the inverse of the matrix from
#makeCacheMatrix(). Subsequent calls return the previously cached inverse of the
#matrix
cacheSolve <- function(x, ...){
	
	inv <- x$getinv()
	if(!is.null(inv)){
		
		message("getting cached data")
		return(inv)
		
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
	
}