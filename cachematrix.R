## cachematrix.R: functions to cach the inverse of a matrix.  A new "matrix" 
##		object can be create using "makeCacheMatrix" and the inverse of the 
##		matrix is calculated using "cacheSolve".  Do not recalculated if the 
##		inverse is in cache.

## Felipe Aristizabal 
## cursera: R Programming
## Peer Assignment 2
## August 22, 2014

#-------------------------------------------------------------------------------
## makeCacheMatrix: This function creates a special "matrix" object that can 
##		cache its inverse.  Clean the cash if the matrix changes. 
## Usage: a <- makeCacheMatrix(mat) 
##		mat: invertible matrix
##		a: special "matrix" object with cache inverse
##			a$get(): returns the matrix
##			a$set(): modifies the matrix
##			a$getinv(): returns the invers of the matrix
##			a$setinv(): stores the invers of the matrix
#-------------------------------------------------------------------------------
makeCacheMatrix <- function(mat = matrix()) {
	inv <- NULL
	set <- function(y) {
		mat <<- y
		inv <<- NULL
	}
	get <- function() mat
	setinv <- function(usrInv) inv <<- usrInv 
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


#-------------------------------------------------------------------------------
## cacheSolve: computes the inverse of the special "matrix" returned by 
##		makeCacheMatrix. If availabe use the cache.
## Usage: cacheSolve(a)
##		a: special "matrix" object created with makeCacheMatrix
#-------------------------------------------------------------------------------
cacheSolve <- function(mat, ...) {
	inv <- mat$getinv()
	if(!is.null(inv)) {
		message("Getting cached data ...")
		return(inv)
	}
	mdat <- mat$get()
	inv <- solve(mdat, ...)
	mat$setinv(inv)
	
	## Return a matrix that is the inverse of 'mat'
	return(inv)
}

#-------------------------------------------------------------------------------
#[EOF]