## cachematrix.R:  
## 
##
## Felipe Aristizabal 
## cursera: R Programming
## Peer Assignment 2
## August 22, 2014


## makeCacheMatrix: Write a short comment describing this function

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


## cacheSolve: Write a short comment describing this function

cacheSolve <- function(mat, ...) {
	inv <- mat$getinv()
	if(!is.null(inv)) {
		message("Getting cached data ...")
		return(inv)
	}
	mdat <- mat$get()
	inv <- solve(mdat, ...)
	mat$setinv(inv)
	
	## Return a matrix that is the inverse of 'x'
	return(inv)
}
