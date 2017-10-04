# The "makeCacheMatrix" function creates an object containing four functions.
# It also contains the matrix given as argument to the function "makeCacheMatrix" or to the set function contained in the obect. 
# The "cacheSolve" function calculates the inverse of the matrix contained in the makeCacheMatrix object.
  

# "makeCacheMatrix" takes a matrix in input, and stores its inverse matrix once it has been calculated with the "cacheSolve" function.
# If the original matrix is modified in the makeCacheMatrix object the inverse matrix is set to NULL.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(solve) inv <<- solve
	getInv <- function() inv
	list(set = set, get = get,
		setInv = setInv,
		getInv = getInv)
}


# Calculates the inverse matrix of the matrix stored in the makeCacheMatrix object.
# If the calculation has already been done, the function returns the inverse matrix stored in the makeCacheMatrix object, without any further calculation.
cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInv()
	if(!is.null(inv)){
		message("Getting cached data")
		return(inv)
	}
	data <- x$get()
	inv  <- solve(data, ...)
	x$setInv(inv)
	return(inv)
}
