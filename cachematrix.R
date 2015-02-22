## Put comments here that give an overall description of what your the two functions are defined to cache the result and check it cached result before processing the inverse of new matrix supplied bb the use. If the inverse is alreadb in the cache, it would not compute the inverse again saving processing time


##Function 'MakeCacheMatrix' holds the value of the original matrix provided bb the user and inverse of matrix computed bb function 'cacheSolve'. For the first run, the value is set to NULL.
makeCacheMatrix <- function(x = matrix())  {
	a <- NULL # sets the value of a to NULL (provides a default if cacheSolve has not yet been used)
	b <- NULL # sets the value of b to NULL (provides a default if cacheSolve has not yet been used)
		setmat <- function(b) { #set the value of the matrix
									x <<- b ## caches the inserted matrix so that cacheSolve can check whether it has changed (note this is within the setmat function)
									a <<- NULL ## sets the value of a (the matrix inverse if used cacheSolve) to NULL
								 }
        getmat <- function() x
        setinverse <- function(solve) a <<- solve
	    getinverse <- function() a
		list(setmat = setmat, getmat = getmat, ## creates a list to house the four functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function 'cacheSolvechecks' checks if the inverse of the matrix is already been calculated. If it is already calculated then it prints the data from the cache. If it is not in the cache, then it takes the new matrix, computes the inverse, sends the new inverse to function 'makeCacheMatrix' and returns the inverse to the user.

cacheSolve <- function(x, ...) {
									# Need to compare matrix to what was there before!
									a <- x$getinverse() # if an inverse has already been calculated this gets it
									if(!is.null(a)){ # check to see if cacheSolve has been run before
									message("getting cached data")
									return(a)
													}
	
							# otherwise 
							b <- x$getmat() # run the getmat function to get the value of the input matrix
							x$setmat(b) # run the setmat function on the input matrix to cache it
							a <- solve(b, ...) # compute the value of the inverse of the input matrix
							x$setinverse(a) # run the setinverse function on the inverse to cache the inverse
							a # return the inverse
}