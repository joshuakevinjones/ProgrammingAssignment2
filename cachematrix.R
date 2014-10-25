## The overall intent of these functions is to cache a matrix inverse, rather than compute it repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	#! The sample function usages the following syntax to make a vector: makeVector <- function(x = numeric()) _
		#! In R, I ran class(numeric) and got "function."  I then tried class(matrix) and also got "function." _
		#! I will assume that I can just use "matrix" in place of "vector" since the two seem to have the same class.

	Minv <- NULL
    set <- function(y) {
        x <<- y
        Minv <<- NULL
			#! Here, I just copied the syntax from the vector example and put "Minv" for "matrix inverse" instead of "m" in the example. _
				#! Obviously, this is just my best guess.	
	}
	get <- function() x
		#! Copied same syntax from the vector example.

	setMinv <- function(z) Minv <<- z
		#! I did not know what to do here, so I tried to copy the vector example syntax. _
			#! I used "z" above where the vector example used "mean"...
	
    getMinv <- function() Minv
		#! I just copied the example again, replacing the example variable names with the ones I created.
	
	list(set = set, get = get,
       setMinv = setMinv,
       getMinv = getMinv)
		#! Again, copied the example syntax and replaced variable names with my own.
}	
	
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
	## If the inverse has already been calculated (and the matrix has not changed), 
	## ...then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
		#! Similar to the above, I used the vector example function and replaced the variable names with the names I created.
	
		Minv <- x$getMinv()
		if(!is.null(Minv)) {
			message("getting cached data.")
			return(Minv)
    }
	data <- x$get()
    Minv <- solve(data)
	x$setMinv(Minv)
    Minv
}
