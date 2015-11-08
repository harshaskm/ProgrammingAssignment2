## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function will return a special variable that can hold variables and functions
# After it has been declared, executed by passing a square matrix, it can be used to
# fetch inverse of the matrix that was passed as the parameter and it can also be cached for future use.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x

	#Following is a function that is being formulated
	# and assigned to a function variable named setsolve
	# what does setsolve function variable do?
	# Ans: setsolve is a function which when passed a value to it
	#      will assign the passed value to variable m
	# for ex: if 100 is passed to setsolve like this: setsolve(100)
	# and the result is captured in a vaiable it would get the value 100
	# > y <- setsolve(100)
	# > y
	# [1] 100
		setsolve <- function(solve) m <<- solve

	#Following is a function that is being formulated
	# to just return the value contained in variable m
		getsolve <- function() m

	#All of what we are creating in the function makeCacheMatrix will have to be
	# stored in any variable that will be used to call this function.
	# so that variable should contain the variables in this function and the functions
	# that are declared here, it will be returned as a list:
		list(set = set, get = get,
		     setsolve = setsolve,
		     getsolve = getsolve)
}


## Write a short comment describing this function
# This function will return the inverse of a matrix if it has already been calcluated
#  else it will calculate and return the same.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	#Following line of code will try to fetch value if we already have it
		m <- x$getsolve()

	#If value is m is not null then we say that we are fetching cached data
	# meaning we print the value
	# and also return the value that is already available	
		if(!is.null(m)) {
			message("getting cached data")
			return(m)
		}

	#If we already do not have the inverse of the matrix calculated
	# it gets calculated in the lines below:
		data <- x$get()

	#-------------///***\\\------------#
	#-------------///***\\\------------#
	#This is the crux of the function where it calculates the inverse of the square matrix
	# and assigns the same to variable "m"

		m <- solve(data, ...)

	#-------------///***\\\------------#
	#-------------///***\\\------------#

		x$setsolve(m)
	#Derived inverse of the matrix is returned by the calling the variable name below:
		m
}

