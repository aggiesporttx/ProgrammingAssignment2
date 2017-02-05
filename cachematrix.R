## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse.
## This function contains functions within the function.  get, set, setInverse and getInverse

makeCacheMatrix <- function(x = matrix()) {
	objMatrixInverse <- NULL

	##  use set to store the original matrix
	set <- function(y) {
		x <<- y
		objMatrixInverse <<- NULL  ##loading in a new matrix.  reset the inv
	}

	##  the get function retrieves the ORIGINAL matrix
	get <- function() {
		x
	}
	
	## set inverse caches the matrix inverse
	setInverse <- function(inverse)  {
		objMatrixInverse <<- inverse
	}

	## retreives the cached inverse  
	getInverse <- function() {
			objMatrixInverse 
		}
	
	list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special matrix returned by makeCacheMatrix 
##  Check it if has first been calculated and matrix is still the same
cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
## every time we load in a new matrix it null out the inverse
	objMatrixInv <- x$getInverse()

	## if we got something back, we're done
	if (!is.null(objMatrixInv ))  {
		message("getting cached inv matrix")
		return(objMatrixInv)
	}

	## otherwise we have work to do
	objMatrix <- x$get()  ## pull back cached matrix
	objMatrixInv  <- solve(objMatrix )  ## calculate matrix inversion
	x$setInverse (objMatrixInv  )  ##store matrix inversion
	objMatrixInv  ## return it
}
