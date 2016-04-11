## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

      xInv <- NULL 
      # set function is used to set a matrix to object
      set <- function(y) {
	  x <<- y
	  xInv <<- NULL       }

      get <- function() x # returns the input matrix
      setInv <- function(inv) xInv <<- inv # sets the inversed matrix
      getInv <- function() xInv # returns the inversed matrix
      # returns a list that contains,
      # x$set(newmatrix) # to change matrix
      # x$get # gets the set matrix
      # x$setInv # sets the inversed matrix
      # x$getInv # gets the inversed matrix
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
xInv <- x$getInv() # gets the inversed matrix from object x
      # it is null if uncalculated
      if(!is.null(xInv)) { # if the inversion result is there
	  message("getting cached data")
	  return(xInv) # returns the calculated inversion
      }
      data <- x$get() # if not, we do x$get to get the matrix object
      xInv <- solve(data) 
      x$setInv(xInv) # sets it to the object
      xInv # returns the solved result

}


  #  To Test
  # generate a random square, non-singular matrix
  test <- matrix(runif(9,1,100),3,3)
  # generate the makeCacheMatrix object with this matrix
  testCached <- makeCacheMatrix(test)
  # from now on calculate or retrieve calculated inversion using the cacheSolve function

  testInv <- cacheSolve(testCached)
  testInv <- cacheSolve(testCached)

