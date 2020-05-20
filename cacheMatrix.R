## This function reates a matrix that can cache its inverse

makeCacheMatrix <- function(x=matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	
	get function() x
	
	setinverse <- function () inverse
	getinverse <- function (inv) 	inv <<- inverse
	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
	
}



## This function calculates the inverse of the matrix returned by the functon makeCacheMatrix above

cacheSolve <- function(x, ...) {
	inverse <- x$getinverse
	
	if(!is.null(inverse)) {
		message("Getting cached data")
		return(inverse)
	}
	
	z <- x$get () 
	inverse <- solve(z, ...)
	
	x$setinverse(inverse)
	return(inverse)
}

m  <- 	matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
m2 <- 	makeCacheMatrix(m)

cacheSolve(m2)


########################################################################################

## This function reates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(solveMatrix) inverse <<- solveMatrix
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function calculates the inverse of the matrix returned by the functon makeCacheMatrix above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse()
  
  if(!is.null(inverse)){
    message("Getting cached data")
    return(inverse)
  }
  
  z <- x$get()
  inverse <- solve(z)
  x$setinverse(inverse)
  inverse      
}

## 	Example of how to use this program with defined matrix 
##	m  <- 	matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
##	m2 <- 	makeCacheMatrix(m)

##	cacheSolve(m2)
