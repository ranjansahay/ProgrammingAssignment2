## This code has two "wrapper" functions:
## 1) makeCacheMatrix : is a wrapper for the matrix() function. It creates an object which 
## contains a matrix and its inverse.
## 2) cacheSolve() is a wrapper for solve() function
## Both the functions encapsulate R's matix() and solve() functions, respectively and provide
## some additional features.
##
## function: makeCacheMatrix
## the function takes a matrix as input and returns a list of
## methods/functions
## This function encapsulates two variables: 
## 1) 'x' assigned to the input matrix and 
## 2) 'inverse' which is a placeholder for the inverse of x
## The list of functions returned by this function are the accesor methods to inspect or alter the encapsulated variables
##
## function: cacheSolve
## This function takes the result of the makeCacheMatrix function as an input parameter.
## Returns the attribute 'inverse' of the input parameter.
## If 'inverse' is set to NULL, it calculates the inverse of the matrix and sets the 'inverse' attribute.
##
## ERROR HANDLING/EXCEPTIONS
## These functions do not handle any error. There no checks for singular matix
## or non-square matrix. The error handling and exception catching is left to the encapsulated
## functions (matrix(), solve())
##
## Example:
## mat1 <- makeCacheMatrix(matrix(c(1:4),2,2))
## cacheSolve(mat1)

## function: makeCacheMatrix
## input: a matix
## returns: The list of functions (accesor methods for encapsulated variables)

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## function: cacheSolve
## input: 
##       1. result of the makeCacheMatrix function as an input parameter.
##       2. ellipsis: all other formal parameter expected by solve() function
## returns: inverse of the makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
