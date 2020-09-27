## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix creates 2 objects: x (given matrix) and inv (inverse matrix), 
#and a list including 4 nested functions that will be used later

#cacheSolve makes use of the previous functions created by makeCacheMatrix in 
#order to obtained inv or, if it had already been acquired, print it  without 
#the need of recalculating it

## Write a short comment describing this function

# Explanation of each function nested in mackeCachecMatrix:
#makeCacheMatrix$set allows the setting of a new matrix
#makeCacheMatrix$get shows the value assigned to x
#makeCacheMatrix$setinv sets the value of inv 
#makeCacheMatrix$getinv shows the value assigned to inv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function

#First it searchs for the calculated inverse matrix in the inv object from 
#makeCacheMatrix. If the inverse matrix exists it is returned, if not, it is 
#calculated using the solve() function and is assigned to inv

#the "x" argument in cacheSolve must be an object of type makeCacheMatrix(). It 
#must be the object to which the result of makeCacheMatrix was assigned, not 
#the given matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv## Return a matrix that is the inverse of 'x'
}
