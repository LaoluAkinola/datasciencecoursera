## Given a matrix x, makeCacheMatrix creates and returns a special 
## matrix object containing x and its inverse. This object also
## contains functions to modify and access the data in the matrix
## object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  setMatrix <- function(y){
    x <<- y
    inv <<- NULL
  }
  getMatrix <- function(){
    x
  }
  setInv <- function(matrix_inverse){
    inv <<- matrix_inverse
  }
  getInv <- function(){
    inv
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInv = setInv, getInv = getInv)

}


## Given a matrix object as defined in makeCacheMatrix(), cacheSolve() 
## evaluates the inverse of the matrix if the caches is empty, otherwise,
## the inverse of the matrix is evaluated and cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cache data")
    return(inv)
  }
  
  matrixData <- x$getMatrix()
  inv <- solve(matrixData)
  x$setInv(inv)
  inv
}

