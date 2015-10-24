## This function creates a special "matrix" object that can cache its inverse.
## 1. create a inverse of matrix
##    1.1 set the a inverse of matrix
##    1.2 get a inverse of matrix
##    1.3 set a matrix to be inversed
##    1.4 get a matrix to be inversed


makeCacheMatrix <- function(x = numeric()) {
  
  set <- function(y) {
      x <<- y
      Iocm <<- NULL
  }
  get <- function(){
    if(!sqrt(length(x))%%1==0) { 
      message("This matrix cannot be inverse")
      return(x)
    }else {
    mat <- matrix(x,length(x))
    }
  setInverseMatrix <- function(InverseMatrix) Iom <<- InverseMatrix
  getInverseMatrix <- function() Iom
  }
    
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## 2. create and cashe the inverse of matrix if the inverse of matrix is not cached

cachesolve <- function(x, ...) {
  Iom <- x$getInverseMatrix()
  if(!is.null(Iom)) {
    message("getting cached data")
    return(Iom)
  }
  data <- x$get()
  Iom <- solve(data, ...)
  x$setInverseMatrix(Iom)
  Iom
}

