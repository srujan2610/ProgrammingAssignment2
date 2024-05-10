#1st answer
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheMatrix <- makeCacheMatrix()

matrixValue <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
cacheMatrix$set(matrixValue)

cacheMatrix$get()

inverseMatrixValue <- solve(matrixValue)
cacheMatrix$setInverse(inverseMatrixValue)

cacheMatrix$getInverse()

#2nd Answer


cacheSolve <- function(cacheMatrix) {
  inv <- cacheMatrix$getInverse()
  
  if (!is.null(inv)) {
    message("Getting cached inverse.")
    return(inv)
  }
  
  mat <- cacheMatrix$get()
  inv <- solve(mat)
  

  cacheMatrix$setInverse(inv)

  inv
}
cacheMatrix <- makeCacheMatrix()

matrixValue <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
cacheMatrix$set(matrixValue)

cacheSolve(cacheMatrix)

cacheSolve(cacheMatrix)
