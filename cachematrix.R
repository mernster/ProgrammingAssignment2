## read in a matrix with its inverse initialized to Null which can later be set
makeCacheMatrix <- function(x = matrix()) {
  x.inverse <- NULL

  set <- function(mat) {
    x <<- mat
    x.inverse <<- NULL
  }  
  
  getMatrix <- function() x

  getInverse <- function() x.inverse
  
  setInverse <- function(inverse.matrix) x.inverse <<-inverse.matrix
  
  list(set = set, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)  
}

## read in a matrix and check if its inverse has been stored, if not calculate and store it
cacheSolve <- function(x, ...) {
  hasCached <- !is.null(x$getInverse())

  reportAlreadyCached <- function(){
    message("getting cached inverse matrix")
    x$getInverse()
  }

  cacheInverse <- function(){
    message("Inverse matrix has not been cached, solving and writing to cache")
    mat <- x$getMatrix()
    inverse.matrix <- solve(mat)
    x$setInverse(inverse.matrix)
    inverse.matrix
  }
  
  if(hasCached) reportAlreadyCached() else cacheInverse()
}
