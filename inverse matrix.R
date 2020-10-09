makeCacheMatrix <- function(x = matrix()) {
  inverse_data <- NULL
  set <- function(y){
    x <<- y
    inverse_data <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inverse_data <<- solveMatrix
  getInverse <- function() inverse_data
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inverse_data <- x$getInverse()
  if(!is.null(inverse_data)){
    message("getting cached data")
    return(inverse_data)
  }
  data <- x$get()
  inverse_data <- solve(data)
  x$setInverse(inverse_data)
  inverse_data
}

#example
test <- makeCacheMatrix(matrix(c(1,2,3,5,1,2,3,4,3), nrow = 3))
round(cacheSolve(test) %*% matrix(c(1,2,3,5,1,2,3,4,3), nrow = 3))
#so we have identity matrix after multiplication