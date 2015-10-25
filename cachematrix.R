## create a function that can create a matrix 
## example of creating a matrix: test_matrix <- makeCacheMatrix(matrix(1:4,2,2))
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## create a function that can calculate the inverse of the matrix above and if the inverse has already been calculated 
## (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    print("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
## using the cacheSolve, example :

##cacheSolve(test_matrix)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##cacheSolve(test_matrix)
##[1] "getting cached data"
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
