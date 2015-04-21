## Functions to do  create a matrix object. In the matrix object an inverse of that matrix can be stored.
 

## The matrix object. Stores a matrix and it s inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## returns the inverse of a matrix. If the inverse already exists, the existing inverse is returned
##PARAM x: a makeCacheMatrix object

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("returning cached inverse")
    return(i)
  }
  message("calculating and returning inverse")
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

#code to test above functions
#matrix <- makeCacheMatrix(rbind(c(2,3,1,5), c(1,0,3,1),c(0, 2, -3, 2 ) ,c(0,2,3,1)))
#matrix$get()
#first time not from cache
#cacheSolve(matrix)
#second time from cache
#cacheSolve(matrix)
