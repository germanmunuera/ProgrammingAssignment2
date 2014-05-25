## Calculates the inverse matrix of an invertible matrix. The result is cached
## while the invertible matrix is not modified
## Example:
## > inv <- makeCacheMatrix(matrix(c(2,2,3,2), 2,2))
## > inv$get()
## [,1] [,2]
## [1,]    2    3
## [2,]    2    2
## > cacheSolve(inv)
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0

## makeCacheMatrix -> Stores an invertible matrix 'x', the inverse matrix 'i'
##   (when it's calculated) and getter/setter functions
makeCacheMatrix <- function(x = matrix()) {
  # Variable to store the calculated inverse matrix
  i <- NULL
  
  # set: Assigns a new matrix into 'x' and reset the inverse matrix 'i'
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # get: Returns the current assigned matrix 'x'
  get <- function() x
  # setinverse: Assigns the calculated inverse matrix into 'i'
  setinverse <- function(inverse) i <<- inverse
  # getinverse: Returns the calculated inverse matrix 'i'
  getinverse <- function() i
  # list of cache matrix functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve -> Calculates the inverse matrix of an invertible matrix.
## The 'x' parameter must to be a function object returned by makeCacheMatrix function
## Retrieve the cached object when is present, otherwise calculate the inverse 
## matrix with 'solve' and cache it 
cacheSolve <- function(x, ...) {
  # Gets the inverse matrix of 'x' and return it when it's not equal null
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # Gets the invertible matrix of 'x'
  data <- x$get()
  # Calculate the inverse matrix of 'x'
  i <- solve(data, ...)
  # Cache the inverse matrix of 'x'
  x$setinverse(i)
  # Return a matrix that is the inverse of 'x'
  i
}
