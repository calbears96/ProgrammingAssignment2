## Two functions are created here. The first creates a cache of inverse matrix and the second
## solves the inverse matrix (checking to see if the answer is cached first). If the answer has
## been cached, it fetches that value from the first function. If not, it solves the inverse of 
## the matrix and stores the results in the makeCacheMatrix function

## makeCacheMatrix creates a matrix that is just a list containing a function to do the following:
## 1: set the value of the matrix
## 2: get the value of the matrix
## 3: get the value of the inverse matrix
## 4: get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set , get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of the matrix created with makeCacheMatrix. It first checks to see
## if the inverse has already been calculated. If it has, then it retrieves the inverse matrix from the cache
## and skips the computation. Otherwise, cacheSolve calculates the inverse of the data and then sets the value
## of the inverse matrix in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message('getting cached data')
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}