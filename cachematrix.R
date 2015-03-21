## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # makeCacheMatrix$set: set the CacheMatrix's matrix object.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # getCacheMatrix$set: get the CacheMatrix's matrix object.
  get <- function() {
    x
  }
  
  # makeCacheMatrix$setinverse: set the CacheMatrix's inversed matrix.
  # Usually called by the cacheSolve function.
  setinverse <- function(inverse) {
    i <<- inverse
  }
  
  # makeCacheMatrix$setinverse: get the CacheMatrix's inversed matrix.
  getinverse <- function() {
    i
  }
  
  # Build a list containing the CacheMatrix object's functions. Return
  # the list as the CacheMatrix object.
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Get the inverse CacheMatrix object, or NULL, if it is not set.
  i <- x$getinverse()
  
  # Check to see if the inverse CacheMatrix subject is set. If it is set,
  # just return it.
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}

## testCacheMatrix: Unit tests to ensure proper functioning of the CacheMatrix object 
testCacheMatrix <- function() {
  # Create a normal matrix for testing, testMatrix
  # Matricies are contstructed by columns, from left to right.
  testMatrix <- matrix(c(1, 0, 5, 2, 1, 6, 3, 5, 0), nrow = 3, ncol = 3)
  
  print("Running Tests...")
  
  # Create the CacheMatrix object
  cacheMatrix <- makeCacheMatrix(testMatrix)
  
  # Print the CacheMatrix matrix
  print("CacheMatrix:")
  print(cacheMatrix$get())
  
  # At this point, the inverse of the matrix stored in the CacheMatrix object should
  # not exist. Therefore, the result returned from the CacheMatrix$get() function 
  # should be NULL.
  message("Cached inverse of CacheMatrix. Should be NULL:")
  print(cacheMatrix$getinverse())
  
  # Run cacheSolve on the CacheMatrix object, creating an inversed matrix stored in
  # the CacheMatrix object.
  message("Setting inverse of CacheMatrix...")
  cacheSolve(cacheMatrix)
  
  # Print the stored, inversed matrix. If functioning properly, it should no longer 
  # be NULL.
  message("Cached inverse of CacheMatrix. Should be no longer be NULL:")
  print(cacheMatrix$getinverse())
  
  # Call cacheSolve a second time the stored, inversed matrix. If functioning properly,
  # it should print a the message "getting cached data".
  message('Cached inverse of CacheMatrix. Should print message "getting cached data":')
  print(cacheSolve(cacheMatrix))
}
