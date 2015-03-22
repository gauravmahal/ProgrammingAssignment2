                        ##Caching the Inverse of a Matrix

##Matrix inversion is usually a costly computation.
##So, there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly


## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##It computes inverse of special "matrix" returned by above function
##If the inverse has already been calculated (and the matrix has not changed)
##then it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i    ## Return a matrix that is the inverse of 'x'
}