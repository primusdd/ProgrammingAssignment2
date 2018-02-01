## Created by Daniel Hubbeling, 1-feb-2018
## The functiona below can be used to cache the inverse of a given matrix
## To test the following commands will work:
##
## aMatrix <- makeCacheMatrix(matrix(sample(0:10, 25, TRUE), 5, 5)) ## initialize the special matrix with a randomly created matrix
## aMatrix$get()                                                    ## shows the created matrix
## aMatrix$getinverse()                                             ## shows the inverse of this matrix, but not solved yet so NULL
## cacheSolve(aMatrix)                                              ## shows the inverse and stores in cache
## cacheSolve(aMatrix)                                              ## shows the same inverse but this time from cache
## aMatrix$getinverse()                                             ## also shows the inverse of this matrix now in cache


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}