## The following functions allows efficient calculation of matrix inversion by chaching the result and re-calculating it
## only in case the content of the matrix to be inverted is updated
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve should retrieve the inverse from the cache.
## Usage example: 
##      z<-rbind(c(1, -1/3), c(-1/3, 1))  ## create input matrix
##      x<-makeCacheMatrix(z) ## create an object matrix X from Z that can be used with cache
##      y<-cacheSolve(x) ## calculate the invers matrix
##      y%*%z # check that the result is indeed the matrix invers of Z
##      y<-cacheSolve(x) ## re-calculate the invers matrix and see that the cached value was used
################################################################################################


## This function creates a special "matrix" object that can be used to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversmatrix <- NULL  ## initialize the invers matrix value
  set <- function(y) { 
    ## set a new value for the matrix to be inversed and initialize / re-initialize the (cached) result
    x <<- y
    inversmatrix <<- NULL
  }
  get <- function() x ## get current value of the matrix to be inversed
  setinvers <- function(inversvalue) inversmatrix <<- inversvalue ##set inverse matrix value
  getinvers <- function() inversmatrix ## get invers matrix value
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inversmatrix <- x$getinvers() ## (try to) get current invers matrix value
  if(!is.null(inversmatrix)) { ## if chached value can be found
    message("getting cached data")
    return(inversmatrix) ## return cached value and exit
  }
  ## in case cached value could not be found
  data <- x$get() ## get matrix to be inversed
  inversmatrix <- solve(data, ...) ## calculate the invers matrix
  x$setinvers(inversmatrix) ## set calculated invers matrix as a cache to be used in the future
  inversmatrix ## return invers matrix
}