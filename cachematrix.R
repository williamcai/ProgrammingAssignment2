# ---------------------------------------------------------------------------- #
#   makeCacheMatrix: This function creates a special "matrix" object that can 
#    cache its inverse.
#   cacheSolve: This function computes the inverse of the special "matrix" 
#    returned by makeCacheMatrix above. If the inverse has already been 
#    calculated (and the matrix has not changed), then the cachesolve should 
#    retrieve the inverse from the cache.
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#   makeCacheMatrix: This function creates a special "matrix" object that can 
#    cache its inverse.
# ---------------------------------------------------------------------------- #
makeCacheMatrix <- function(x = matrix()) {
  matrix <- NULL
  set <- function(y) {
    x <<- y
    matrix <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse) matrix <<- inverse
  getmatrix <- function() matrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)

}

# ---------------------------------------------------------------------------- #
#   cacheSolve: This function computes the inverse of the special "matrix" 
#    returned by makeCacheMatrix above. If the inverse has already been 
#    calculated (and the matrix has not changed), then the cachesolve should 
#    retrieve the inverse from the cache.
# ---------------------------------------------------------------------------- #
cacheSolve <- function(x, ...) {
   matrix <- x$getmatrix()
  if(!is.null(matrix)) {
    message("getting cached data")
    return(matrix)
  }
  data <- x$get()
  matrix <- InvertMatrix(data)
  x$setmatrix(matrix)
  matrix
}

# ---------------------------------------------------------------------------- #
#   InvertMatrix: This function computes the inverse of the matrix using
#    solve function.
# ---------------------------------------------------------------------------- #
InvertMatrix <- function(x) {
  n_col <- ncol(x)
  n_row <- nrow(x)
  if (n_col != n_row) {
    # matrix is not invertible
    return(NULL)
  }
  
  matrix_I <- matrix(0,nrow=n_row,ncol=n_col)
  for(i in 1:n_row) {
    matrix_I[i,i] = 1
  }
  # solve this : x*m = matrix_I
  m <- solve(x,matrix_I)
  return(m)
}

