## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

# Invert matrix using solve function
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
  m <- solve(x,matrix_I)
  return(m)
}

