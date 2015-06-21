## Matrix Inversion and Caching
## function that creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  imtx <- NULL
  set <- function(y){
    x <<- y
    imtx <<- NULL
  }
  get <- function() x
  setimtx <- function(inv_mat) imtx <<- inv_mat
  getimtx <- function() imtx
  list(set = set, get = get,
       setimtx = setimtx, 
       getimtx = getimtx)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  imtx <- x$getimtx()
  if(!is.null(imtx)){
    message("getting cached data")
    return(imtx)
  }
  data <- x$get()
  imtx <- solve(data)
  x$setimtx(imtx)
  imtx
}
