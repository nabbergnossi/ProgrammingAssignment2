## these two functions are used to cache the inverse of a matrix 
##so that it doesn't need to be calculated repeatedly

##makeCacheMatrix creates a list of functions that cache a matrix and it's transpose
### makeCacheMatrix also handles retrieving the matrix and it's transpose

makeCacheMatrix <- function(x = matrix()) {
  tp <- NULL #clears tp variable
  #the set function expects a matrix. the matrix is assigned to x
  set <- function(y) {
    x <<- y
    tp <<- NULL
  }
  #the get function retrieves the value of x
  get <- function() x
  #settranspose expects a matrix. 
  #settranspose is used in cacheSolve to transpose
  settranspose <- function(t) tp <<- t
  #gettranspose is used by cacheSolve to get the transpose created by settranspose
  gettranspose <- function() tp
  #creates list of functions with names
  list(set = set, get = get,
       settranspose = settranspose,
       gettranspose = gettranspose)

}


## cacheSolve expects a list created by the function makeCacheMatrix.
## cacheSolve attempts to retrived a cached transpose created by makeCacheMatrix
## if the transpose doesn't exist cacheSolve will create it and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        tp <- x$gettranspose() #try to get the transpose
        #check to see if transpose already exists, if so return cached
        if(!is.null(tp)) {
          message('retrieving cached matrix')
          return(tp)
        }
        #if no transpose exists then create
        data <- x$get()
        tp <- t(data, ...)
        x$settranspose(tp)
        tp
}
