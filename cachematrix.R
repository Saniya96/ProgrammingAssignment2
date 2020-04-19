#### Make a CacheMatrix function - this function is used to create a cache list that
##stores the inverse of the function once it has been calculated.
## First we initialize the inverse and the matrix. 
## After this we set the values of the matrix and ensure the variables are globally updated by
## using <<-
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(a){
    x <<- a
    inv <<- NULL
  }
   get <- function() x
   setinverse <- function(inverse) inv <<- inverse
   getinverse <- function()inv
   list(set = set, get = get,
        setinverse = setinverse, 
        getinverse = getinverse)
}


## The cacheSolve function allows to compute the inverse of the matrix. 
## x is the matrix and the $ sign allows to call for the inverse
## of the matrix using x$getinverse() and assigning it to the variable inv.
## This is also known as scoping the functions.
## The if function allows us to test a condition, to check if the matrix is null or if it has values.
## If it has values, the x$get() will get the values in the matrix and the
## the solve function allows to get the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
    my_matrix <- x$get()
    inv <- solve(my_matrix, ...)
    x$setinverse(inv)
    inv
}
