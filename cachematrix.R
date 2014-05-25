## cachematrix.R
##
## The program contains the following functions "makeCacheMatrix" and "cacheSolve":
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.
##
## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function( x = matrix() ) {
## Initialize "m" with NULL value
m <- NULL
## Set the value of the matrix
set <- function( y ) {
x <<- y
m <<- NULL
}
## Get the value of the matrix
get <- function() {x}
## Set the value of the inverse matrix
setSolve <- function(y) {m <<- y}
## Get the value of the inverse matrix
getSolve <- function() {m}
## Return
list(set=set,get=get,setSolve=setSolve,getSolve=getSolve)
}

## The second function calculates the inverse matrix of the object created with makeCacheMatrix

cacheSolve <- function(x, ...) {
## Get the cached inverse matrix that is stored in "x"
m <- x$getSolve()
## If the cached inverse matrix isn`t null then message "getting cached data" and display the value
if(!is.null(m)){
message("getting cached data")
return(m)}
## If there is no value cached then get the matrix and compute the inverse
data<-x$get()
m <- solve(data, ... )
## Store the inverse matrix in the cache
x$setSolve(m)
## Display the inverse matrix
m
}
