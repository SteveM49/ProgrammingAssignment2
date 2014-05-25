## These functions save the solution (inverse) of 
## a matrix so te calculation can be done only once,
## then on subsequent calls a cached version of the 
## result is produced
##
## this function sets up an instance of the object
## that holds te inverse of a matrix
## If mat is a matrix usage is
## a <- makeCacheMatrix(mat)

makeCacheMatrix <- function(x = matrix()) {
       inverse <- NULL
       set <- function(y) {
                x <<- y
               inverse <<- NULL
       }
       get <- function() x
       setinv <- function(solve) inverse <<- solve
       getinv <- function() inverse
       list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## Return a matrix that is the inverse of 'x'
##
## use makeCacheMatix first: a <- makeCacheMatrix(x)
## then use object "a" to get the solution:
## cacheSolve(a)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        +     inverse <- x$getinv()
        +     if(!is.null(inverse)) {
                +         message("getting cached data")
                +         return(inverse)}
        +     data <- x$get()
        +     inverse <- solve(data, ...)
        +     x$setinv(inverse)
        +     inverse}
