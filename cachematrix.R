## There are two functions that cache the inverse of a matrix
## Functions can  be run as follows
##    m<-makeCacheMatrix(<<some invertable matrix>>)
##    cacheSolve(m)


## makeCacheMatrix takes an invertible matrix.
## it returns a list of 4 functions that set and get
## both the input matrix and the inverse of the matrix.
## When the function is called 2 pieces of data are set.
##   1. The inverse of the matrix is set to null
##   2. The input matrix is stored as the result of the function get()
##      Because get is defined in the makeCacheMatrix function
##      it gets the value passed into the function.

makeCacheMatrix <- function(x = matrix()) {
   ## initialize the inverse to null
        i <- NULL

   ## when a new matrix comes in, set x and null out the matrix
   ## x and i are in different environments, the <<- operator
   ## lets us find them.
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

   ## get the input matrix
        get <- function() x

   ## store the inverse (passed in - assume correct)
        setinverse <- function(inverse) i <<- inverse

   ## get the stored inverse
        getinverse <- function() i

   ## return the list of the above functions
   ## use name/value pairs to name the functions created above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Gets the inverse of the matrix passed into makeCacheMatrix
## or set using the makeCacheMatrix$set function
cacheSolve <- function(x, ...) {
        ## First, see if the inverse is cached
        i <- x$getinverse()
 
        ## If the cached value isn't null return it
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
	  ## else, get the matrix and find the inverse
        data <- x$get()
        i <- solve(data, ...)

	  ## cache the inverse
        x$setinverse(i)

	  ## return the inverse
        i
}


