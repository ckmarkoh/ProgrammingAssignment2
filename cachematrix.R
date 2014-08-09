## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function create a class of special matrix
# It return a list contains four function:
# 1. set(y): set the value of matrix to be x to be y and m to be NULL
# 2. get(): get the value of matrix x 
# 3. setinverse(inverse): set the value of inverse matrix m to be inverse
# 4. getinverse(): get the value of inverse matrix m
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

#This function calculate the inverse matrix of the matrix made by makeCacheMatrix
#First, it use the method "getinverse()" to check whether the inverse has been computed or not.
#If the inverse has been computed, it return matrix m and print the message "getting cached data"
#If not, it compute the inverse matrix by the function "solve(data, ...)",
#and store the inverse matrix m by "$setimverse(m)", and then return m.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
