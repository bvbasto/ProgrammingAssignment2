## Put comments here that give an overall description of what your
## functions do

#BVB 2014.08.23
# makeCacheMatrix  
        #       receives a matrix trough the initialization or the function setMatrix
        #       everytime he gets a new matrix inverse variable is reseted
        #       the inverted matrix is stored in inverse variable
        #       both data and inverted matrix (if aledy been) passed through the setInverse function, are stored in cache

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setMatrix <- function(y) { 
                x <<- y
                #if the inverted matrix was already been calculated, variable is reseted
                inverse <<- NULL
        }
        getMatrix <- function() x
        setInverse  <- function(y) inverse <<- y
        getInverse <- function() inverse
        
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
# cacheSolve
        #       this function uses  makeCacheMatrix to store an inverted Matrix
        #       it expectes to receive a makeCacheMatrix object and if this object aready
        #       have the inverted matrix, the result is returned without any calculation
        #       if the inverted matrix is not present, this function calculates the inverted matrix
        #       store this result and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        #already have a soltuion for the last matrix submited, return cache 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #otherwise, calculate it
        data <- x$getMatrix()
        m <- solve(data)
        x$setInverse(m)
        #and return it
        m
}
