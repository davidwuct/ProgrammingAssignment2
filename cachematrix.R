## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix is used to store the matrix and its inverse-matrix if it has existed. If the inverse-matrix hasn't 
## existed yet, we use cacheSolve to compute it ourselves and return it to makeCacheMatrix for storing. 

## Write a short comment describing this function
## makeCacheMatrix is a function that has two variable: x, inv that stores the matrix and its inverse-matrix respectively.
## Besides, it has four sub-functions: setMatrix, getMatrix, setInverse, getInverse, and we can use them to get the current- 
## stored matrix, set the content of matrix x, get the current-stored inverse-matrix, and set the content of inverse-matrix 
## inv, respectively.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setMatrix <- function(y = matrix()) x <<- y;
    getMatrix <- function() x
    setInverse <- function(inverse = matrix()) inv <<- inverse
    getInverse <- function() inv
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve is a function that access the x, which is returned by makeCacheMatrix, as an argument.  
## First, it calls the getInverse of x, if the returned data is not NULL, which means the inverse-matrix
## has already computed, we can simply return the existed inverse-matrix. On the other hand, if the inverse-matrix
## hasn't been computed, we compute it right now! For computing the inverse-matrix, we have to access the matrix with 
## getMatrix. Then we use R-defined function, Solve(), to compute the inverse-matrix and return.
cacheSolve <- function(x, ...) {
    inv1 <- x$getInverse()
    if(!is.null(inv1)){
        message("getting cached inverse-matrix")
        return(inv1)
    }
    else{ 
        matrix_temp <- x$getMatrix()
        inv_temp <- solve(matrix_temp)
        x$setInverse(inv_temp)
        inv_temp
    }
        ## Return a matrix that is the inverse of 'x'
}
