## Put comments here that give an overall description of what your
## functions do

## As the assignment requests, this function creates a special "matrix" object that 
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        ## to Set value of matrix:
        setMatrixVal <- function(y){
                x <<- y
                invMatrix <<- NULL
        }
        ## To get value of matrix
getMatrixVal <- function()x
## To set value of inverse matrix
setInverseVal <- function(inverse) invMatrix <<- inverse
## To get value of inverse matrix
getInverseVal <- function() invMatrix
 list(setMatrixval = setMatrixVal, getMatrixVal = getMatrixVal,
     setInverseVal = setInverseVal, getInverseVal = getInverseVal)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInverseVal()
        if(!is.null(invMatrix)){
                message("Getting cached inversible matrix")
                return(invMatrix)
        }
        matrixData <- x$getMatrixVal()
        invMatrix <- solve(matrixData, ...)
        x$setInverseVal(invMatrix)
        return(invMatrix)
}

                        ## Tests
## 1 
matrix1 <- matrix(1:4, 2, 2)
matrix1
cacheMatrix <- makeCacheMatrix(matrix1)
cacheSolve(cacheMatrix)

## 2
matrix2 <- matrix(c(2, 10, 5, 14), 2, 2)
cacheMatrix <- makeCacheMatrix(matrix2)
cacheSolve(cacheMatrix)

