## makeCacheMatrix is the function that construct an object containing a
## matrix and inverse of this matrix.

## cacheSolve is a function that calculate inverse matrix containing in object
## created by function makeCacheMatrix. If the object contains inverse matrix,
## then function doesn't calculate inverse matrix again. It just return inverse
## matrix from object.


## makeCacheMatrix takes one argument x - the initial matrix. One can change
## the matrix x by function setX and take it by function getX. setInverseMatrix
## and getInverseMatrix is a functions that can change or get inverse matrix
## accordingly.
makeCacheMatrix <- function(x = matrix()) {
    
    inverseMatrix <- NULL
    
    setX <- function(matrixParam) {
        x <<- matrixParam
        inverseMatrix <<- NULL
    }
    
    getX <- function() {
        return(x)
    }
    
    setInverseMatrix <- function(inverseMatrixParam) {
        inverseMatrix <<- inverseMatrixParam
    }
    
    getInverseMatrix <- function() {
        return(inverseMatrix)
    }
    
    list(
        setX = setX,
        getX = getX,
        setInverseMatrix = setInverseMatrix,
        getInverseMatrix = getInverseMatrix
    )
    
}

## cacheSolve takes argument x - an object created by function makeCacheMatrix.
## cacheSolve calculate inverse matrix of matrix that contained in object,
## set this inverse matrix to object and return inverse matrix. If object
## already contain inverse matrix, then cacheSolve just return inverse matrix
## from object.
cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverseMatrix()
    if (is.null(inverseMatrix)) {
        matrix <- x$getX()
        inverseMatrix <- solve(matrix)
        x$setInverseMatrix(inverseMatrix)
        return(inverseMatrix)
    }
    else {
        message("matrix from cache")
        return(inverseMatrix)
    }
}
