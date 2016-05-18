##This is a pair of functions that cache the inverse of a matrix.
##Matrix inversion is usually a costly computation and there may be some benefit to
##caching the inverse of a matrix rather than computing it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        
        MatrixInv = NULL
        
        set <- function(y) {
                x <<- y
                MatrixInv <<- NULL
        }
        
        get = function() x
        SetMatrixInv = function(inverse) MatrixInv <<- inverse 
        GetMatrixInv = function() MatrixInv
        list(set=set,
             get=get,
             SetMatrixInv=SetMatrixInv,
             GetMatrixInv=GetMatrixInv)

}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the function 
##cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        MatrixInv = x$GetMatrixInv()
        
        #Get the inverse from the cache if it has already been calculated
        
        if (!is.null(MatrixInv)){
                message("Getting cached matrix")
                return(MatrixInv)
        }
        
        #Else, calculate the inverse
        
        matrix.data = x$get()
        MatrixInv = solve(matrix.data, ...)
        
        x$SetMatrixInv(MatrixInv)
        
        return(MatrixInv)
}
