## ProgrammingAssignment2
## Author: Fernando Perez
## Test this functions with: test_cachematrix.R

## makeCacheMatrix returns the functions to handle the matrix inversion
## set -> stores the original Matrix
## get -> recovers the original Matrix
## setInverse -> stores the Inverse Matrix
## getInverse -> recovers the Inverse Matrix

makeCacheMatrix <- function(mMat = matrix()) {

        
        mInv <- NULL
        
        set <- function(y) {
                mMat <<- y
                mInv <<- NULL
        }
        
        get <- function() { 
                mMat
        }
        
        setInverse <- function(inv) {
                mInv <<- inv
        }
        
        getInverse <- function() {
                mInv
        }
        
        ## Returns the list of 4 funtions to handle the matrix inversion
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve returns the inverse matrix of the input parameter matrix (mMat)
## if the inverse has already been solved, it is recovered from cache

cacheSolve <- function(mMat, ...) {
        
        ## 1. Get Inverse from cache
        mInv <- mMat$getInverse()
        
        # 2. If Inverse is not null (already computed), return Inverse
        if(!is.null(mInv)) {
                message("getting cached inverse matrix")
                return(mInv)
        }
        
        # 3. Get Matrix from Cache and compute Inverse, if Matrix is square
        dataMatrix <- mMat$get()
        if(dim(dataMatrix)[1] != dim(dataMatrix)[2]) {
                message("the argument must be a square matrix")
                return(NULL)
        }
        
        mInv <- solve(dataMatrix, ...)
        
        # 4. Store Inverse in cache
        mMat$setInverse(mInv)
        
        # 5. Return Inverse
        mInv
}

