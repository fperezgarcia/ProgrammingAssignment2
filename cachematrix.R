## ProgrammingAssignment2
## Author: Fernando Perez


makeCacheMatrix <- function(mMat = matrix()) {
        ## 
        ## 
        ##
        
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
        
        ## Returns a list of 4 funtions to handle the mean
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(mMat, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        mInv <- mMat$getInverse()
        
        if(!is.null(mInv)) {
                message("getting cached inverse matrix")
                return(mInv)
        }
        
        dataMatrix <- mMat$get()
        if(dim(dataMatrix)[1] != dim(dataMatrix)[2]) {
                message("the argument must be a square matrix")
                return(NULL)
        }
        
        mInv <- solve(dataMatrix, ...)
        mMat$setInverse(mInv)
        
        mInv
}

