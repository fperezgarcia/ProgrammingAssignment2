##Testing the functions
testMatrix  = matrix (rnorm(1:100), 10, 10)
mCacheVector <- makeCacheMatrix(testMatrix)
testMatrixInv <- cacheSolve(mCacheVector)

## Display testMatrix %*% testMatrixInv -> Shold be the identity matrix
print("Test 1. Should return the identity matrix")
print(testMatrix %*% testMatrixInv)

## Second time should retrieve inverse without computing it
print("Test 2. Already cached, should return the identity matrix")
testMatrixInv <- cacheSolve(mCacheVector)
print(testMatrix %*% testMatrixInv)

## If Vector changes, mean is computed again
## In this case as it is not a square matrix it should return an error
print("Test 3. Should return error: Not square matrix")
testMatrix  = matrix (rnorm(1:90), 10, 9)
mCacheVector <- makeCacheMatrix(testMatrix)
testMatrixInv <- cacheSolve(mCacheVector)