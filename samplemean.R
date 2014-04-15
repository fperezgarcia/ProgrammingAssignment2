makeVector <- function(x = numeric()) {
    ## x <- initial vector
    ## m <- mean of x
    ##
    
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() { 
        x
    }
    
    setmean <- function(mean) {
        m <<- mean
    }
    
    getmean <- function() {
        m
    }
    
    ## Returns a list of 4 funtions to handle the mean
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

##Testing the functions
sample <- rnorm(5000)
mVector <- makeVector(sample)
mMean <- cachemean(mVector)

## Display mean
print(mMean)

## Second time should retrieve m without computing it
mMean <- cachemean(mVector)
print(mMean)

## If Vector changes, mean is computed again
mVector <- makeVector(1:5000)
mMean <- cachemean(mVector)
print(mMean)
