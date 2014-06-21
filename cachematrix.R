## given a is an invertible matrix
## if you make an object holding a matrix like this : matObj <- makeCacheMatrix(a)
## calling like this cacheSolve(matObj) will return cached inverse of matrix if possible. 
## if no-cached data, it'll solve() for a that's stored in matObj
## it doesn't matter if a changes, what's stored in matObj will be used.

## Creates an object that holds a matrix "x" and 4 functions called set,get,setmean,getmean
## 
makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) {
		m <<- mean
	}
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


## returns the inverse of a matrix that is encapsulated in matObj. takes advantage of caching if available
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		  m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		message("uncached-> solving")
        data <- x$get()
        m <- solve(data, ...)
        x$setmean(m)
        m
}
