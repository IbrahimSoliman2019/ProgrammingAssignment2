## this function calculates the inverse of the matrix and cache the value so this will save time
##as the computation of amtrix matrix is a costly computation
  
## makeCacheMatrix creates a special "matrix", which is really a list containing a function to

##set the value of the matrix

##get the value of the matrix

##set the value of the inverse

##get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  
      inver<-NULL
      set <- function(y) {
        x <<- y
        inver <<- NULL
      }
      get <- function() x
      setinverse <- function(Inverse) inver <<- Inverse
      getinverse <- function() inver
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)

}


## This function computes the inverse of the special "matrix"
##returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

      cacheSolve <- function(x, ...) {
                  ## Return a matrix that is the inverse of 'x'
            inver <- x$getinverse()
            if(!is.null(inver)) {
              message("getting cached data")
              return(inver)
            }
            data <- x$get()
            m <- inv(t(da))
            x$setinverse(m)
            m
}
