## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#For both functions, used the test cases contained here at this url,https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg , in testing these functions: 


#This function, makeCacheMatrix, takes a matrix as input and returns a list of functions
#The returned list of functions "holds" the makeCacheMatrix variables, including m, in memory, for later use.

makeCacheMatrix <- function(x = matrix()) {
        #m holds, or will hold, the eventual inverse of x
        m <- NULL
        set <- function(y) {
                #sets x, in parent frame, to whatever y is
                x <<- y
                #resets m, the inverse, to null in parent frame
                m <<- NULL
        }
        
        #gets x
        get <- function() x
        
        #sets inverse, m, to be whatever the eventual calculated inverse that has been passed in is.
        setinverse <- function(inverse) m <<- inverse
        
        #gets inverse
        getinverse <- function() m
        
        #returns list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Write a short comment describing this function
#cacheSolve takes an object of makeCacheMatrix and returns a matrix that is an inverse of the input matrix to makeCacheMatrix.
#It first gets the value of m from the makeCacheMatrix object and then, if the inverse has already been calculated (m is not null) it returns that with an appropriate message
#If m is null then it gets the input matrix to the makeCacheMatrix and calculates the matrix inverse of it. 
#It then sets the inverse in the makeCacheMatrix object and returns the matrix inverse.

cacheSolve <- function(x, ...) {
        #assumes argument passed in is a makeCacheMatrix object
        ## Return a matrix that is the inverse of 'x'
        #pulls in whatever the inverse is if it was previously calculated. Pulls in NULL otherwise - since this was established in the makeCacheMatrix definition
        m <- x$getinverse()
        #if m is not null then we have previously calculated the inverse, so it prints the appropriate message to user and returns the inverse.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #otherwise we get the value of x, assign it to data, and run solve (to calculate the matrix inverse) on it and then assign that to m.
        data <- x$get()
        m <- solve(data, ...)
        #then we set the inverse in the makeCacheMatrix object to cache the value.
        x$setinverse(m)
        #return m, the matrix inverse.
        m
}


