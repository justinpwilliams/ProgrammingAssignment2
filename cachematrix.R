## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix(), ...) {
        s <- NULL
        set <- function(y, ...) {
                x <<- y
                s <<- NULL
        }
        
        ## Returns the matrix passed to the function. Used to pull the actual
        ## matrix into the evaluation function 'cacheSolve'
        get <- function() x
        
        ## When called, setSol looks for 's' in the parent envrionment and sets
        ## it to the passed 'solution' matrix. If 's' is not found, 's' is set 
        ## in the global environment.
        setSol <- function(solution) s <<- solution
        
        ## This returns the current value of 's' in the global environment.
        getSol <- function() s
        list(set = set, get = get, 
             setSol = setSol, getSol = getSol)
}


## 'x' is the output of the special vector above
cacheSolve <- function(x, ...) {
        ## This loads the matix from the 
        s <- x$getSol()
        
        ## Check to see if the solution already exists in the passed special 
        ## matrix and returns that value to 's' if it does exist.
        if(!is.null(s)) {
                message("getting cached data")
                
                ## If found, return the stored solution and exit the entire 
                ## function, skipping the definition of a new solution.
                return(s)
        }
        
        ## Calling the get function of the special "matrix". This
        ## brings the list into the current environment
        data <- x$get()
        
        ## Calculating the inverse and storing it in this 
        ## environment in the variable 's'
        s <- solve(data, ...)
        
        ## Taking the inverse just calculated and pushing it to the                 
        ## parent environment for storage, aka cacheing it.
        x$setSol(s)
        
        ## Returning the solution
        s
}
