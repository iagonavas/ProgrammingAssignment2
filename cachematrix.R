## The first function creates a list containing four functions, which are the functions set, get, setinverse and getinverse.
## It also stores the data as an object called x, which in this case is a matrix.
## It stores the information of x in the parent frame (cache) so it extends beyond this function and can be used later in the second function.
## Finally, it "clears" the cached inv (which is the objet I'll store the inverse matrix) by assigning NULL to it.

makeCacheMatrix <- function(x = matrix()) { ## creates a function with one argument, x, which by default is an empty matrix.
        inv <- NULL ## creates an object inv
        set <- function(y) { ## creates the function set, which just sends information to the cache
          x <<- y ## sends the imput to the cache object x so it can be used later
          inv <<- NULL ## "clears" the cached object inv
        }
        get <- function() x ## creates the function get, which just retrieves back x
        setinverse <- function(inverse) inv <<- inverse ## creates the function setinverse, which sends the input to the cache object inv
        getinverse <- function() inv ## function getinverse: retrieves inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## creates a list to be used as an argument to the second function. This needs to be a list so the subset operator $ will work.
}


## This function retrieves the matrix called in the first function and checks if there is an inverse matrix already calculated.
## If there is an inverse matrix calculated in cache, it just retrieves it. 
## If there is not, it calculates the inverse matrix using the function solve() and returns the result.

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() ## uses the function getinverse(), already defined above, to create an object in this environment based on what is in cache.
        if(!is.null(inv)) { ## asks if inv is not empty. 
            message("getting cached data") ## tells me that it is getting information from cached data and not calculating from scratch.
                return(inv) ## retrieves the cached data.
        }
        data <- x$get() ## creates an object from the object previously store in cache (the matrix previously defined).
        inv <- solve(data, ...) ## calculates the inverse matrix.
        x$setinverse(inv) ## stores the calculation in cache
        inv ## returns the calculated matrix.
}
