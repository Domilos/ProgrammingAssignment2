makeCacheMatrix <- function(x = matrix()) { # input x : a matrix

        inv <- NULL  #  inv is our inversed matrix (reset to NULL everytime makeCacheMatrix is called)  

        set <- function(y) { # takes an input matrix
                x <<- y # saves the input matrix
                inv <<- NULL # resets the inversed matrix to NULL
        }

        get <- function() x # returns the value of the original

        setinv <- function(mean) inv <<- mean # store the value. 

        getinv <- function() inv # return the value. 

        list(set = set, get = get, # accessed each time makeVector() is called
             setinv = setinv,
             getinv = getinv)
}
cacheSolve <- function(x, ...) { # takes an input created bu makeCacheMatrix

        inv <- x$getinv()  # accesses the object 'x' and gets the inversed matrix

        if(!is.null(inv)) {  # if the inversed matrix was already cached 
                message("getting cached data")
                return(inv)
        }

        data <- x$get()

        inv <- solve(data, ...)  # if there are no cached we have to calculate the inverse

        x$setinv(inv)

        inv
}
