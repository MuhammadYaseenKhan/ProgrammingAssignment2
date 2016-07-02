## There are a pair of functions machCasheMatrix() and cacheSolve().
## Both are developed to find the inverse of a matrix and retaining a cached copy.
## Since, calculation of inverse of a matrix is a heavy process, 
## specially when matrix are in higher dimensions (say 200,500),
## it is better to maintain a cached version of inversed Matrix.
## If we are running it first time, inverse of matrix will be calcuated and cached,
## hence we can use this cached information.


## makeCacheMatrix(x) takes a matrix 'x' as an argument and create some associate functions set() and get()
## to set its value in partent enviornment and to get its value respectively. 
## Where in, there is defined a local variable 'im' which retains inverse of 'x'. 
## To access and modify 'im' there are associate funcions namely set.im() and get.im().
## One thing to keep in notice: 
## when you are assigning a new matrix 'x', the previous value of 'im' will be set to NULL

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL # initializing variable for inverse of matrix
    
    # sets the input matrix y to the variable x, 
    # and nullifies the variable 'im' in parent enviornment
    set <- function(y) { 
        x <<- y
        im <<- NULL
    }
    
    # returns the value of x
    get <- function(){
        x
    }
    
    # assigns the input variable 'm' to the varible 'im' in parent enviornment
    set.im <- function(m){
        im <<- m
    } 
    
    # returns the value of inverse of matrix
    get.im <- function(){
        im
    }
    
    #retruns the cached inverse matrix
    list(set = set, get = get, set.im = set.im, get.im = get.im)
}


## cacheSolve(x) takes a matrix 'x' as an input argument
## and returns a matrix that is the inverse of 'x', such that;
## it tries to access the cached value of 'im' (i.e. the inverse of matrix using get.im())
## if the returned value is NULL, then it accesses the matrix 'x' and executes the solve() function over it,
## and copies the result of solve() in a varible named 'm'.
## Then, it assignes the value of 'm' to the variable 'im' using the setter function of cached inverse matrix 'x'.
## Finally, it returns a matrix that is the inverse of 'x' 

cacheSolve <- function(x, ...) {
    # accessing the value of im
    m <- x$get.im()
    
    # checking whether it is NULL or not
    if(!is.null(m)) {
        # if found not NULL then returns the inverse of matrix that is m
        message("getting cached data")
        
        # since it is not the last line of function therefore, we are to use return() command explicitly.
        return(m)
    }
    
    # otherwise, if 'im' is not NULL it gets the matrix and calcualtes its inverse
    data <- x$get()
    m <- solve(data, ...)
    
    # caches the inverse of matrix
    x$set.im(m)
    
    # returns the inverse of matrix
    m
}

# matrix <- matrix(runif(9), nrow = 3, ncol = 3)
# matrix
# cache.matrix <- makeCacheMatrix(matrix)
# inverse.matrix <- cacheSolve(cache.matrix)
# inverse.matrix
