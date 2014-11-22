## Write a short comment describing this function
##makeCacheMatrix crates sort of a cointainer for our matrix
##it takes the matrix as indata and stores the matrix as it was when it was created
##and also the matrix when it is solved

makeCacheMatrix <- function(vMatrix = matrix()) {
		vSolvedMatrix <- NULL
        set <- function(vInMatrix) {
                vMatrix <<- vInMatrix				##sets the cache matrix to function indata
                vSolvedMatrix <<- NULL				##sets the solved version to NULL
        }
        get <- function() {vMatrix}					##returns the matrix
        setmatrix <- function(vInParam) {vSolvedMatrix <<- vInParam}	##defines setmatrix which stores the matrix in cache
        getmatrix <- function() {vSolvedMatrix}							##denfines getmatrix that returns the solvec matrix
		
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Write a short comment describing this function
##the function cacheSolve checks the makeCacheMatrix "object" to see if it has been solved
## and if not it solves the matrix, stores it and passes the result. If the matrix has been solved it
## returns the solved version from cache

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()				##gets the solved matrix or null
        if(!is.null(m)) {				## if not null then we'll use the cached matrix
                message("getting cached data")		
                return(m)		
        }
        data <- x$get()					##else get unsolved
        m <- solve(data, ...)			##solve it
        x$setmatrix(m)					##cache it
        m						
}

mat <- matrix(1:4, nrow=2,ncol= 2)		##use these lines to test code.
mat										##before
mcm <- makeCacheMatrix(mat)				##solve it
cacheSolve(mcm)							##after	
