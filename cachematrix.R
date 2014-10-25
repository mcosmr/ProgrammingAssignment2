makeCacheMatrix <- function(x = matrix()) {			##create a list with functions and initialize local variables and cache variables
        l_m <- NULL                                          
        set <- function(y) {                                ##set function: set the value of the matrix
                c_x <<- y                                  	
                c_m <<- NULL                                
        }
        get <- function() c_x                               ## get function: get the value of the matrix from cache 
        set_c_m <- function(l_m) c_m <<- l_m   			## save the value of the inverse matrix in cache
        get_c_m <- function() c_m                       	## get the value of the inverse matrix from cache
        list(set = set, get = get,					## list of functions
             set_c_m = set_c_m,					 
             get_c_m = get_c_m)
}


cacheSolve <- function(x) {                  			
        l_m<- x$get_c_m()               				## Get the value for inverse matrix from cache and put it in a local m.
        if(!is.null(l_m)) {                 			## Check if it is NULL.  
                message("getting cached data")  		## If so, return of inverse matrix + message.
                return(l_m)
        }                                     			
        matrix <- x$get()               				## Select Function x$get in makeCacheMatrix to obtain the UNinverted matrix in order to calculate invers,                        
        inv_matrix <- solve(matrix)  				## solve()- calculate value of inverse matrix
        x$set_c_m(inv_matrix)           				## Store value of inverse matrix in cache variable 
        inv_matrix                           			## return inv_matrix
}
