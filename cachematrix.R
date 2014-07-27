## makeCacheMatrix function creates a special matrix object that can cache its inverse

## The first function makeCacheMatrix returns list of functions to perform
## 1. set: Setting the value of the matrix which should be invertible square matrix
## 2. get: Fetching the invertible matrix value when called
## 3. setinv : store the inverted matrix in cache memory
## 4. getinv : Fetch the value from cache

makeCacheMatrix <- function(x = matrix()) {

 
        # Exit if the matrix doesnot satisfy the properties of invertible matrix

        
        if((!is.matrix(y)) || (dim(y)[1]!= dim(y)[2]) || (det(y)== 0)){


	        stop("Please provide a invertible square matrix")

	}
        
        inv <- NULL  
        
	set <- function(y) {
       
                # Check if the 'y" is invertible square matrix and not singular


                if((is.matrix(y)) && (dim(y)[1]== dim(y)[2]) && (det(y)!=0)){
		        
		
		        x <<- y
		        
		        inv <<- NULL
               
	        }
               
	        else{
			
		        stop("Please provide a invertible square matrix")

		
	        }
        
        }
        
       
	
	 # Return invertible matrix to the calling function 
        
	 get <- function() x
        
        
         # Store the inverted matrix in cache
        
         setinv <- function(minv){ inv <<- minv }
        
        
         # REturn the inverted matrix from cache, it will return NULL if not stored in memory before
        
         getinv <- function() inv
        
        
         # Return list of function objects

         list(setmat = set, 
             
              getmat = get,
             
              setinv = setinv,
             
              getinv = getinv)


}




## Construct a function which can compute the inverse of a square matrix 
## only when the pre compute value is not available in cache.
## cacheSolve functions accepts the the list of functions return from makeCachematrix()

cacheSolve <- function(x, ...) {

   
# get the inversed matrix data from cache calling the getinv()
        
# from the list return by function makecachematrix()and store in object "cin"
	
	
        cin<-x$getinv()                  # fetch the matrix inverse value from cache
        
        if(!is.null(cin)){
	 
                message("retrieving existing inversed sqaure matrix from cache!!")
	    
	 	return(cin)              # return pre-inversed matrix,if exists
	    
	 } 
	 
        else{                            # If cache doesn't have pre calculated inverse matrix,then calculate
	    
         	mdata<-x$getmat()        # Fetch the input square matrix
                              
         	minv<-solve(mdata,...)   # inverse the matrix
	 	
                x$setinv(minv)           # Store the value in cache
	 	
                return(minv)             # Return value of computed inverse matrix
             
	 }

}



