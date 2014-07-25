## Put comments here that give an overall description of what your
## functions do
## I have to functions. The first one makeCacheMatrix define a class of matrix allowing to store 
## the inverse of the matrix and to recall it if needed.
## To instance a new object simply use my_matrix <- makeCacheMatrix(your_R_matrix)
## Then you can :
##      set the matrix to a new value using the "set" method : my_matrix$set(new_matrix)
##      get the matrix using the get method : my_matrix$get()
##      get the inverse using the getInv method : my_matrix$getInv()
##      set the inverse using the setInv method : my_matrix$setInv()


## Let's try to do a constructor (hum) and two methods...

makeCacheMatrix <- function(x = matrix()) {
        invmat <- NULL #used to later store the inv of the matrix.
        set <- function(y){
                invmat <<- NULL  #a new matrix is set, we haven't yet calculate its inverse, so invmat is set to NULL
                x <<- y # here y is the matrix received by the set function.
        }
        get <- function() x #obvious
        setInv <- function(inv) invmat <<- inv #obvious
        getInv <- function() invmat #obvious
        list(set = set, get = get, getInv=getInv, setInv=setInv) #obvious
}


## Let's create a function(!) that use an instance of the "class" defined above to get the cached inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmat <- x$getInv() #get the value stored in the environment of the instance.
        if(!is.null(invmat)){
                #invmat is not NULL so it's it means that the inv has already been calculated and stored in the cache.
                #let's return this value...
                message("getting cache data")
                return(invmat) #stop the function
        # This part of the code is executed only if #invmat is NULL. I would have put an else to make the code clearer but whatever let's mimic the code.
        }
        mat <- x$get() # To calculate the inverse we need the matrix...
        if(det(mat)){
                #the det is not 0 so the matrix can be inverted, let's invert it and store the result
                inv <- solve(mat)
                x$setInv(inv)
                inv
        }
        else {
                # the determinant of the matrix is 0, so the matrix can not be inversed.
                # we generate an error message.
                stop("The matrix is not inversible !")
        }
        
}

## Trying a new implementation of the class without a public setInv method because we shouldn't in OOP.
## Furthermore it will help me to better understand the environment logic as the initial exercice is a copy/paste of the example.
## This new class has only 3 methods :
##      set because we need a constructor
##      get obvious
##      getInv if the inv has been calculated then it retrieve the cached inv matrix, if not it try to calculate it and send back the result.
makeCacheMatrix2 <- function(x = matrix()) {
        invmat <- NULL
        set <- function(y){
                invmat <<- NULL
                x <<- y
        }
        get <- function() x
        getInv <- function(){
                # Seems logical to test if the inv has actually been calculated before returning the value
                # let's calculate it if it's not the case.
                # a self. would make it easyer
                if(is.null(invmat)){
                        # so invmat is null then the inverse hasn't been calculated yet, let's do it.
                        if(det(x)){
                                message("calculating") #message doesn't get printed. 
                                inv <- solve(x)
                                invmat <<- inv # and as we have calculated it let's cache it...
                                inv #just to return the value
                        }
                        else {
                                stop("The matrix is not inversible !")
                        }       
                }
                else {
                        #the inv has already been calculated. let's return it...
                        message("getting cache data") #message doesn't get printed...
                        invmat
                }
        }
        list(set = set, get = get, getInv = getInv)
}