#############################################################################################################################################################
## Answers  #################################################################################################################################################
#############################################################################################################################################################

## So, the first object, makeCacheMatrix, creates a few things: x (a matrix), y, and 4 functions (set, get, setinverse, getinverse)
## The second object, cacheSolve, checks whether the object inv has anything stored in it (which it would if we've run cacheSolve before)
## And if it does then it will print the message "getting cached inverse" and then will return the saved value of cacheSolve.
## However, if the value of inv has been cleared or if we haven't run cacheSolve before, casheSolve will get the matrix from x and will caulculate and return the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinverse <- function(solve) {inv <<- solve}
        getinverse <- function() {inv}
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
}

testmat <- matrix(rnorm(10),3,3)
testmat1 <- makeCacheMatrix(m)
cacheSolve(testmat1)
cacheSolve(testmat1)

#############################################################################################################################################################
## Extra Work  ##############################################################################################################################################
#############################################################################################################################################################

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<-  NULL
        }
        get <- function() {x}
        setmean <- function(mean) {m <<- mean}
        getmean <- function() {m}
        list(set = set, get = get, setmean = setmean, getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
}

## Quick Detour  ############################################################################################################################################
#so the example they gave us was making a vector, now for the assignment, we have to create a matrix. So, let's think about what's different between creating a vector vs. a matrix.

#quick detour about lexical scoping to help in solving the assignment 
# Section 10.7: scope https://cran.r-project.org/doc/manuals/R-intro.html#Scope
cube <- function(n) {
        sq <- function() n*n
        return(n*sq())
}
cube(2)

?cat #concatenate and print

open.account <- function(total) {
        list(
                deposit = function(amount) {
                        if(amount <= 0)
                                stop("Deposits must be positive!\n")
                        total <<- total + amount
                        cat(amount, "deposited.  Your balance is", total, "\n\n")
                },
                withdraw = function(amount) {
                        if(amount > total)
                                stop("You don't have that much money!\n")
                        total <<- total - amount
                        cat(amount, "withdrawn.  Your balance is", total, "\n\n")
                },
                balance = function() {
                        cat("Your balance is", total, "\n\n")
                }
        )
}

## Above, "<<-" changes the value associated with "total." 

ross <- open.account(100)
robert <- open.account(200)

ross$withdraw(30)
ross$balance()
robert$balance()

ross$deposit(50)
ross$balance()
ross$withdraw(500)

## Back to Programming Assignment  ##############################################################################################################################################

myVector <- makeVector(1:15)

myVector$get() #shows the list of what x is: a vector of numbers 1:15

#illustration of how each works in R
aVector <- makeVector(1:10) # neccessary to have the input cachemean later be makeVector(), which is why we specify aVector in the way we do here.
aVector$get()               # retrieve the value of x
aVector$getmean()           # retrieve the value of m, which should be NULL
aVector$set(30:50)          # reset value with a new vector
cachemean(aVector)          # notice mean calculated is mean of 30:50, not 1:10
aVector$getmean()           # retrieve it directly, now that it has been cached

# first thing is to create an empty numeric vector (which what we're doing when we say "x equals numeric()") to store key info within make vector. Similar to how we had to create an empty data frame in the other function we were coding in that last assignment, I think.
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<-  NULL
        }
        get <- function() {x}
        setmean <- function(mean) {m <<- mean}
        getmean <- function() {m}
        list(set = set, get = get, setmean = setmean, getmean = getmean) # eg. gives the name "set" to the set() function just like we did with by(list(Country=...$...)) with aggregate funciton before.
}                                                                        # this allows us to call each with the "$" syntax later

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
}