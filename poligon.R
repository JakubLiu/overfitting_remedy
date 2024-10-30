CovMatrix <- function(vector_of_vectors){
    sig <- matrix(rep(0,length(vector_of_vectors)*length(vector_of_vectors)),
                nrow = length(vector_of_vectors),
                ncol = length(vector_of_vectors))

    for(i in 1:length(vector_of_vectors)){
        for(j in 1:length(vector_of_vectors)){
            if(i == j){
                sig[i,j] <- var(vector_of_vectors[[i]])
            }
            else{
                sig[i,j] <- cov(vector_of_vectors[[i]], vector_of_vectors[[j]])
            }
        }
    }
    return(sig)
}

v1 <- rnorm(10,0,1)
v2 <- rnorm(10,0,1)
list <- list(v1,v2)
#list[1]
#list[[1]]
#list[[1]][1]
CovMatrix(list(v1,v2))