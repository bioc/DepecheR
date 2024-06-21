#This function is used internally in the depeche function.
#allSolutions: this is an object that should be derived from the
#depechePenaltyOpt function output, which saves all the cluster matrices that
#are generated in the optimization process.
#selectionDataSet: the name speaks for itself
#k: same meaning as in depeche, i.e. the number of startpoint cluster centers.
depecheClusterCenterSelection <- function(allSolutions, selectionDataSet, k,
                                          nCores){
    # Now, all clusterCenters are used to
    # allocate the selectionDataSet.
    allocationResultList <-
        lapply(allSolutions, function(x) {
            dAllocate(inDataFrame = selectionDataSet, depModel =
                                   list("clusterCenters" = x,
                               "logCenterSd" = FALSE))
        })

    # Here, the corrected Rand index with
    # each allocationResult as the first
    # vector vector and all the others as
    # individual second vectors is identified
    cl <- makeCluster(nCores, type = "SOCK")
    registerDoSNOW(cl)
    meanARIList <-
        foreach(i = seq_len(length(allocationResultList))) %dopar%
        mean(vapply(allocationResultList,
                    FUN.VALUE = 0.5, rand_index,
                    inds2 = allocationResultList[[i]], k = k
        ))
    stopCluster(cl)
    meanARIVector <- unlist(meanARIList)

    # Now the solution being the most similar
    # to all the others is retrieved
    optimalClusterCenters <-
        unlist(allSolutions[[which(meanARIVector ==
                                       max(meanARIVector))[1]]])
    colnames(optimalClusterCenters) <- colnames(selectionDataSet)
    # Remove all rows and columns that do not
    # contain any information
    reducedClusterCenters <-
        optimalClusterCenters[
            which(rowSums(optimalClusterCenters) != 0),
            which(colSums(optimalClusterCenters) != 0)
            ]
    #If the clusters are separated by
    #one factor only, or if, by some strange coincidence, it has been possible to create
    #only one cluster, which should be impossible,
    #then we need to turn this back into a dataframe here.
    if(is.numeric(reducedClusterCenters)){
        if (length(which(rowSums(optimalClusterCenters) != 0)) == 1) {
            reducedClusterCenters <- as.data.frame(t(reducedClusterCenters))
        } else if (length(which(colSums(optimalClusterCenters) != 0)) == 1) {
            reducedClusterCenters <- data.frame(reducedClusterCenters)
        }
        colnames(reducedClusterCenters) <-
            colnames(optimalClusterCenters)[which(colSums(optimalClusterCenters) != 0)]
    }
    reducedClusterCenters
}
