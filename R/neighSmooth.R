#' Euclidean neighbor smoothing
#'
#'
#' This function constructs a variable that for each event shows the average
#' value for its euclidean k-nearest neighbors. It builds on the same
#' idea as has been put forward in the Sconify package:
#' -Burns TJ (2019). Sconify: A toolkit for performing KNN-based statistics for
#' flow and mass cytometry data. R package version 1.4.0 and
#' -Hart GT, Tran TM, Theorell J, Schlums H, Arora G, Rajagopalan S, et al.
#' Adaptive NK cells in people exposed to Plasmodium falciparum correlate
#' with protection from malaria. J Exp Med. 2019 Jun 3;216(6):1280–90.
#' First, the k nearest neighbors are defined for cell x. Then, the average
#' value for the k nearest neighbors is returned as the result for cell x.
#' @param focusData The data that should be smoothed. Should be a matrix with
#' the variables to be smoothed as columns.
#' @param euclidSpaceData The data cloud in which the nearest neighbors for the
#' events should be identified. Can be a vector, matrix or dataframe. It is
#' worth noting that if this data has more than 10 dimensions, the first step
#' of the algorithm will be the creation of a 10-dimensional PCA using
#' fast.prcomp from gmodels. So in cases where this function is used iteratively,
#' it might be wiser to run the PCA beforehand.
#' @param neighRows The rows in the dataset that correspond to the neighbors
#' of the focusData points. "default" is all the focusData points, but a subset
#' can be added instead, if preferred. This is good to use to increase
#' robustness, e.g. by running 100 iterations with different sets of neighbors
#' with the same number of points from each group/individual.
#' @param ctrlRows Optionally, a set of control rows that are used to remove
#' background signal from the neighRows data before sending the data back.
#' @param kNeighK The number of nearest neighbors. "default" is the max of
#' 100 and the number of neighbor rows divided by 10000. Mutliple different
#' values here is preferred.
#' @param kMeansK The number of clusters in the initial step of the algorithm.
#' A higher number leads to shorter runtime, but potentially lower accuracy.
#' This is not used if kMeansCenters is provided. "default" is the highest of 1
#' and the number of cells in euclidSpaceData divided by 1000.
#' @param kMeansCenters Here, a pre-clustering of the data can be provided, in
#' which case the clustering will not be performed internally. Wise if for
#' example a bootstrapping scheme is used to define the neighRows iteratively,
#' as the k-means step can be quite time consuming. This part is the cluster
#' centers or centroids.
#' @param kMeansClusters See above. Here, the clusters, instead of the centroids
#' are provided if used.
#' @param method The method to use for the smoothing. Three values possible:
#' mean (default), median and mode.
#' @param nCores The number of cores used. Defaults to number of cores in the
#' computer minus 1.
#' @return An object of the same dimensions as focusData that has been smoothed.
#' @importFrom ClusterR KMeans_rcpp
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom foreach foreach %dopar%
#' @importFrom gmodels fast.prcomp
#' @importFrom FNN knnx.index
#' @examples
#' data(testData)
#' data(testDataSNE)
#' euclidSpaceData <-
#'     testData[, c(
#'         "SYK", "CD16", "CD57", "EAT.2",
#'         "CD8", "NKG2C", "CD2", "CD56"
#'     )]
#' \dontrun{
#' smoothGroupVector <- neighSmooth(
#'     focusData = as.numeric(testData$label),
#'     euclidSpaceData
#' )
#' }
#' @export neighSmooth
neighSmooth <- function(focusData, euclidSpaceData,
                        neighRows = "default",
                        ctrlRows = NULL,
                        kNeighK = "default",
                        kMeansK = "default",
                        kMeansCenters = NULL,
                        kMeansClusters = NULL,
                        method = "mean",
                        nCores = detectCores() - 1) {
    if (is.vector(focusData)) {
        focusData <- data.frame(focusData, 0)
        focusDataIsVector <- TRUE
    } else {
        focusDataIsVector <- FALSE
    }
    if(is.matrix(focusData)){
        focusData <- as.data.frame(focusData)
    }

    if (is.vector(euclidSpaceData)) {
        euclidSpaceData <- data.frame(euclidSpaceData, euclidSpaceData)
    } else if (is.matrix(euclidSpaceData)) {
        euclidSpaceData <- as.data.frame(euclidSpaceData)
    }
    # First, the dimensionality is reduced to 10 dimensions for the
    # euclidSpaceData
    if (ncol(euclidSpaceData) > 10) {
        dataRedDim <- fast.prcomp(euclidSpaceData)$x[, seq(1, 10)]
    } else {
        dataRedDim <- euclidSpaceData
    }
    if(is.character(neighRows) && neighRows == "default"){
        neighRows <- seq_len(nrow(as.matrix(focusData)))
        message("All events will be used as potential neighbors")
    }
    if(is.character(kNeighK) && kNeighK == "default"){
        kNeighK <- max(100,round(nrow(as.matrix(focusData))/10000))
        message("The number of neighbors will be ", kNeighK)
    }

    if(is.character(kMeansK) && kMeansK == "default" &&
       is.null(kMeansCenters)){
        kMeansK <- max(1, round(nrow(
            as.matrix(euclidSpaceData)
        ) / 1000))
        message("The number of k-means centers will be ", kMeansK)
    }

    # Now, the cells are clustered according to this analysis
    if(all(is.null(kMeansCenters) |
       is.null(kMeansClusters))){
        kMeansResult <- KMeans_rcpp(dataRedDim, kMeansK, max_iters = 100)
        kMeansCenters <- kMeansResult$centroids
        kMeansClusters <- kMeansResult$clusters
        message("Done with k-means")
    } else if(any(is.null(kMeansCenters) |
                  is.null(kMeansClusters))){
        message("Illogical input. Please provide both kMeansCenters ",
                "and kMeansClusters or else they will not be used")
    }

    # Here, the rows connected to the neighbors, the control neighbors or
    # neither are defined
    groupVec <- rep("none", nrow(dataRedDim))
    groupVec[neighRows] <- "neigh"
    if (missing(ctrlRows) == FALSE) {
        groupVec[ctrlRows] <- "ctrl"
    }

    rowNumbers <- seq_len(nrow(dataRedDim))

    focusDataClustList <- split(focusData, kMeansClusters)
    dataRedDimClustList <- split(as.data.frame(dataRedDim), kMeansClusters)
    groupVecClustList <- split(groupVec, kMeansClusters)
    rowNumList <- split(rowNumbers, kMeansClusters)

    if(nrow(kMeansCenters) <= 11){
        distCenters <- as.list(as.data.frame(
            t(knnx.index(kMeansCenters, kMeansCenters, nrow(kMeansCenters)))
        ))
    } else if (nrow(kMeansCenters) > 11 &
               nrow(kMeansCenters) < 100) {
        distCenters <- as.list(as.data.frame(
            t(knnx.index(kMeansCenters, kMeansCenters, 11))
        ))
    } else {
        #Here, the situations where the data contains very much data is
        #considered. Here, we consider neighbors in the ten percent
        #of the clusters lying closest to the cluster in question
        numClusts <- round(nrow(kMeansCenters)/10)
        distCenters <- as.list(as.data.frame(
            t(knnx.index(kMeansCenters, kMeansCenters, numClusts))
        ))
    }

    print("Now the first bit is done, and the iterative part takes off")

    cl <- parallel::makeCluster(nCores, type = "SOCK")
    registerDoSNOW(cl)

    allClusters <- names(focusDataClustList)
    firstCluster <- 1
    resultList <- list()
    x <- 1
    while (firstCluster <= length(allClusters)) {
        timeBefore <- Sys.time()
        if (((firstCluster + nCores) - 1) < length(allClusters)) {
            clusterRange <- firstCluster:((firstCluster + nCores) - 1)
        } else {
            clusterRange <- firstCluster:length(allClusters)
        }

        # Now, the datasets are constructed from this cluster range
        locDataRedDimClustList <- dataRedDimClustList[clusterRange]

        # Here, the neighbors and the control neighbors are found in the 11
        # closest clusters for each of the focus clusters.
        locDistCenters <- distCenters[clusterRange]

        neighCtrlNeighReturnList <- lapply(locDistCenters, function(y) {
            locNeighList <- lapply(y, function(z) {
                locNeigh <- dataRedDimClustList[[z]]
                locReturn <- focusDataClustList[[z]]
                locGroup <- groupVecClustList[[z]]
                list(
                    locNeigh[which(locGroup == "neigh"), ],
                    locNeigh[which(locGroup == "ctrl"), ],
                    locReturn[which(locGroup == "neigh"), ],
                    locReturn[which(locGroup == "ctrl"), ]
                )
            })
            locNeighNeigh <- as.matrix(do.call(
                "rbind",
                lapply(locNeighList, "[[", 1)
            ))
            locNeighReturn <- as.matrix(do.call(
                "rbind",
                lapply(locNeighList, "[[", 3)
            ))
            locCtrlNeigh <- as.matrix(do.call(
                "rbind",
                lapply(locNeighList, "[[", 2)
            ))
            locCtrlReturn <- as.matrix(do.call(
                "rbind",
                lapply(locNeighList, "[[", 4)
            ))
            list(locNeighNeigh, locNeighReturn, locCtrlNeigh, locCtrlReturn)
        })

        resultNeigh <- foreach(
            i = seq_along(locDataRedDimClustList),
            .packages = "DepecheR"
        ) %dopar%
            microClust(
                dataCenter = as.matrix(locDataRedDimClustList[[i]]),
                dataNeigh = neighCtrlNeighReturnList[[i]][[1]],
                dataReturn = neighCtrlNeighReturnList[[i]][[2]],
                method = method, k = kNeighK
            )
        if (missing(ctrlRows) == FALSE) {
            resultCtrl <- foreach(
                i = seq_along(locDataRedDimClustList),
                .packages = "DepecheR"
            ) %dopar%
                microClust(
                    dataCenter = as.matrix(locDataRedDimClustList[[i]]),
                    dataNeigh = neighCtrlNeighReturnList[[i]][[3]],
                    dataReturn = neighCtrlNeighReturnList[[i]][[4]],
                    method = method, k = kNeighK
                )

            neighCtrlDifference <- lapply(seq_along(resultNeigh), function(y) {
                resultNeigh[[y]] - resultCtrl[[y]]
            })
        } else {
            neighCtrlDifference <- resultNeigh
        }

        resultList[[x]] <- do.call("rbind", neighCtrlDifference)

        timeAfter <- as.numeric(Sys.time() - timeBefore)
        print(paste0(
            "Clusters ", clusterRange[1], " to ",
            clusterRange[length(clusterRange)],
            " smoothed in ", timeAfter, " ",
            attributes(timeAfter)$units, ". Now, ",
            length(allClusters) - clusterRange[length(clusterRange)],
            " clusters are left."
        ))
        firstCluster <- (clusterRange[length(clusterRange)] + 1)
        x <- x + 1
    }

    parallel::stopCluster(cl)
    fullResult <- do.call("rbind", resultList)
    fullResultOrdered <- fullResult[order(unlist(rowNumList)), ]
    if (focusDataIsVector) {
        fullResultOrdered <- fullResultOrdered[, 1]
    }
    return(fullResultOrdered)
}
