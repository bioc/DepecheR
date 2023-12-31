#' Wilcoxon rank-sum or signed rank test comparison of subject groups in a
#' dClust result
#'
#'
#' This function is used to compare groups of individuals from whom comparable
#' cytometry or other complex data has been generated.
#' @importFrom matrixStats rowMedians
#' @param xYData A dataframe or matrix with two columns. Each row contains
#' information about the x and y positition in the field for that observation.
#' @param idsVector Vector with the same length as xYData containing information
#' about the id of each observation.
#' @param groupVector Vector with the same length as xYData containing
#' information about the group identity of each observation.
#' @param clusterVector Vector with the same length as xYData containing
#' information about the cluster identity of each observation.
#' @param displayVector Optionally, if the dataset is very large and the SNE
#' calculation hence becomes impossible to perform for the full dataset, this
#' vector can be included. It should contain the set of rows from the data used
#' for statistics, that has been used to generate the xYData.
#' @param paired Defaults to FALSE, i.e. no assumption of pairing is made and
#' Wilcoxon rank sum-test is performed. If true, the software will by default
#' pair the first id in the first group with the firs id in hte second group
#' and so forth.
#' @param multipleCorrMethod Which method that should be used for adjustment
#' of multiple comparisons. Defaults to Benjamini-Hochberg,
#' but all other methods available in \code{\link{p.adjust}} can be used.
#' @param densContour If density contours should be created for the plot(s) or
#' not. Defaults to TRUE. a
#' @param plotName The main name for the graph and the analysis.
#' @param groupName1 The name for the first group
#' @param groupName2 The name for the second group
#' @param lowestPlottedP If multiple plots should be compared, it might be
#' useful to define a similar color scale for all plots, so that the same color
#' always means the same statistical value. A p-value that determines this can
#' be added here. Default is a p-value of 0.05. In cases where no datapoints
#' have any lower p-values than this, a Wilcoxon-statistic corresponding as
#' closely as possible to 0.05 will be identified with iterations of datasets
#' with the same size as indicated by hte group vector. If one value is
#' lowerthan 0.05, the wilcoxon statistic from this comparison is used instead.
#' @param title If there should be a title displayed on the plotting field.
#' As the plotting field is saved as a png, this title cannot be removed as an
#' object afterwards, as it is saved as coloured pixels. To simplify usage for
#' publication, the default is FALSE, as the files are still named, eventhough
#' no title appears on the plot.
#' @param plotDir If different from the current directory. If specified and
#' non-existent, the function creates it. If "." is specified, the plots will be
#' saved at the current directory.
#' @param bandColor The color of the contour bands. Defaults to black.
#' @param dotSize Simply the size of the dots. The default makes the dots
#' smaller the more observations that are included.
#' @param createOutput For testing purposes. Defaults to TRUE. If FALSE, no
#' plots are generated.
#' @seealso \code{\link{dColorPlot}}, \code{\link{dDensityPlot}},
#' \code{\link{dResidualPlot}}
#' @return This function always returns a dataframe showing the Wilcoxon
#' statistic and the p-value for each cluster, with an included adjustment for
#' multiple comparisons (see above). It also returns a sne based plot showing
#' which events that belong to a cluster dominated by the first or the second
#' group.
#' @examples
#'
#' # Load some data
#' data(testData)
#' \dontrun{
#' # Load or create the dimensions that you want to plot the result over.
#' # uwot::umap recommended due to speed, but tSNE or other method would
#' # work as fine.
#' data(testDataSNE)
#'
#' # Run the clustering function. For more rapid example execution,
#' # a depeche clustering of the data is inluded
#' # testDataDepeche <- depeche(testData[,2:15])
#' data(testDataDepeche)
#'
#' # Run the function
#' dWilcoxResult <- dWilcox(
#'     xYData = testDataSNE$Y, idsVector = testData$ids,
#'     groupVector = testData$label, clusterVector =
#'     testDataDepeche$clusterVector
#' )
#'
#' # Here is an example of how the display vector can be used.
#' subsetVector <- sample(1:nrow(testData), size = 10000)
#'
#' # Now, the SNE for this displayVector could be created
#' # testDataSubset <- testData[subsetVector, 2:15]
#' # testDataSNESubset <- Rtsne(testDataDisplay, pca=FALSE)$Y
#' # But we will just subset the testDataSNE immediately
#' testDataSNESubset <- testDataSNE$Y[subsetVector, ]
#'
#' # And now, this new SNE can be used for display, although all
#' # the data is used for the Wilcoxon calculations
#' dWilcoxResult <- dWilcox(
#'     xYData = testDataSNESubset, idsVector = testData$ids,
#'     groupVector = testData$label, clusterVector =
#'         testDataDepeche$clusterVector, displayVector = subsetVector
#' )
#' }
#' @export dWilcox
dWilcox <- function(xYData, idsVector, groupVector, clusterVector,
                    displayVector, paired = FALSE,
                    multipleCorrMethod = "BH", densContour = TRUE,
                    plotName = "default", groupName1 = unique(groupVector)[1],
                    groupName2 = unique(groupVector)[2], title = FALSE,
                    lowestPlottedP = 0.05, plotDir = ".", bandColor = "black",
                    dotSize = 500 / sqrt(nrow(xYData)), createOutput = TRUE) {
    if (plotDir != ".") {
        dir.create(plotDir)
    }

    if (length(unique(groupVector)) != 2) {
        stop("More or less than two groups are present. Please correct this.")
    }

    if (length(unique(idsVector)) < 8) {
        warning("NB! The number of unique ids is smaller than 8, so",
                " statisticac omparison is not suitable. Use dResidualPlot ",
                "instead to view differences.")
    }

    if (is.matrix(xYData)) {
        xYData <- as.data.frame(xYData)
    }

    if (plotName == "default") {
        plotName <- paste0(groupName1, "_vs_", groupName2)
    }

    # Here, the statistical evaluation is
    # performed.  First, the data is divided
    # into each group.
    clusterVectorGroup1 <- clusterVector[groupVector == unique(groupVector)[1]]
    clusterVectorGroup2 <- clusterVector[groupVector == unique(groupVector)[2]]
    idsVectorGroup1 <- as.character(idsVector[groupVector ==
        unique(groupVector)[1]])
    idsVectorGroup2 <- as.character(idsVector[groupVector ==
        unique(groupVector)[2]])

    # Now, a table with the percentage of
    # cells in each cluster for each
    # individual is created for both groups.

    clusterTable1 <-
        as.matrix(as.data.frame.matrix(table(
            clusterVectorGroup1,
            idsVectorGroup1
        )))
    clusterTable2 <-
        as.matrix(as.data.frame.matrix(table(
            clusterVectorGroup2,
            idsVectorGroup2
        )))

    # In the very unlikely event that there
    # is not a single observation for one
    # cluster from one of the groups, this
    # cluster is substituted to that table
    # with a row of zeros.
    if (nrow(clusterTable1) < length(unique(clusterVector))) {
        zeroMat <- matrix(data = 0, nrow = length(unique(clusterVector)) -
            nrow(clusterTable1), ncol = ncol(clusterTable1))

        colnames(zeroMat) <- colnames(clusterTable1)
        allRowNames <- as.character(sort(unique(clusterVector)))
        row.names(zeroMat) <- allRowNames[-which(allRowNames %in%
            row.names(clusterTable1))]
        # Here, rows are added to the cluster
        # table to make the number of rows the
        # same as the unique values of the
        # cluster vector.
        clusterTable1big <- rbind(clusterTable1, zeroMat)

        # The rows of the table are re-sorted
        clusterTable1bigResorted <-
            clusterTable1big[order(as.numeric(row.names(clusterTable1big))), ]
        clusterTable1 <- clusterTable1bigResorted
    }

    # And the same procedure is done for the
    # second group
    if (nrow(clusterTable2) < length(unique(clusterVector))) {
        zeroMat <- matrix(data = 0, nrow = length(unique(clusterVector)) -
            nrow(clusterTable2), ncol = ncol(clusterTable2))

        colnames(zeroMat) <- colnames(clusterTable2)
        allRowNames <- as.character(sort(unique(clusterVector)))
        row.names(zeroMat) <- allRowNames[-which(allRowNames %in%
            row.names(clusterTable2))]
        # Here, rows are added to the cluster
        # table to make the number of rows the
        # same as the unique values of the
        # cluster vector.
        clusterTable2big <- rbind(clusterTable2, zeroMat)

        # The rows of the table are re-sorted
        clusterTable2bigResorted <-
            clusterTable2big[order(as.numeric(row.names(clusterTable2big))), ]
        clusterTable2 <- clusterTable2bigResorted
    }

    countTable1 <- table(idsVectorGroup1)
    countTable2 <- table(idsVectorGroup2)

    clusterFractionsForAllIds1 <- clusterTable1
    clusterFractionsForAllIds2 <- clusterTable2

    for (i in seq_len(length(countTable1))) {
        x <- clusterTable1[, i] / countTable1[i]
        clusterFractionsForAllIds1[, i] <- x
    }

    for (i in seq_len(length(countTable2))) {
        x <- clusterTable2[, i] / countTable2[i]
        clusterFractionsForAllIds2[, i] <- x
    }

    # And here the statistical test is
    # performed for each cluster individually
    statisticList <- mapply(wilcox.test,
        as.data.frame.matrix(t(clusterFractionsForAllIds1)),
        as.data.frame.matrix(t(clusterFractionsForAllIds2)),
        MoreArgs = list(
            alternative = "two.sided",
            paired = paired, exact = FALSE
        ),
        SIMPLIFY = FALSE
    )

    # Now, the statistics and the p-values
    # are retrieved
    statistic <- unlist(lapply(statisticList, `[[`, 1))
    p_values <- unlist(lapply(statisticList, `[[`, 3))

    # Here, adjustments for multiple
    # comparisons are performed
    p_adjusted <- p.adjust(p_values, method = multipleCorrMethod)

    # Now the median for each group and
    # cluster is calculated
    median1 <- 100 * rowMedians(clusterFractionsForAllIds1)
    median2 <- 100 * rowMedians(clusterFractionsForAllIds2)

    # Combine the four
    result <- data.frame(
        as.numeric(names(p_values)),
        median1, median2, statistic, p_values, p_adjusted
    )
    row.names(result) <- c(seq_len(nrow(result)))
    colnames(result) <- c(
        "Cluster", paste0("Median_%_for_", groupName1),
        paste0("Median_%_for_", groupName2),
        "Wilcoxon_statistic", "p-value",
        paste0(multipleCorrMethod, "_corrected_p-value")
    )

    # Here, a vector with the same length as
    # the cluster vector is generated, but
    # where the cluster info has been
    # substituted with the statistic.
    # Here, the p-values are transformed to
    # be useful for plotting
    medianClustDiff <- median1 - median2
    p_adjusted_log <- log10(p_adjusted)
    p_adjusted_log_inv <- p_adjusted_log
    pVector <- clusterVector
    for (i in seq_len(length(p_adjusted_log))) {
        if (medianClustDiff[i] > 0) {
            p_adjusted_log_inv[i] <- -p_adjusted_log[i]
        }
        pVector[clusterVector == result$Cluster[i]] <- p_adjusted_log_inv[i]
    }

    # If a
    # displayVector has been included, it is
    # used here, to subset the clusterVector
    if (missing(displayVector) == FALSE) {
        pVector <- pVector[displayVector]
    }

    # Here the data that will be used for
    # plotting is scaled.
    colnames(xYData) <- c("V1", "V2")

    # Here, the maximum values for the
    # plotting are defined. If not added by
    # the user, they are obtained from the
    # data.

    if (min(p_adjusted) < lowestPlottedP) {
        message(
            "NB!, The lowest p-value with this dataset was ",
            min(p_adjusted), ". Therefore, this p-value will define the color",
            " scale instead than the chosen value of ", lowestPlottedP, "."
        )
        lowestPlottedP <- min(p_adjusted)
    }

    # Now, the lowest value is
    # log-transformed
    lowestPlottedPLog <- log10(lowestPlottedP)
    # Here, the breaks for the color
    # interpretation are created
    brks <- seq(lowestPlottedPLog, -lowestPlottedPLog, length.out = 10)

    # assign each value to a bin
    grps <- cut(pVector, breaks = brks, include.lowest = TRUE)
    colors <- colorRampPalette(c("#FF0000", "white", "#0000FF"))(9)
    xYData$col <- colors[grps]

    dPlotCoFunction(
        colorVariable = xYData$col, plotName =
            paste0(plotName, "_Wilcox_result"),
        xYData = xYData, title = title,
        densContour = densContour, bandColor = bandColor,
        dotSize = dotSize, plotDir = plotDir,
        createOutput = createOutput
    )

    # Create a color legend with text
    # Here the scale is created
    scaleHighPart <- 10^seq(0, lowestPlottedPLog, len = 3)
    scaleLowPart <- rev(scaleHighPart[2:3])
    plotScale <- c(scaleLowPart, scaleHighPart)

    yname <- paste(multipleCorrMethod, " corrected p-values", sep = "")
    topText <- paste(groupName1, " is more abundant", sep = "")
    bottomText <- paste(groupName2, " is more abundant", sep = "")

    if (createOutput) {
        pdf(file.path(plotDir, paste0(
            plotName,
            "_Wilcoxon_scale.pdf"
        )))
        par(fig = c(0.35, 0.65, 0, 1), xpd = NA)
        z <- matrix(seq_len(9), nrow = 1)
        x <- 1
        y <- seq(lowestPlottedPLog, -lowestPlottedPLog, length.out = 9)
        image(x, y, z, col = colors, axes = FALSE, xlab = "", ylab = yname)
        text(0.32, -lowestPlottedPLog, labels = (round(plotScale[5],
            digits = 5
        )))
        text(0.32, -lowestPlottedPLog / 2, labels = (round(plotScale[4],
            digits = 5
        )))
        text(x = 0.32, y = 0, labels = 1)
        text(0.32, lowestPlottedPLog / 2, labels = (round(plotScale[2],
            digits = 5
        )))
        text(0.32, lowestPlottedPLog, labels = (round(plotScale[1],
            digits = 5
        )))
        text(1, -lowestPlottedPLog * 1.2, labels = topText, cex = 1.1)
        text(1, lowestPlottedPLog * 1.2, labels = bottomText, cex = 1.1)
        box()
        dev.off()
    }

    if (createOutput) {
        write.csv(result, file.path(plotDir, paste0(
            plotName,
            "_WilcoxResult.csv"
        )))
    }

    return(result)
}
