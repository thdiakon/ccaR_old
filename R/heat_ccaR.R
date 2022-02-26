#' @title Plots the cca heatmap
#'
#' @description This function plots the cca heatmap. The tiles within the triangular matrix contain color-coded data that demonstrate the degree of overlap between pairs of reviews using the corrected covered area (CCA) measure. It is taking as input the citation matrix, the font size of the text in the tiles and the color used in the heatmap.
#'
#' @param cm A dataframe for the citation matrix
#'
#' @param fontsize A number which controls the aesthetic of font size of the numbers in the tiles of the heatmap
#'
#' @param chroma The color of the heatmap
#'
#' @return heat_cca
#'
#' @example man/examples/example2.R
#'
#' @export
heat_cca <- function(cm, fontsize=5, chroma="#527e11"){

    c(missing(fontsize), missing(chroma))  
    
    # create a table with all the parameters
    V1<-c()
    V2<-c()
    CCA_Percentage<-0

    a <- cca(cm)
    cm <- cm[, -1]
    cm <- cm[, order(colnames(cm))]

    a2 <- a[-nrow(a),]

    j2 <- t(utils::combn(colnames(cm),2))

    colnames(j2) <- paste0("V", 1:2)

    data_hm <- cbind(j2, a2)


    # create a table with the number of primary studies for each SR

    V3 <- colnames(cm)
    V4 <- colnames(cm)
    r2 <- c()
    for (i in 1:ncol(cm)){
        r2[i] <- sum(cm[i])
        r2[i] <- paste(r2[i], "*")
    }


    data_hm2 <- data.frame(V3, V4, r2)



    # CCA heatmap --------------------------------------------------------------------------------




    heat_cca <- ggplot2::ggplot(data = data_hm, ggplot2::aes(x = V1, y = V2)) +
        ggplot2::theme_classic(base_size = 16) +
        ggplot2::geom_tile(ggplot2::aes(fill = CCA_Percentage), color='grey') +
        ggplot2::geom_tile(data = data_hm2, ggplot2::aes(x = V3, y = V4), fill = "grey", color='grey', inherit.aes = F) +
        ggplot2::coord_equal() +
        ggplot2::geom_text(ggplot2::aes(color = CCA_Percentage > 60, label = round(CCA_Percentage, 2)), size = fontsize) +
        ggplot2::geom_text(data = data_hm2, ggplot2::aes(x = V3, y = V4),  label = r2, size = fontsize, inherit.aes = F) +
        ggplot2::scale_fill_gradient(low="white", limits = c(0, 100),
                            breaks=c(0, 20, 40, 60, 80, 100), high=chroma,
                            name = "CCA (%)") +
        ggplot2::scale_color_manual(guide = "none", values = c("black", "white")) +
        ggplot2::labs(caption = "*total number of primary studies included in the review \nCCA: Corrected Covered Area") +
        ggplot2::theme(
            axis.title=ggplot2::element_blank(),
            axis.text.x=ggplot2::element_text(angle=90, vjust = 0.5, hjust=0.3
            ))

    return(heat_cca)
}
