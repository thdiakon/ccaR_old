#' @title Calculates the Corrected Covered Area (CCA) Index
#'
#' @description This package calculates the Corrected Covered Area (CCA) index. The measure for assessing the overall degree of overlap in an OOSR. It is taking as input the citation matrix.
#'
#' @param cm A dataframe for the citation matrix
#'
#' @return res
#'
#' @example man/examples/example1.R
#'
#' @export

cca <- function(cm){

    cm <- cm[, -1]

    studies<-nrow(cm)
    reviews<-ncol(cm)

    overlap_counts <- c()
    N <-c()
    r<-c()
    c<-c()
    CCA_Proportion<-c()
    CCA_Percentage<-c()

    j<-t(utils::combn(reviews,2))


    for (i in 1:nrow(j)){

        cm2<-cm[j[i,]]

        reviews[i]<-paste(colnames(cm2[1]), "vs." ,colnames(cm2[2]))

        overlap_counts[i] <- nrow(cm2[as.logical(rowSums(cm2) == 2), ])

        N[i]<- sum(cm2)

        r[i]<-nrow(cm2[as.logical(rowSums(cm2 != 0)), ])

        c[i]<-ncol(cm2)

        CCA_Proportion[i] <- (N[i]-r[i])/((r[i]*c[i])-r[i])

        CCA_Percentage[i] <- round(CCA_Proportion[i]*100, digits = 1)
    }

    overall<-nrow(j)+1
    reviews[overall]<-"Overall"
    overlap_counts[overall] <- " "
    N[overall]<-sum(cm)
    r[overall]<-nrow(cm)
    c[overall]<-ncol(cm)
    CCA_Proportion[overall] <- (N[overall]-r[overall])/((r[overall]*c[overall])-r[overall])
    CCA_Percentage[overall] <- round(CCA_Proportion[overall]*100, digits = 1)

    res<- data.frame(reviews,overlap_counts,
    N,r,c,CCA_Proportion,CCA_Percentage,stringsAsFactors=FALSE)

#    return(res)

}

