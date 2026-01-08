# ==============================================================================
# VALIDITY FUNCTIONS FOR OPTIVAL
# ==============================================================================
# These functions implement the core model-based validity calculations
# following the methodology described in Burisch (1984) and extended
# in the OPTIVAL framework.
# ==============================================================================

#' Model-Based Incremental Validity (Burisch Method)
#' 
#' Computes the expected incremental contribution of each item to test validity
#' based on factor loadings and the extended congeneric measurement model.
#' 
#' @param selite Integer vector specifying the order in which items should
#'   be evaluated/added to the test
#' @param lam Numeric vector of item factor loadings
#' @param lamy Numeric scalar, criterion factor loading
#' 
#' @return A list with two components:
#'   \item{nite}{Integer vector from 1 to m (number of items)}
#'   \item{deltabur}{Numeric vector of expected incremental validity 
#'     contributions for each item position}
#' 
#' @details This function implements the model-based approach for determining
#' incremental validity based on equation (1) in the OPTIVAL methodology:
#' 
#' For validity to increase when adding item (n+1), the condition is:
#' r_(n+1),y > r_X,y * r_(n+1),X+ * correction_term
#' 
#' The function computes, for each item in the specified order:
#' 1. The expected direct contribution (term1): item-criterion covariance
#' 2. The expected redundancy (term2-term4): overlap with existing items
#' 3. The net incremental validity (deltabur = term1 - redundancy)
#' 
#' This model-based approach:
#' - Predicts diminishing returns as items are added
#' - Identifies the optimal stopping point (where incremental validity ≤ 0)
#' - Is more stable and generalizable than purely empirical selection
#' 
#' @references
#' Burisch, M. (1984). Approaches to personality inventory construction: 
#' A comparison of merits. American Psychologist, 39(3), 214-227.
#' 
#' @keywords internal
Burisch <- function(selite, lam, lamy) {
  m <- length(lam)
  lamord <- lam[selite]
  lamordfil <- lamord
  
  nite <- 1:m
  term1 <- numeric(m)
  term2nume <- numeric(m)
  term2 <- numeric(m)
  term3nume <- numeric(m)
  term3deno <- numeric(m)
  term3 <- numeric(m)
  tmpnumeS1 <- numeric(m)
  tmpnumeS2 <- numeric(m)
  tmpdenoS1 <- numeric(m)
  tmpdenoS2 <- numeric(m)
  tmpS1 <- numeric(m)
  tmpS2 <- numeric(m)
  
  sumlam <- 0
  sumclam <- numeric(m)
  
  # Main computation loop
  for (i in 1:m) {
    sumlam <- sumlam + lamord[i]
    
    if (i == 1) {
      sumprev <- 0
    } else {
      sumprev <- sumlam - lamord[i]
    }
    
    # Term 1: Direct item-criterion contribution
    term1[i] <- lamord[i] * lamy
    term2nume[i] <- sumprev * lamy
    
    # Term 2: Redundancy with previous items
    if (i == 1) {
      term2[i] <- 0
    } else {
      tmpnume1 <- lamordfil[1:(i-1)]
      TMPdeno1 <- outer(tmpnume1, tmpnume1)
      TMPdeno2 <- TMPdeno1 - diag(diag(TMPdeno1), nrow = nrow(TMPdeno1), ncol = ncol(TMPdeno1))
      TMPdeno3 <- TMPdeno2 + diag(i-1)
      tmpdeno <- sqrt(sum(TMPdeno3))
      term2[i] <- term2nume[i] / tmpdeno
    }
    
    # Term 3: Normalization factor
    term3nume[i] <- 1 + (lamord[i] * sumprev)
    tmpnume3 <- lamordfil[1:i]
    TMPdeno4 <- outer(tmpnume3, tmpnume3)
    TMPdeno5 <- TMPdeno4 - diag(diag(TMPdeno4), nrow = nrow(TMPdeno4), ncol = ncol(TMPdeno4))
    TMPdeno6 <- TMPdeno5 + diag(i)
    term3deno[i] <- sqrt(sum(TMPdeno6))
    term3[i] <- term3nume[i] / term3deno[i]
  }
  
  # Compute cumulative sums of loadings
  tmpsum <- 0
  for (i in 1:m) {
    for (j in 1:i) {
      tmpsum <- tmpsum + lamord[j]
    }
    sumclam[i] <- tmpsum
    tmpsum <- 0
  }
  
  # Compute correction terms
  tmpc1 <- 0
  tmpc2 <- 0
  for (i in 2:m) {
    for (j in 1:(i-1)) {
      tmpc1 <- tmpc1 + (1 + (lamord[j] * (sumclam[i] - lamord[j])))
      tmpc2 <- tmpc2 + (1 + (lamord[j] * (sumclam[i] - lamord[j] - lamord[i])))
    }
    tmpnumeS1[i] <- tmpc1
    tmpnumeS2[i] <- tmpc2
    tmpc1 <- 0
    tmpc2 <- 0
  }
  
  # Compute denominators
  for (i in 1:m) {
    tmpdenoc1 <- lamordfil[1:i]
    TMPdenoc2 <- outer(tmpdenoc1, tmpdenoc1)
    TMPdenoc3 <- TMPdenoc2 - diag(diag(TMPdenoc2), nrow = nrow(TMPdenoc2), ncol = ncol(TMPdenoc2))
    TMPdenoc4 <- TMPdenoc3 + diag(i)
    tmpdenoS1[i] <- sqrt(sum(TMPdenoc4))
    
    if (i == 1) {
      tmpdenoS2[i] <- 0
    } else {
      tmpdenoS2[i] <- tmpdenoS1[i-1]
    }
  }
  
  # Compute final correction terms
  for (i in 1:m) {
    if (i == 1) {
      tmpS1[i] <- 0
      tmpS2[i] <- 0
    } else {
      tmpS1[i] <- tmpnumeS1[i] / tmpdenoS1[i]
      tmpS2[i] <- tmpnumeS2[i] / tmpdenoS2[i]
    }
  }
  
  # Final incremental validity calculation
  DIFC <- tmpS1 - tmpS2
  term4 <- (term2 * DIFC) + (term2 * term3)
  deltabur <- term1 - term4
  
  return(list(nite = nite, deltabur = deltabur))
}

#' Empirical Incremental Validity from Data
#' 
#' Computes the empirical test validity as a function of test length by
#' sequentially adding items and correlating sum scores with the criterion.
#' 
#' @param X Numeric matrix of item scores (N x m)
#' @param y Numeric vector of criterion scores (N x 1)
#' @param selite Integer vector specifying the order in which items are added
#' @param lam Numeric vector of item factor loadings (not used in computation
#'   but kept for compatibility)
#' @param lamy Numeric scalar, criterion factor loading (not used in computation
#'   but kept for compatibility)
#' 
#' @return A numeric vector of length m containing the test-criterion correlation
#'   for each test length (1 to m items)
#' 
#' @details This function computes the empirical validity curve by:
#' 1. Reordering items according to selite
#' 2. For each test length k (from 1 to m):
#'    - Computing the sum score of the first k items
#'    - Correlating this sum score with the criterion
#' 3. Returning the sequence of correlations
#' 
#' This provides the empirical counterpart to the model-predicted validity
#' curve, allowing validation of model predictions.
#' 
#' @keywords internal
increvalsim <- function(X, y, selite, lam, lamy) {
  n <- nrow(X)
  m <- ncol(X)
  
  XORD <- X[, selite]
  lamord <- lam[selite]
  
  valiparc <- numeric(m)
  
  for (i in 1:m) {
    tmpsum <- rowSums(as.matrix(XORD[, 1:i, drop = FALSE]))
    tmpvali1 <- cor(tmpsum, y)
    valiparc[i] <- tmpvali1
  }
  
  return(valiparc)
}
