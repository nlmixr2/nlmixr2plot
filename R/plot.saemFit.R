#' Plot an SAEM model fit
#'
#' Plot an SAEM model fit
#'
#' @param x a saemFit object
#' @param ... others
#' @return a list
#' @author Wenping Wang
#' @export
#' @importFrom ggplot2 .data
plot.saemFit <- function(x, ...) {
  fit <- x
  saem.cfg <- attr(fit, "saem.cfg")
  ## .env$model$assignPtr()
  .evtM <- saem.cfg$evtM
  dat <- as.data.frame(saem.cfg$evt)
  dat <- cbind(dat[dat$EVID == 0, ], DV = saem.cfg$y)
  df <- rbind(cbind(dat, grp = 1), cbind(dat, grp = 2), cbind(dat, grp = 3))
  dopred <- attr(fit, "dopred")
  yp <- dopred(fit$mprior_phi, saem.cfg$evt, saem.cfg$opt)
  yi <- dopred(fit$mpost_phi, saem.cfg$evt, saem.cfg$opt)
  df$DV[df$grp == 2] <- yp
  df$DV[df$grp == 3] <- yi
  df0 <- df
  
  m <- fit$par_hist
  df <- data.frame(val = as.vector(m), par = rep(1:ncol(m), each = nrow(m)
  ), iter = rep(1:nrow(m), ncol(m)))
  p1 <- ggplot2::ggplot(df, ggplot2::aes(.data$iter, .data$val)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(~par, scales = "free_y")
  print(p1)
  
  for (cmt in sort(unique(df0$CMT))) {
    df <- df0[df0$CMT == cmt, ]
    dfG1 <- df[df$grp == 1, ]
    dfG2 <- df[df$grp == 2, ]
    dfG3 <- df[df$grp == 3, ]
    
    p6 <-
      ggplot2::ggplot(dfG1, ggplot2::aes(.data$TIME, .data$DV)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~ID) +
      ggplot2::geom_line(ggplot2::aes(.data$TIME, .data$DV), dfG2, col = "blue") +
      ggplot2::geom_line(ggplot2::aes(.data$TIME, .data$DV), dfG3, col = "red")
    
    df <- cbind(dfG1, PRED = dfG2[, "DV"])
    df$RES <- df$DV - df$PRED
    p2 <-
      ggplot2::ggplot(df, ggplot2::aes(.data$PRED, .data$DV)) +
      ggplot2::geom_point() +
      ggplot2::geom_abline(
        intercept = 0,
        slope = 1, col = "red"
      )
    p3 <-
      ggplot2::ggplot(df, ggplot2::aes(.data$PRED, .data$RES)) +
      ggplot2::geom_point() +
      ggplot2::geom_abline(
        intercept = 0,
        slope = 0, col = "red"
      )
    
    df <- df0[df0$CMT == cmt, ]
    df <- cbind(dfG1, IPRED = dfG3[, "DV"])
    df$IRES <- df$DV - df$IPRED
    p4 <-
      ggplot2::ggplot(df, ggplot2::aes(.data$IPRED, .data$DV)) +
      ggplot2::geom_point() +
      ggplot2::geom_abline(
        intercept = 0,
        slope = 1, col = "red"
      )
    p5 <-
      ggplot2::ggplot(df, ggplot2::aes(.data$IPRED, .data$IRES)) +
      ggplot2::geom_point() +
      ggplot2::geom_abline(
        intercept = 0,
        slope = 0, col = "red"
      )
    
    print(p2)
    print(p4)
    print(p3)
    print(p5)
    print(p6)
  }
  invisible(NULL)
}
