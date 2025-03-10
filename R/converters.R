#' Convert BDI-I scores to a depression common metric
#'
#' This function allows you to convert scores of the Beck Depression Inventory,
#' first version (BDI-I) to a depression "common metric" score. Common metric scores
#' can be "reconverted" to the original scale by appending `.rev` to the respective
#' converter function.
#'
#' @details Conversions are conducted using the crosswalk tables provided in
#' Wahl et al. ([2014](https://doi.org/10.1016/j.jclinepi.2013.04.019); supplement).
#' Common metric scores _θ_ are normed so that values of 50 indicate the general
#' population mean, with a population standard deviation of 10.
#'
#' @param x A scalar or vector of scores that should be converted.
#' @usage
#' bdi1(x)
#' bdi1.rev(x)
#'
#' @export bdi1 bdi1.rev
#' @aliases bdi1.rev
#' @importFrom stats approxfun
#' @import utils
#' @examples
#' \dontrun{
#' x <- runif(100,0,30)
#' bdi1(x)
#' }

bdi1 = function(x) {
  res = approxfun(y = cm.bdi1$Theta,
                  x = cm.bdi1$Sum.Scores,
                  method = "linear",
                  rule = 2)(x)
  return(res)
}

bdi1.rev = function(x) {
  res = approxfun(x = cm.bdi1$Theta,
                  y = cm.bdi1$Sum.Scores,
                  method = "linear",
                  rule = 2)(x)
  return(res)
}



#' Convert BDI-II scores to a depression common metric
#'
#' This function allows you to convert scores of the Beck Depression Inventory,
#' second version (BDI-II) to a depression "common metric" score. Common metric scores
#' can be "reconverted" to the original scale by appending `.rev` to the respective
#' converter function.
#'
#' @details Conversions are conducted using the crosswalk tables provided in
#' Wahl et al. ([2014](https://doi.org/10.1016/j.jclinepi.2013.04.019); supplement).
#' Common metric scores _θ_ are normed so that values of 50 indicate the general
#' population mean, with a population standard deviation of 10.
#'
#' @param x A scalar or vector of scores that should be converted.
#' @usage
#' bdi2(x)
#' bdi2.rev(x)
#'
#' @export bdi2 bdi2.rev
#' @aliases bdi2.rev
#' @importFrom stats approxfun
#' @import utils
#' @examples
#' \dontrun{
#' x <- runif(100,0,30)
#' bdi2(x)
#' }

bdi2 = function(x) {
  res = approxfun(y = cm.bdi2$Theta,
                  x = cm.bdi2$Sum.Scores,
                  method = "linear",
                  rule = 2)(x)
  return(res)
}

bdi2.rev = function(x) {
  res = approxfun(x = cm.bdi2$Theta,
                  y = cm.bdi2$Sum.Scores,
                  method = "linear",
                  rule = 2)(x)
  return(res)
}


#' Convert CES-D scores to a depression common metric
#'
#' This function allows you to convert scores of the Center for Epidemiological Studies'
#' Depression Scale (CES-D) to a depression "common metric" score. Common metric scores
#' can be "reconverted" to the original scale by appending `.rev` to the respective
#' converter function.
#'
#' @details Conversions are conducted using the crosswalk tables provided in
#' Wahl et al. ([2014](https://doi.org/10.1016/j.jclinepi.2013.04.019); supplement).
#' Common metric scores _θ_ are normed so that values of 50 indicate the general
#' population mean, with a population standard deviation of 10.
#'
#' @param x A scalar or vector of scores that should be converted.
#' @usage
#' cesd(x)
#' cesd.rev(x)
#'
#' @export cesd cesd.rev
#' @aliases cesd.rev
#' @importFrom stats approxfun
#' @import utils
#' @examples
#' \dontrun{
#' x <- runif(100,0,30)
#' cesd(x)
#' }

cesd = function(x) {
  res = approxfun(y = cm.cesd$Theta,
                  x = cm.cesd$Sum.Scores,
                  method = "linear",
                  rule = 2)(x)
  return(res)
}

cesd.rev = function(x) {
  res = approxfun(x = cm.cesd$Theta,
                  y = cm.cesd$Sum.Scores,
                  method = "linear",
                  rule = 2)(x)
  return(res)
}



#' Convert WHO-5 scores to a depression common metric
#'
#' This function allows you to convert scores of the WHO-Five Wellbeing Index
#' to a depression "common metric" score. Common metric scores
#' can be "reconverted" to the original scale by appending `.rev` to the respective
#' converter function.
#'
#' @details Conversions are conducted using the crosswalk tables provided in
#' Wahl et al. ([2014](https://doi.org/10.1016/j.jclinepi.2013.04.019); supplement).
#' Common metric scores _θ_ are normed so that values of 50 indicate the general
#' population mean, with a population standard deviation of 10.
#'
#' @param x A scalar or vector of scores that should be converted.
#' @usage
#' who5(x)
#' who5.rev(x)
#'
#' @export who5 who5.rev
#' @aliases who5.rev
#' @importFrom stats approxfun
#' @import utils
#' @examples
#' \dontrun{
#' x <- runif(100,0,30)
#' who5(x)
#' }

who5 = function(x) {
  res = approxfun(y = cm.who5$Theta,
                  x = cm.who5$Sum.Scores,
                  method = "linear",
                  rule = 2)(x)
  return(res)
}

who5.rev = function(x) {
  res = approxfun(x = cm.who5$Theta,
                  y = cm.who5$Sum.Scores,
                  method = "linear",
                  rule = 2)(x)
  return(res)
}



#' Convert PHQ-9 scores to a depression common metric
#'
#' This function allows you to convert scores of the 9-item Patient Health Questionnaire
#' to a depression "common metric" score. Common metric scores
#' can be "reconverted" to the original scale by appending `.rev` to the respective
#' converter function.
#'
#' @details Conversions are conducted using the crosswalk tables provided in
#' Wahl et al. ([2014](https://doi.org/10.1016/j.jclinepi.2013.04.019); supplement).
#' Common metric scores _θ_ are normed so that values of 50 indicate the general
#' population mean, with a population standard deviation of 10.
#'
#' @param x A scalar or vector of scores that should be converted.
#' @usage
#' phq9(x)
#' phq9.rev(x)
#'
#' @export phq9 phq9.rev
#' @aliases phq9.rev
#' @importFrom stats approxfun
#' @import utils
#' @examples
#' \dontrun{
#' x <- runif(100,0,30)
#' phq9(x)
#' }

phq9 = function(x) {
  res = approxfun(y = cm.phq$Theta,
                  x = cm.phq$Sum.Scores,
                  method = "linear",
                  rule = 2)(x)
  return(res)
}

phq9.rev = function(x) {
  res = approxfun(x = cm.phq$Theta,
                  y = cm.phq$Sum.Scores,
                  method = "linear",
                  rule = 2)(x)
  return(res)
}


#' Convert PROMIS-8a scores to a depression common metric
#'
#' This function allows you to convert PROMIS-8a sum scores
#' to a depression "common metric". Common metric scores
#' can be "reconverted" to the original scale by appending `.rev` to the respective
#' converter function.
#'
#' @details Conversions are conducted using the crosswalk tables provided in
#' Wahl et al. ([2014](https://doi.org/10.1016/j.jclinepi.2013.04.019); supplement).
#' Common metric scores _θ_ are normed so that values of 50 indicate the general
#' population mean, with a population standard deviation of 10.
#'
#' @param x A scalar or vector of scores that should be converted.
#' @usage
#' promis8(x)
#' promis8.rev(x)
#'
#' @export promis8 promis8.rev
#' @aliases promis8.rev
#' @importFrom stats approxfun
#' @import utils
#' @examples
#' \dontrun{
#' x <- runif(100,0,30)
#' promis8(x)
#' }

promis8 = function(x) {
  res = approxfun(y = cm.promis8$Theta,
                  x = cm.promis8$Sum.Scores,
                  method = "linear",
                  rule = 2)(x)
  return(res)
}

promis8.rev = function(x) {
  res = approxfun(x = cm.promis8$Theta,
                  y = cm.promis8$Sum.Scores,
                  method = "linear",
                  rule = 2)(x)
  return(res)
}



#' Convert EPDS scores to a depression common metric
#'
#' This function allows you to convert scores of the Edinburgh Postnatal Depression Scale
#' to a depression "common metric". Common metric scores
#' can be "reconverted" to the original scale by appending `.rev` to the respective
#' converter function.
#'
#' @details Conversions are conducted using the crosswalk tables provided in
#' Wahl et al. ([2014](https://doi.org/10.1016/j.jclinepi.2013.04.019); supplement)
#' and Blackwell et al. ([2021](https://doi.org/10.1037/pas0001009)).
#' Common metric scores _θ_ are normed so that values of 50 indicate the general
#' population mean, with a population standard deviation of 10.
#'
#' @param x A scalar or vector of scores that should be converted.
#' @usage
#' epds(x)
#' epds.rev(x)
#'
#' @export epds epds.rev
#' @aliases epds.rev
#' @importFrom stats approxfun
#' @import utils
#' @examples
#' \dontrun{
#' x <- runif(100,0,30)
#' epds(x)
#' }

epds = function(x) {
  cm.epds$Theta[nrow(cm.epds)] = cm.epds$Theta[nrow(cm.epds)]+0.1
  res = approxfun(y = cm.epds$Theta,
                  x = cm.epds$Sum.Scores,
                  method = "linear",
                  rule = 2)(x)
  return(res)
}

epds.rev = function(x) {
  cm.epds$Theta[nrow(cm.epds)] = cm.epds$Theta[nrow(cm.epds)]+0.1
  res = suppressWarnings(
    approxfun(x = cm.epds$Theta,
                  y = cm.epds$Sum.Scores,
                  method = "linear",
                  rule = 2)(x))
  return(res)
}



#' Convert HADS-D scores to a depression common metric
#'
#' This function allows you to convert scores of the Hospital Anxiety and Depression Scale (HADS)
#' depression subscale
#' to a depression "common metric" score. Common metric scores
#' can be "reconverted" to the original scale by appending `.rev` to the respective
#' converter function.
#'
#' @details Conversions are conducted using the crosswalk tables provided in
#' Wahl et al. ([2014](https://doi.org/10.1016/j.jclinepi.2013.04.019); supplement).
#' Common metric scores _θ_ are normed so that values of 50 indicate the general
#' population mean, with a population standard deviation of 10.
#'
#' @param x A scalar or vector of scores that should be converted.
#' @usage
#' hadsd(x)
#' hadsd.rev(x)
#'
#' @export hadsd hadsd.rev
#' @aliases hadsd.rev
#' @importFrom stats approxfun
#' @import utils
#' @examples
#' \dontrun{
#' x <- runif(100,0,30)
#' hadsd(x)
#' }

hadsd = function(x) {
  res = approxfun(y = cm.hadsd$Theta,
                  x = cm.hadsd$Sum.Scores,
                  method = "linear",
                  rule = 2)(x)
  return(res)
}

hadsd.rev = function(x) {
  res = approxfun(x = cm.hadsd$Theta,
                  y = cm.hadsd$Sum.Scores,
                  method = "linear",
                  rule = 2)(x)
  return(res)
}



#' Convert HAM-D/HDRS-17 scores to a depression common metric
#'
#' This function allows you to convert scores of the 17-item Hamilton Depression Rating Scale
#' to a depression "common metric" score. Common metric scores
#' can be "reconverted" to the original scale by appending `.rev` to the respective
#' converter function.
#'
#' @details Conversions are conducted using the crosswalk tables provided in
#' Wahl et al. ([2014](https://doi.org/10.1016/j.jclinepi.2013.04.019); supplement).
#' Common metric scores _θ_ are normed so that values of 50 indicate the general
#' population mean, with a population standard deviation of 10.
#'
#' The HAM-D/HDRS-17 is not part of the original depression common metric model.
#' To convert scores, values are first transformed to BDI-I equivalents using the
#' equipercentile linking crosswalk developed by Furukawa et al.
#' ([2020](https://doi.org/10.1017/S2045796019000088)). Transformed scores should therefore be
#' interpreted cautiously.
#'
#' @param x A scalar or vector of scores that should be converted.
#' @usage
#' hamd(x)
#' hamd.rev(x)
#'
#' @export hamd hamd.rev
#' @aliases hamd.rev
#' @importFrom stats approxfun
#' @import utils
#' @examples
#' \dontrun{
#' x <- runif(100,0,30)
#' hamd(x)
#' }

hamd = function(x) {
  f.hamd.bdi = approxfun(x = dat.furukawa[,"hdrs.17"],
                         y = dat.furukawa[,"bdi.1"],
                         method = "linear",
                         rule = 2)
  x = f.hamd.bdi(x)
  res = approxfun(y = cm.bdi1$Theta,
                  x = cm.bdi1$Sum.Scores,
                  method = "linear", rule = 2)(x)
  return(res)
}

hamd.rev = function(x) {
  f.bdi.hamd = approxfun(y = dat.furukawa[,"hdrs.17"],
                         x = dat.furukawa[,"bdi.1"],
                         method = "linear",
                         rule = 2)
  res = approxfun(x = cm.bdi1$Theta,
                  y = cm.bdi1$Sum.Scores,
                  method = "linear",
                  rule = 2)(x)
  res = f.bdi.hamd(res)
  return(res)
}



#' Convert MÅDRS scores to a depression common metric
#'
#' This function allows you to convert scores of the Montgomery-Åsberg Depression Rating Scale
#' to a depression "common metric" score. Common metric scores
#' can be "reconverted" to the original scale by appending `.rev` to the respective
#' converter function.
#'
#' @details Conversions are conducted using the crosswalk tables provided in
#' Wahl et al. ([2014](https://doi.org/10.1016/j.jclinepi.2013.04.019); supplement).
#' Common metric scores _θ_ are normed so that values of 50 indicate the general
#' population mean, with a population standard deviation of 10.
#'
#' The MÅDRS is not part of the original depression common metric model.
#' To convert scores, values are transformed to HAM-D and then BDI-I equivalents using the
#' equipercentile linking crosswalk developed by Furukawa et al.
#' ([2020](https://doi.org/10.1017/S2045796019000088)), and the IRT-based crosswalk by Carmody et al.
#' ([2006](10.1016/j.euroneuro.2006.04.008)). Transformed scores should therefore be
#' interpreted cautiously.
#'
#' @param x A scalar or vector of scores that should be converted.
#' @usage
#' madrs(x)
#' madrs.rev(x)
#'
#' @export madrs madrs.rev
#' @aliases madrs.rev
#' @importFrom stats approxfun
#' @import utils
#' @examples
#' \dontrun{
#' x <- runif(100,0,30)
#' madrs(x)
#' }

madrs = function(x) {
  f.madrs.hamd = suppressWarnings(
    approxfun(y = dat.carmody[,"hdrs.17"],
              x = dat.carmody[,"madrs"],
              method = "linear",
              rule = 2))
  f.hamd.bdi = approxfun(x = dat.furukawa[,"hdrs.17"],
                         y = dat.furukawa[,"bdi.1"],
                         method = "linear",
                         rule = 2)
  x = f.madrs.hamd(x); x = f.hamd.bdi(x)
  res = approxfun(y = cm.bdi1$Theta,
                  x = cm.bdi1$Sum.Scores,
                  method = "linear", rule = 2)(x)
  return(res)
}

madrs.rev = function(x) {
  f.hamd.madrs = suppressWarnings(
    approxfun(x = dat.carmody[,"hdrs.17"],
              y = dat.carmody[,"madrs"],
              method = "linear",
              rule = 2))
  f.bdi.hamd = approxfun(y = dat.furukawa[,"hdrs.17"],
                         x = dat.furukawa[,"bdi.1"],
                         method = "linear",
                         rule = 2)
  x = approxfun(x = cm.bdi1$Theta,
                y = cm.bdi1$Sum.Scores,
                method = "linear",
                rule = 2)(x)
  x = f.bdi.hamd(x); res = f.hamd.madrs(x)
  return(res)
}



#' Convert QIDS-SR scores to a depression common metric
#'
#' This function allows you to convert scores of the 16-item Quick Inventory of Depressive Symptomatology (QIDS-SR)
#' to a depression "common metric" score. Common metric scores
#' can be "reconverted" to the original scale by appending `.rev` to the respective
#' converter function.
#'
#' @details Conversions are conducted using the crosswalk tables provided in
#' Wahl et al. ([2014](https://doi.org/10.1016/j.jclinepi.2013.04.019); supplement).
#' Common metric scores _θ_ are normed so that values of 50 indicate the general
#' population mean, with a population standard deviation of 10.
#'
#' The QIDS-SR is not part of the original depression common metric model.
#' To convert scores, values are transformed to HAM-D and then BDI-I equivalents using the
#' equipercentile linking crosswalk developed by Furukawa et al.
#' ([2020](https://doi.org/10.1017/S2045796019000088)), and the IRT-based crosswalk by Rush et al.
#' ([2003](https://doi.org/10.1016/S0006-3223(02)01866-8)). Transformed scores should therefore be
#' interpreted cautiously.
#'
#' @param x A scalar or vector of scores that should be converted.
#' @usage
#' qids(x)
#' qids.rev(x)
#'
#' @export qids qids.rev
#' @aliases qids.rev
#' @importFrom stats approxfun
#' @import utils
#' @examples
#' \dontrun{
#' x <- runif(100,0,30)
#' qids(x)
#' }

qids = function(x) {
  f.qids.hamd = suppressWarnings(
    approxfun(y = dat.rush[,"hdrs.17"],
              x = dat.rush[,"qids"],
              method = "linear",
              rule = 2))
  f.hamd.bdi = approxfun(x = dat.furukawa[,"hdrs.17"],
                         y = dat.furukawa[,"bdi.1"],
                         method = "linear",
                         rule = 2)
  x = f.qids.hamd(x); x = f.hamd.bdi(x)
  res = approxfun(y = cm.bdi1$Theta,
                  x = cm.bdi1$Sum.Scores,
                  method = "linear", rule = 2)(x)
  return(res)
}

qids.rev = function(x) {
  f.hamd.qids = suppressWarnings(
    approxfun(x = dat.rush[,"hdrs.17"],
              y = dat.rush[,"qids"],
              method = "linear",
              rule = 2))
  f.bdi.hamd = approxfun(y = dat.furukawa[,"hdrs.17"],
                         x = dat.furukawa[,"bdi.1"],
                         method = "linear",
                         rule = 2)
  x = approxfun(x = cm.bdi1$Theta,
                y = cm.bdi1$Sum.Scores,
                method = "linear",
                rule = 2)(x)
  x = f.bdi.hamd(x); res = f.hamd.qids(x)
  return(res)
}


#' Convert IDS-SR scores to a depression common metric
#'
#' This function allows you to convert scores of the 30-item Inventory of Depressive Symptomatology (IDS-SR)
#' to a depression "common metric" score. Common metric scores
#' can be "reconverted" to the original scale by appending `.rev` to the respective
#' converter function.
#'
#' @details Conversions are conducted using the crosswalk tables provided in
#' Wahl et al. ([2014](https://doi.org/10.1016/j.jclinepi.2013.04.019); supplement).
#' Common metric scores _θ_ are normed so that values of 50 indicate the general
#' population mean, with a population standard deviation of 10.
#'
#' The IDS-SR is not part of the original depression common metric model.
#' To convert scores, values are transformed to HAM-D and then BDI-I equivalents using the
#' equipercentile linking crosswalk developed by Furukawa et al.
#' ([2020](https://doi.org/10.1017/S2045796019000088)), and the IRT-based crosswalk by Rush et al.
#' ([2003](https://doi.org/10.1016/S0006-3223(02)01866-8)). Transformed scores should therefore be
#' interpreted cautiously.
#'
#' @param x A scalar or vector of scores that should be converted.
#' @usage
#' ids(x)
#' ids.rev(x)
#'
#' @export ids ids.rev
#' @aliases ids.rev
#' @importFrom stats approxfun
#' @import utils
#' @examples
#' \dontrun{
#' x <- runif(100,0,30)
#' ids(x)
#' }

ids = function(x) {
  f.ids.hamd = suppressWarnings(
    approxfun(y = dat.rush[,"hdrs.17"],
              x = dat.rush[,"ids"],
              method = "linear",
              rule = 2))
  f.hamd.bdi = approxfun(x = dat.furukawa[,"hdrs.17"],
                         y = dat.furukawa[,"bdi.1"],
                         method = "linear",
                         rule = 2)
  x = f.ids.hamd(x); x = f.hamd.bdi(x)
  res = approxfun(y = cm.bdi1$Theta,
                  x = cm.bdi1$Sum.Scores,
                  method = "linear", rule = 2)(x)
  return(res)
}

ids.rev = function(x) {
  f.hamd.ids = suppressWarnings(
    approxfun(x = dat.rush[,"hdrs.17"],
              y = dat.rush[,"ids"],
              method = "linear",
              rule = 2))
  f.bdi.hamd = approxfun(y = dat.furukawa[,"hdrs.17"],
                         x = dat.furukawa[,"bdi.1"],
                         method = "linear",
                         rule = 2)
  x = approxfun(x = cm.bdi1$Theta,
                y = cm.bdi1$Sum.Scores,
                method = "linear",
                rule = 2)(x)
  x = f.bdi.hamd(x); res = f.hamd.ids(x)
  return(res)
}



#' Convert GAD-7 scores to an anxiety common metric
#'
#' This function allows you to convert scores of the 7-item Generalized Anxiety Disorder (GAD-7) questionnaire
#' to an anxiety "common metric" score. Common metric scores
#' can be "reconverted" to the original scale by appending `.rev` to the respective
#' converter function.
#'
#' @details Conversions are conducted based on the IRT model developed in
#' Fischer et al. ([2013](https://doi.org/10.1007/s11136-013-0599-y); supplement).
#' Common metric scores _θ_ are normed so that values of 50 indicate the general
#' population mean, with a population standard deviation of 10.
#'
#' @param x A scalar or vector of scores that should be converted.
#' @usage
#' gad7(x)
#' gad7.rev(x)
#'
#' @export gad7 gad7.rev
#' @aliases gad7.rev
#' @importFrom stats approxfun
#' @import utils
#' @examples
#' \dontrun{
#' x <- runif(100,0,30)
#' gad7(x)
#' }

gad7 = function(x) {
  res = approxfun(y = cm.gad$Theta,
                  x = cm.gad$Sum.Scores,
                  method = "linear", rule = 2)(x)
  return(res)
}

gad7.rev = function(x) {
  res = approxfun(x = cm.gad$Theta,
                  y = cm.gad$Sum.Scores,
                  method = "linear", rule = 2)(x)
  return(res)
}


#' Convert HADS-A scores to an anxiety common metric
#'
#' This function allows you to convert scores of the Hospital Anxiety and Depression Scale anxiety subscale
#' to an anxiety "common metric" score. Common metric scores
#' can be "reconverted" to the original scale by appending `.rev` to the respective
#' converter function.
#'
#' @details Conversions are conducted based on the IRT model developed in
#' Fischer et al. ([2013](https://doi.org/10.1007/s11136-013-0599-y); supplement).
#' Common metric scores _θ_ are normed so that values of 50 indicate the general
#' population mean, with a population standard deviation of 10.
#'
#' @param x A scalar or vector of scores that should be converted.
#' @usage
#' hadsa(x)
#' hadsa.rev(x)
#'
#' @export hadsa hadsa.rev
#' @aliases hadsa.rev
#' @importFrom stats approxfun
#' @import utils
#' @examples
#' \dontrun{
#' x <- runif(100,0,30)
#' hadsa(x)
#' }

hadsa = function(x) {
  res = approxfun(y = cm.hadsa$Theta,
                  x = cm.hadsa$Sum.Scores,
                  method = "linear", rule = 2)(x)
  return(res)
}

hadsa.rev = function(x) {
  res = approxfun(x = cm.hadsa$Theta,
                  y = cm.hadsa$Sum.Scores,
                  method = "linear", rule = 2)(x)
  return(res)
}


