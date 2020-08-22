install.packages("survey")
library(survey)
#' Prediction
#'
#' Bankruptcy prediction function
#'
#' @export
#' @param input. Required.
prediction <- function(inputformula, inputvalues) {
	useformula <- as.formula(inputformula)
	usevalues <- as.data.frame(inputvalues)
	design34 <- svydesign(id = ~casenumber, weights = ~pweight, data = data)
	model34 <- svyglm(lnfeeexpord ~ lnroles + lnassets + lndaysin + yearconfirmed + lnemployees + shop + saleall, design = design34)
	newdata1 <- with(data, usevalues)
	pred <- predict(model34, newdata = newdata1, se.fit = T)
	efit <- pred[1]
	lfit <- efit - 1.44 * sqrt(attr(pred, "var")) / .97
	hfit <- efit + 1.44 * sqrt(attr(pred, "var")) / .97
	list(efit = efit, hfit = hfit, lfit = lfit)
}
