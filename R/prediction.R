#' Prediction
#'
#' Bankruptcy prediction function
#'
#' @export
#' @param input. Required.
prediction <- function(inputformula, inputvalues) {
	library(survey)
	useformula <- as.formula(inputformula)
	usevalues <- as.data.frame(inputvalues)
	design31 <- svydesign(id = ~casenumber, weights = ~pweight, data = predictiondata)
	model31 <- svyglm(lnfeeexpord ~ lnassets + prepack + preneg + lnsales + xyearfiled + shop, design = design31)
	newdata1 <- with(predictiondata, usevalues)
	pred <- predict(model31, newdata = newdata1, se.fit = T)
	efit <- pred[1]
	lfit <- efit - 1.44 * sqrt(attr(pred, "var")) / .97
	hfit <- efit + 1.44 * sqrt(attr(pred, "var")) / .97
	list(efit = efit, hfit = hfit, lfit = lfit)
}
