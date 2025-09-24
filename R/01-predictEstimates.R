# Predict estimates for Bayesian spatio-temporal model
#
# @param model Model object
# @param XPred Prediction grid (data.frame)
# @return Data frame with predictions and uncertainties
predict_bayes <- function(model, XPred) {
    sc <- model$sc
    betas <- model$model$beta
    

    PredMatr <- Predict.matrix(sc, data = XPred)

    sapply(1:nrow(betas), function(x) {
        PredMatr %*% betas[x, ] * model$sRe + model$mRe
    })
}

# Summarize Bayesian predictions with uncertainties
#
# @param Predictions Matrix of predictions from the model
# @param XPred Data frame of predictor variables
# @param model Model object
# @param estType Estimate type (e.g. "Mean", "1 SE", "Quantile", etc.)
# @param estQuantile Quantile value (if applicable)
# @param sdValue Standard deviation value for uncertainty intervals
#  (if NULL, credible intervals are computed)
# @param minVal Minimum value for predictions (for truncation)
# @param maxVal Maximum value for predictions (for truncation)
# @return Data frame with predictions and uncertainties
summarize_bayes_predictions <- function(
    Predictions,
    model,
    XPred,
    estType = "Mean",
    estQuantile = 0.9,
    sdValue = NULL,
    minVal = NULL, maxVal = NULL) {
    betaSigma <- model$model$betaSigma

    if (!is.null(model$IndependentType) && model$IndependentType != "numeric") {
        Predictions <- invLogit(Predictions)
    }
    if (!is.null(betaSigma)) {
        PredMatrV <- Predict.matrix(model$scV, data = XPred)
        PredictionsSigma <- rowMeans(sqrt(sapply(1:nrow(betaSigma), function(x) {
            exp((PredMatrV %*% betaSigma[x, ])) / model$model$sigma[x]
        }) * model$sRe^2))
    } else {
        if (!is.null(model$IndependentType) && model$IndependentType != "numeric") {
            PredictionsSigma <- sqrt(Predictions * (1 - Predictions))
        } else {
            PredictionsSigma <- sqrt(mean(model$model$sigma) * model$sRe^2)
        }
    }

    if (!is.null(sdValue)) {
        Est <- rowMeans(Predictions)
    } else {
        Est <- switch(estType,
            "Mean" = rowMeans(Predictions),
            "1 SE" = apply(Predictions, 1, sd),
            "2 SE" = apply(Predictions, 1, sd) * 2,
            "1 SETOTAL" = sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2),
            "2 SETOTAL" = sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2) * 2,
            "1 SD Population" = PredictionsSigma * 1,
            "2 SD Population" = PredictionsSigma * 2,
            "Quantile" = apply(Predictions, 1, quantile, estQuantile, names = FALSE),
            "QuantileTOTAL" = rowMeans(Predictions + qnorm(estQuantile) * sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2)),
            rowMeans(Predictions)
        )
    }

    if (!is.null(sdValue)) {
        IntLower <- pmax(minVal, pmin(maxVal, rowMeans(Predictions) - sdValue * apply(Predictions, 1, sd)))
        IntUpper <- pmax(minVal, pmin(maxVal, rowMeans(Predictions) + sdValue * apply(Predictions, 1, sd)))
    } else {
        #precomputing quantiles for faster execution
        qs <- apply(Predictions, 1, quantile, c(0.025, 0.975), names = FALSE)
        qs2 <- apply(Predictions + sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2), 1,
            quantile, c(0.025, 0.975),
            names = FALSE
        )
        IntLower <- qs[1, ]
        IntUpper <- qs[2, ]
    }

    # add estimates and uncertainties
    if (!is.null(sdValue)) {
        XPred <- data.frame(XPred,
            Est = Est,
            Sd = apply(Predictions, 1, sd),
            SdTotal = sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2),
            PredictionsSigma = PredictionsSigma,
            IntLower = IntLower,
            IntUpper = IntUpper
        )
    } else {
        XPred <- data.frame(XPred,
            Est = Est,
            Sd = apply(Predictions, 1, sd),
            SDPop = PredictionsSigma,
            SdTotal = sqrt(PredictionsSigma^2 + apply(Predictions, 1, sd)^2),
            IntLower = IntLower,
            IntUpper = IntUpper,
            IntLowerTotal = qs2[1, ],
            IntUpperTotal = qs2[2, ],
            resError = sqrt(mean(model$model$sigma + model$model$tau) * model$sRe^2)
        )
    }

    return(XPred)
}

# Predict estimates for frequentist spatio-temporal model (GAMM)
#
# @param model Model object
# @param XPred Prediction grid (data.frame)
# @return Data frame with predictions and uncertainties
predict_gamm <- function(model, XPred) {
    predict(model$model$gam,
        newdata = XPred,
        se.fit = TRUE, type = "response", newdata.guaranteed = TRUE
    )
}

# Summarize GAMM predictions with uncertainties
# 
# @param Est Predictions from the GAMM model (list with fit and se.fit)
# @param XPred Prediction grid (data.frame)
# @param model Model object
# @param estType Estimate type
# @param estQuantile Quantile value
# @param sdValue Standard deviation value for uncertainty intervals
#    (if NULL, credible intervals are computed)
# @param minVal Minimum value for predictions (for truncation)
# @param maxVal Maximum value for predictions (for truncation)
# @return Data frame with predictions and uncertainties
summarize_gamm_predictions <- function(
    Est,
    model,
    XPred,
    estType = "Mean",
    estQuantile = 0.9,
    sdValue = NULL,
    minVal = NULL,
    maxVal = NULL
) {
    if (!is.null(sdValue)) {
        Est$fit <- Est$fit
    } else {
        if (!is.null(model$IndependentType) && model$IndependentType != "numeric") {
            varM <- Est$fit * (1 - Est$fit)
        } else {
            varM <- var(residuals(model$model$gam))
        }

        Est$fit <- switch(estType,
            "1 SE"           = Est$se.fit,
            "2 SE"           = Est$se.fit * 2,
            "1 SETOTAL"      = sqrt(Est$se.fit^2 + varM),
            "2 SETOTAL"      = sqrt(Est$se.fit^2 + varM) * 2,
            "1 SD Population"= sqrt(varM) * 1,
            "2 SD Population"= sqrt(varM) * 2,
            "Quantile"       = Est$fit + qnorm(estQuantile) * Est$se.fit,
            "QuantileTOTAL"  = Est$fit + qnorm(estQuantile) * sqrt(Est$se.fit^2 + varM),
            Est$fit # default
        )
    }

    if (!is.null(sdValue)) {
        data.frame(XPred,
            Est = Est$fit,
            Sd = Est$se.fit,
            SdTotal = sqrt(Est$se.fit^2 +  mean(residuals(model$model$gam)^2)),
            PredictionsSigma = sd(residuals(model$model$gam)),
            IntLower = pmax(minVal, pmin(maxVal, Est$fit - sdValue * Est$se.fit)),
            IntUpper = pmax(minVal, pmin(maxVal, Est$fit + sdValue * Est$se.fit))
            # IntLowerTotal = Est$fit - 1.96 * sqrt(Est$se.fit^2 + mean(residuals(model$model$gam)^2)),
            # IntUpperTotal = Est$fit + 1.96 * sqrt(Est$se.fit^2 + mean(residuals(model$model$gam)^2))
        )
    } else {
        fitted_values <- model$model$gam$fitted.values
        if (model$model$gam$family$family == "binomial") {
            fitted_values <- 1 / (1 + exp(-fitted_values))
        }
        data.frame(XPred,
            Est = Est$fit,
            Sd = Est$se.fit,
            SDPop = sqrt(varM),
            SdTotal = sqrt(Est$se.fit^2 + varM),
            IntLower = Est$fit - 1.96 * Est$se.fit,
            IntUpper = Est$fit + 1.96 * Est$se.fit,
            IntLowerTotal = Est$fit - 1.96 * sqrt(Est$se.fit^2 + varM),
            IntUpperTotal = Est$fit + 1.96 * sqrt(Est$se.fit^2 + varM),
            resError = sqrt(mean((fitted_values - model$model$gam$y)^2))
        )
    }
}

# Predict estimates for KDE spatio-temporal model (GAM)
#
# @param model Model object
# @param XPred Prediction grid (data.frame)
# @return Data frame with predictions and uncertainties
predict_gam <- function(model, XPred) {
    sapply(1:length(model$model), function(x) {
        predict(model$model[[x]], x = XPred)
    })
}

# Summarize GAM predictions with uncertainties
# 
# @param Predictions Matrix of predictions from the model
# @param model Model object
# @param XPred Prediction grid (data.frame)
# @param estType Estimate type
# @param estQuantile Quantile value
# @param sdValue Standard deviation value for uncertainty intervals
#  (if NULL, credible intervals are computed)
# @return Data frame with predictions and uncertainties
summarize_gam_predictions <- function(
    Predictions,
    model,
    XPred,
    estType = "Mean",
    estQuantile = 0.9,
    sdValue = NULL
) {
    if (!is.null(sdValue)) {
        Est <- rowMeans(Predictions)
    } else {
        Est <- switch(estType,
            "Mean" = rowMeans(Predictions),
            "1 SE" = apply(Predictions, 1, sd),
            "2 SE" = apply(Predictions, 1, sd) * 2,
            "Quantile" = apply(Predictions, 1, quantile, estQuantile, names = FALSE),
            rowMeans(Predictions)
        )
    }

    if (!is.null(sdValue)) {
        probs <- c(1 - pnorm(sdValue), pnorm(sdValue))
    } else {
        probs <- c(0.025, 0.975)
    }
    qs <- apply(Predictions, 1, quantile, probs, names = FALSE)

    if (is.null(time)) {
        IntLower <- pmax(0, qs[1,])
        IntUpper <- qs[2, ]
    } else if (!is.null(sdValue)) {
        IntLower <- pmax(0, qs[1,])
        IntUpper <- pmax(0, qs[2,])
    } else {
        IntLower <- qs[1, ]
        IntUpper <- qs[2, ]
    }
    data.frame(XPred,
        Est = Est,
        Sd = apply(Predictions, 1, sd),
        IntLower = IntLower,
        IntUpper = IntUpper
    )
}
