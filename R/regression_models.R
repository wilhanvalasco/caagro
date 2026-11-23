# Registry of regression models --------------------------------------------
# Each entry: label, formula (display), type ("lm", "nls", "glm"), params,
# f(theta, x) mean function, fit(d) returning a fitted object, eq(theta)
# formatted equation, x_domain check.

sup_x <- function(k) {
  utf8 <- isTRUE(l10n_info()[["UTF-8"]])
  if (!utf8) return(paste0("x^", k))
  paste0("x", intToUtf8(c(178L, 179L)[k - 1L]))
}

r2_lab <- function(prefix = "") paste0(prefix, if (isTRUE(l10n_info()[["UTF-8"]])) paste0("R", intToUtf8(178L)) else "R2")

fmt_coef <- function(v, digits = 4) formatC(v, format = "g", digits = digits)
sgn <- function(v, digits = 4) paste0(if (v < 0) " - " else " + ", fmt_coef(abs(v), digits))

nls_ctrl <- function() stats::nls.control(maxiter = 500, minFactor = 1 / 4096, warnOnly = FALSE)

# Try several strategies for nls; returns fit or a condition
nls_robust <- function(formula, data, start, lower = NULL, upper = NULL) {
  attempts <- list()
  try1 <- function(alg, st) {
    tryCatch({
      if (alg == "port") {
        args <- list(formula = formula, data = data, start = st, algorithm = "port", control = nls_ctrl())
        if (!is.null(lower)) args$lower <- lower
        if (!is.null(upper)) args$upper <- upper
        do.call(stats::nls, args)
      } else stats::nls(formula, data = data, start = st, control = nls_ctrl())
    }, error = function(e) e)
  }
  starts <- list(start)
  for (m in c(0.8, 1.25, 0.5, 2)) starts[[length(starts) + 1]] <- lapply(start, function(v) v * m)
  for (st in starts) {
    for (alg in c("default", "port")) {
      res <- try1(alg, st)
      if (!inherits(res, "error")) return(res)
      attempts[[length(attempts) + 1]] <- conditionMessage(res)
    }
  }
  structure(list(message = paste("O ajuste n\u00e3o convergiu:", attempts[[1]])), class = c("caagro_nonconv", "error", "condition"))
}

model_registry <- function() {
  list(
    linear = list(
      label = "Linear", type = "lm", formula = "y = a + b x", params = c("a", "b"),
      f = function(th, x) th[1] + th[2] * x,
      fit = function(d) stats::lm(y ~ x, data = d),
      eq = function(th) paste0("y = ", fmt_coef(th[1]), sgn(th[2]), "x")
    ),
    quadratic = list(
      label = "Quadr\u00e1tico", type = "lm", formula = "y = a + b x + c x^2", params = c("a", "b", "c"),
      f = function(th, x) th[1] + th[2] * x + th[3] * x^2,
      fit = function(d) stats::lm(y ~ x + I(x^2), data = d),
      eq = function(th) paste0("y = ", fmt_coef(th[1]), sgn(th[2]), "x", sgn(th[3]), sup_x(2))
    ),
    cubic = list(
      label = "C\u00fabico", type = "lm", formula = "y = a + b x + c x^2 + d x^3", params = c("a", "b", "c", "d"),
      f = function(th, x) th[1] + th[2] * x + th[3] * x^2 + th[4] * x^3,
      fit = function(d) stats::lm(y ~ x + I(x^2) + I(x^3), data = d),
      eq = function(th) paste0("y = ", fmt_coef(th[1]), sgn(th[2]), "x", sgn(th[3]), sup_x(2), sgn(th[4]), sup_x(3))
    ),
    logarithmic = list(
      label = "Logar\u00edtmico", type = "lm", formula = "y = a + b ln(x)", params = c("a", "b"), x_positive = TRUE,
      f = function(th, x) th[1] + th[2] * log(x),
      fit = function(d) stats::lm(y ~ log(x), data = d),
      eq = function(th) paste0("y = ", fmt_coef(th[1]), sgn(th[2]), "ln(x)")
    ),
    exponential = list(
      label = "Exponencial", type = "nls", formula = "y = a exp(b x)", params = c("a", "b"),
      f = function(th, x) th[1] * exp(th[2] * x),
      fit = function(d) {
        st <- if (all(d$y > 0)) { l <- stats::coef(stats::lm(log(y) ~ x, d)); list(a = exp(l[[1]]), b = l[[2]]) } else
          list(a = mean(d$y), b = 0.01 / max(abs(d$x), 1))
        nls_robust(y ~ a * exp(b * x), d, st)
      },
      eq = function(th) paste0("y = ", fmt_coef(th[1]), " exp(", fmt_coef(th[2]), "x)")
    ),
    power = list(
      label = "Pot\u00eancia", type = "nls", formula = "y = a x^b", params = c("a", "b"), x_positive = TRUE,
      f = function(th, x) th[1] * x^th[2],
      fit = function(d) {
        st <- if (all(d$y > 0)) { l <- stats::coef(stats::lm(log(y) ~ log(x), d)); list(a = exp(l[[1]]), b = l[[2]]) } else list(a = mean(d$y), b = 0.5)
        nls_robust(y ~ a * x^b, d, st)
      },
      eq = function(th) paste0("y = ", fmt_coef(th[1]), " x^", fmt_coef(th[2]))
    ),
    logistic3 = list(
      label = "Log\u00edstico (3 par\u00e2metros)", type = "nls", formula = "y = A / (1 + exp((x0 - x)/s))", params = c("A", "x0", "s"),
      f = function(th, x) th[1] / (1 + exp((th[2] - x) / th[3])),
      fit = function(d) {
        fit <- tryCatch(stats::nls(y ~ stats::SSlogis(x, A, x0, s), data = d, control = nls_ctrl()), error = function(e) e)
        if (!inherits(fit, "error")) return(fit)
        st <- list(A = max(d$y) * 1.05, x0 = stats::median(d$x), s = diff(range(d$x)) / 6)
        nls_robust(y ~ A / (1 + exp((x0 - x) / s)), d, st)
      },
      eq = function(th) paste0("y = ", fmt_coef(th[1]), " / (1 + exp((", fmt_coef(th[2]), " - x)/", fmt_coef(th[3]), "))")
    ),
    logistic4 = list(
      label = "Log\u00edstico (4 par\u00e2metros)", type = "nls", formula = "y = B + (A - B)/(1 + exp((x0 - x)/s))", params = c("B", "A", "x0", "s"),
      f = function(th, x) th[1] + (th[2] - th[1]) / (1 + exp((th[3] - x) / th[4])),
      fit = function(d) {
        fit <- tryCatch(stats::nls(y ~ stats::SSfpl(x, B, A, x0, s), data = d, control = nls_ctrl()), error = function(e) e)
        if (!inherits(fit, "error")) return(fit)
        st <- list(B = min(d$y), A = max(d$y), x0 = stats::median(d$x), s = diff(range(d$x)) / 6)
        nls_robust(y ~ B + (A - B) / (1 + exp((x0 - x) / s)), d, st)
      },
      eq = function(th) paste0("y = ", fmt_coef(th[1]), " + (", fmt_coef(th[2]), sgn(-th[1]), ")/(1 + exp((", fmt_coef(th[3]), " - x)/", fmt_coef(th[4]), "))")
    ),
    gompertz = list(
      label = "Gompertz", type = "nls", formula = "y = A exp(-b exp(-c x))", params = c("A", "b", "c"),
      f = function(th, x) th[1] * exp(-th[2] * exp(-th[3] * x)),
      fit = function(d) {
        st <- tryCatch({ g <- stats::nls(y ~ stats::SSgompertz(x, Asym, b2, b3), data = d, control = nls_ctrl())
                         cf <- stats::coef(g); list(A = cf[["Asym"]], b = cf[["b2"]], c = -log(cf[["b3"]])) },
                       error = function(e) list(A = max(d$y) * 1.05, b = 2, c = 3 / max(abs(d$x), 1)))
        nls_robust(y ~ A * exp(-b * exp(-c * x)), d, st)
      },
      eq = function(th) paste0("y = ", fmt_coef(th[1]), " exp(-", fmt_coef(th[2]), " exp(-", fmt_coef(th[3]), "x))")
    ),
    michaelis_menten = list(
      label = "Michaelis-Menten", type = "nls", formula = "y = Vm x / (K + x)", params = c("Vm", "K"),
      f = function(th, x) th[1] * x / (th[2] + x),
      fit = function(d) {
        fit <- tryCatch(stats::nls(y ~ stats::SSmicmen(x, Vm, K), data = d, control = nls_ctrl()), error = function(e) e)
        if (!inherits(fit, "error")) return(fit)
        nls_robust(y ~ Vm * x / (K + x), d, list(Vm = max(d$y), K = stats::median(d$x)))
      },
      eq = function(th) paste0("y = ", fmt_coef(th[1]), "x / (", fmt_coef(th[2]), " + x)")
    ),
    asymptotic = list(
      label = "Assint\u00f3tico", type = "nls", formula = "y = A + (R0 - A) exp(-c x)", params = c("A", "R0", "c"),
      f = function(th, x) th[1] + (th[2] - th[1]) * exp(-th[3] * x),
      fit = function(d) {
        st <- tryCatch({ g <- stats::nls(y ~ stats::SSasymp(x, Asym, R0, lrc), data = d, control = nls_ctrl())
                         cf <- stats::coef(g); list(A = cf[["Asym"]], R0 = cf[["R0"]], c = exp(cf[["lrc"]])) },
                       error = function(e) list(A = max(d$y), R0 = min(d$y), c = 3 / max(abs(d$x), 1)))
        nls_robust(y ~ A + (R0 - A) * exp(-c * x), d, st)
      },
      eq = function(th) paste0("y = ", fmt_coef(th[1]), " + (", fmt_coef(th[2]), sgn(-th[1]), ") exp(-", fmt_coef(th[3]), "x)")
    ),
    mitscherlich = list(
      label = "Mitscherlich", type = "nls", formula = "y = A (1 - exp(-c (x + b)))", params = c("A", "c", "b"),
      f = function(th, x) th[1] * (1 - exp(-th[2] * (x + th[3]))),
      fit = function(d) {
        st <- tryCatch({ g <- stats::nls(y ~ stats::SSasymp(x, Asym, R0, lrc), data = d, control = nls_ctrl())
                         cf <- stats::coef(g); cc <- exp(cf[["lrc"]])
                         list(A = cf[["Asym"]], c = cc, b = -log(max(1e-6, 1 - cf[["R0"]] / cf[["Asym"]])) / cc) },
                       error = function(e) list(A = max(d$y) * 1.1, c = 3 / max(abs(d$x), 1), b = 1))
        nls_robust(y ~ A * (1 - exp(-c * (x + b))), d, st)
      },
      eq = function(th) paste0("y = ", fmt_coef(th[1]), " (1 - exp(-", fmt_coef(th[2]), " (x", sgn(th[3]), ")))")
    ),
    loglogistic = list(
      label = "Log-log\u00edstico (dose-resposta, 4 par.)", type = "nls", formula = "y = c + (d - c)/(1 + exp(b (ln x - ln e)))",
      params = c("b", "c", "d", "e"), x_nonneg = TRUE,
      f = function(th, x) th[2] + (th[3] - th[2]) / (1 + exp(th[1] * (log(x) - log(th[4])))),
      fit = function(d) {
        xpos <- d$x[d$x > 0]
        b0 <- if (stats::cor(d$x, d$y) < 0) 1 else -1
        st <- list(b = b0, c = min(d$y), d = max(d$y), e = stats::median(xpos))
        nls_robust(y ~ c + (d - c) / (1 + exp(b * (log(x) - log(e)))), d, st,
                   lower = c(b = -Inf, c = -Inf, d = -Inf, e = 1e-8), upper = c(b = Inf, c = Inf, d = Inf, e = Inf))
      },
      eq = function(th) paste0("y = ", fmt_coef(th[2]), " + (", fmt_coef(th[3]), sgn(-th[2]), ")/(1 + exp(", fmt_coef(th[1]), " (ln x - ln ", fmt_coef(th[4]), ")))")
    ),
    linear_plateau = list(
      label = "Linear-plat\u00f4 (segmentado)", type = "nls", formula = "y = a + b min(x, x0)", params = c("a", "b", "x0"),
      f = function(th, x) th[1] + th[2] * pmin(x, th[3]),
      fit = function(d) {
        ux <- sort(unique(d$x))
        if (length(ux) < 4) return(structure(list(message = "Linear-plat\u00f4 exige ao menos 4 n\u00edveis de x."), class = c("caagro_nonconv", "error", "condition")))
        prof <- function(x0) sum(stats::residuals(stats::lm(y ~ pmin(x, x0), d))^2)
        o <- stats::optimize(prof, c(ux[2], ux[length(ux) - 1]))
        l <- stats::coef(stats::lm(y ~ pmin(x, o$minimum), d))
        nls_robust(y ~ a + b * pmin(x, x0), d, list(a = l[[1]], b = l[[2]], x0 = o$minimum))
      },
      eq = function(th) paste0("y = ", fmt_coef(th[1]), sgn(th[2]), " min(x, ", fmt_coef(th[3]), ")")
    ),
    binomial = list(
      label = "Binomial (logit)", type = "glm", formula = "logit(p) = a + b x", params = c("a", "b"),
      f = function(th, x) stats::plogis(th[1] + th[2] * x),
      fit = function(d) stats::glm(cbind(s, n - s) ~ x, family = stats::binomial("logit"), data = d),
      eq = function(th) paste0("logit(p) = ", fmt_coef(th[1]), sgn(th[2]), "x")
    ),
    binomial_logdose = list(
      label = "Binomial (logit, log da dose)", type = "glm", formula = "logit(p) = a + b ln(x)", params = c("a", "b"), x_positive = TRUE,
      f = function(th, x) stats::plogis(th[1] + th[2] * log(x)),
      fit = function(d) stats::glm(cbind(s, n - s) ~ log(x), family = stats::binomial("logit"), data = d),
      eq = function(th) paste0("logit(p) = ", fmt_coef(th[1]), sgn(th[2]), "ln(x)")
    )
  )
}

# Regression models available in caagro (internal; listed in ?fit_regression)
regression_models <- function() {
  reg <- model_registry()
  data.frame(model = names(reg), label = vapply(reg, `[[`, "", "label"),
             equation = vapply(reg, `[[`, "", "formula"), type = vapply(reg, `[[`, "", "type"),
             n_params = vapply(reg, function(m) length(m$params), 1L), row.names = NULL)
}
