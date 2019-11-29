hartly.exp.test <- function(x, nrepl=2000)
{
    DNAME <- deparse(substitute(x))
    k <- 0
    n <- length(x)
    t <- max(x) / min(x)
    for (i in 1 : nrepl)
    {
        z <- rexp(n)
        T <- max(z) / min(z)
        if (T < t)k = k + 1
    }
    p.value <- k / nrepl

    RVAL <- list(statistic = c(L = t), p.value = p.value, method = "Hartly test for exponentiality", data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
