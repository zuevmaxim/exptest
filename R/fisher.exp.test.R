fisher.exp.test <- function(x, nrepl=2000)
{
    DNAME <- deparse(substitute(x))
    n <- length(x)
    l <- 0

    calcF <- function(y)
    {
        n <- length(y)
        return(sum(y) / ((n - 1) * y[1]))
    }

    f <- calcF(x)
    for (i in 1 : nrepl)
    {
        e <- rexp(n)
        F <- calcF(e)
        if (F > f)
        {
            l = l + 1
        }
    }
    p.value = l / nrepl
    RVAL <- list(statistic = f, p.value = p.value, method = "Fisher test for exponentiality", data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
