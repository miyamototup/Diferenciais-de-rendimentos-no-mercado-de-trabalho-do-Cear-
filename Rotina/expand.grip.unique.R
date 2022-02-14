# ------------------------------
# http://stackoverflow.com/questions/17171148/non-redundant-version-of-expand-grid
# Ferdinand.kraft
expand.grid.unique <- function(x, y, include.equals=FALSE)
{
    x <- unique(x)
    y <- unique(y)
    g <- function(i)
    {
        z <- setdiff(y, x[seq_len(i-include.equals)])
        if(length(z)) cbind(x[i], z, deparse.level=0)
    }
    as.data.frame(do.call(rbind, lapply(seq_along(x), g)))
}
