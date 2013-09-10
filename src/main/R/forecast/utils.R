
####################################################################
# Finds the median of an ordinal aka ordered factor.
####################################################################
median.ordered <- function(ord) {
    levs <- levels(ord)
    m <- median(as.integer(ord))
    if(floor(m) != m) {
        warning("median is between two values; using the first one")
        m <- floor(m)
    }
    ordered(m, labels = levs, levels = seq_along(levs))
}