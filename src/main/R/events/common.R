# converts a list to a data table
toDataTable <- function(aList) {
    matrix <- do.call(rbind.fill.matrix, 
                      lapply(aList, function(l) t(unlist(l))))
    data.table(matrix)        
}