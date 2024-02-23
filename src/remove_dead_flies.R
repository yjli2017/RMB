remove_dead_flies <- function(monitor) {
  group <- rep(1:(nrow(monitor@assays$mt) %/% 1440 + 1), each = 1440, length.out = nrow(monitor@assays$mt))
  everyday_activity <- as.data.frame(lapply(monitor@assays$mt, function(x) aggregate(x, list(group), sum)$x))
  deadlist <- which(apply(everyday_activity[3,], 2, sum) <= 30)

  monitor@assays$pn <- monitor@assays$pn[, -deadlist]
  monitor@assays$mt <- monitor@assays$mt[, -deadlist]
  monitor@assays$ct <- monitor@assays$ct[, -deadlist]
  monitor@meta.data <- monitor@meta.data[-deadlist,]
  
  return(monitor)
}