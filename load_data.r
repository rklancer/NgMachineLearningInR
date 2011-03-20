load_data <- function() {
  X <<- as.matrix(read.table("data/x.dat", quote="\""))
  y <<- as.matrix(read.table("data/y.dat", quote="\""))
}
