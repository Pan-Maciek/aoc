args = commandArgs(trailingOnly=TRUE)

if (length(args) != 1) {
  write("Usage: Rscript one.r <input>", stderr())
  quit(status=1)
}

if (!file.exists(args[1])) {
  write(paste("Could not open file:", args[1], "file does not exist"), stderr())
  quit(status=1)
}

h <- strtoi(strsplit(readLines("input.in"), ",")[[1]])

print(sum(abs(h - median(h))))