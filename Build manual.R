library(BERG)
pack <- "BERG"
path <- find.package(pack)
system(paste(shQuote(file.path(R.home("bin"),"R")),
             "CMD","Rd2pdf",shQuote(path)))


