.onAttach <- function(libname, pkgname) {
  packageStartupMessage(crayon::green("\u2713"),
                        crayon::cyan(" Loading "),
                        crayon::cyan$bold("{ipdconverters}"),
                        crayon::cyan(" 0.0.9000 [BETA]."))
}
