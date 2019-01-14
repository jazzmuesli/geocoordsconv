library(measurements)


# Convert N/E to +1 and W/S to -1. Otherwise NA.
convert_dir_to_sign=function(x) {
  if (length(grep("[NE]$", x)) > 0) {
    1
  } else if (length(grep("[SW]$", x)) > 0) {
    -1
  } else {
    NA
  }
}

# TODO: write better documentation and UTF-8 compatible examples
#' @title convert coordinates
#' @description convert latitude or longitude converted into a decimal number
#' @param x string or number
#' @return The decimal representation of latitude or longitude in degree-minute-second or degree-decimal-minute format
#' @export convert_coordinate
convert_coordinate = function(x) {
  # convert direction to -1/1
  sign=convert_dir_to_sign(gsub("([NSWE])$", "\\1", x))
  # components to reuse in regular expressions
  grad_comp="[\u00b0\u00ba ]{1,2}"
  dir_comp="([NSWE])$"
  min_comp="[\u2019\u00b4\u0060\u0027\u2032]"
  if (length(grep(paste0("^([0-9]+)", grad_comp, "([0-9]+)", min_comp, "([0-9]+)", dir_comp), x))> 0) {
    dms=gsub(paste0("^([0-9]+)", grad_comp, "([0-9]+)", min_comp, "([0-9]+)", dir_comp), "\\1 \\2 \\3", x)
    as.numeric(measurements::conv_unit(dms, from = 'deg_min_sec', to = 'dec_deg'))*sign
  } else if (length(grep(paste0("^([0-9]+)", grad_comp, "([0-9.]+)", min_comp, "{0,1}", dir_comp), x))> 0) {
    dgs=gsub(paste0("^([0-9]+)", grad_comp, "([0-9.]+)", min_comp, "{0,1}", dir_comp), "\\1 \\2", x)
    as.numeric(measurements::conv_unit(dgs, from = 'deg_dec_min', to = 'dec_deg'))*sign
  } else if (length(grep(paste0("^([0-9]+)", grad_comp, "([0-9.]+)", min_comp, "([0-9]+)", min_comp, "{1,2}([NSWE])$"), x))> 0) {
    dms=gsub(paste0("^([0-9]+)", grad_comp, "([0-9.]+)", min_comp, "([0-9]+)", min_comp, "{1,2}([NSWE])$"), "\\1 \\2 \\3", x)
    as.numeric(measurements::conv_unit(dms, from = 'deg_min_sec', to = 'dec_deg'))*sign
  } else if (length(grep(paste0("^([0-9]+)", grad_comp, "([0-9,.]+)", min_comp, "{0,1}", dir_comp), x))> 0) {
    dgs=gsub(",", ".", gsub(paste0("^([0-9]+)", grad_comp, "([0-9.,]+)", min_comp, "{0,1}", dir_comp), "\\1 \\2", x))
    as.numeric(measurements::conv_unit(dgs, from = 'deg_dec_min', to = 'dec_deg'))*sign
  } else if (length(grep("^[0-9.-]+$", x))) {
    as.numeric(x)
  } else if (length(grep("/", x)) > 0) {
    mean(sapply(strsplit(x, "/")[[1]], convert_coordinate))
  } else {
    NA
  }
}


