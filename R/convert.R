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


#' @export convert_coordinate
#'
convert_coordinate = function(x) {
  # convert direction to -1/1
  sign=convert_dir_to_sign(gsub("([NSWE])$", "\\1", x))
  # components to reuse in regular expressions
  grad_comp="[\u00b0\u00ba ]{1,2}"
  dir_comp="([NSWE])$"
  min_comp="[’´`'′]"
  if (length(grep(paste0("^([0-9]+)", grad_comp, "([0-9]+)[´`']([0-9]+)", dir_comp), x))> 0) {
    # 43°37'83N -> 43.63972
    dms=gsub(paste0("^([0-9]+)", grad_comp, "([0-9]+)[´`']([0-9]+)", dir_comp), "\\1 \\2 \\3", x)
    as.numeric(measurements::conv_unit(dms, from = 'deg_min_sec', to = 'dec_deg'))*sign
  } else if (length(grep(paste0("^([0-9]+)", grad_comp, "([0-9.]+)", min_comp, "{0,1}", dir_comp), x))> 0) {
    # 41°59.00'N" -> 41.9833333333333
    # 42° 29.60´N" -> 42.49333
    # 43°37.67N -> 43.62783
    # 38°33.5’N -> 38.55833
    # 35°09′N -> 35.15
    dgs=gsub(paste0("^([0-9]+)", grad_comp, "([0-9.]+)", min_comp, "{0,1}", dir_comp), "\\1 \\2", x)
    as.numeric(measurements::conv_unit(dgs, from = 'deg_dec_min', to = 'dec_deg'))*sign
  } else if (length(grep("^([0-9]+)[º°]([0-9.]+)[´`']([0-9]+)[´`']{1,2}([NSWE])$", x))> 0) {
    # 43°42'93''N -> 43.72583
    # 28°09'19'N -> 28.15528
    dms=gsub("^([0-9]+)[º°]([0-9.]+)[´`']([0-9]+)[´`']{1,2}([NSWE])$", "\\1 \\2 \\3", x)
    as.numeric(measurements::conv_unit(dms, from = 'deg_min_sec', to = 'dec_deg'))*sign
  } else if (length(grep(paste0("^([0-9]+)", grad_comp, "([0-9,.]+)[´`']{0,1}", dir_comp), x))> 0) {
    # 42°21,12´N -> 42.352
    dgs=gsub(",", ".", gsub(paste0("^([0-9]+)", grad_comp, "([0-9.,]+)[´`']{0,1}", dir_comp), "\\1 \\2", x))
    as.numeric(measurements::conv_unit(dgs, from = 'deg_dec_min', to = 'dec_deg'))*sign
  } else if (length(grep("^[0-9.-]+$", x))) {
    # -27.95 -> as is
    as.numeric(x)
  } else if (length(grep("/", x)) > 0) {
    # two coordinates -> take average
    # 12°02.75'S/12°02.43'S -> -12.04317
    mean(sapply(strsplit(x, "/")[[1]], convert_coordinate))
  } else {
    NA
  }
}


