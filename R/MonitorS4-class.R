#' MonitorS4 S4 Class for TriKinetics Monitor Data
#'
#' @description 
#' The MonitorS4 class is designed to store and organize Drosophila behavioral data
#' from TriKinetics monitors. It follows a similar structure to Seurat objects.
#'
#' @slot meta.data A data.frame containing metadata for each fly (one row per fly)
#' @slot assays A list containing different data types (mt, ct, pn, raw_data)
#' @slot active.assay Character string indicating which assay is currently active
#' @slot time List containing temporal data and light/dark cycle information
#'
#' @name MonitorS4-class
#' @rdname MonitorS4-class
#' @exportClass MonitorS4
setClass("MonitorS4",
  slots = list(
    meta.data = "data.frame",
    assays = "list", 
    active.assay = "character",
    time = "list"
  ),
  prototype = list(
    meta.data = data.frame(),
    assays = list(),
    active.assay = character(0),
    time = list()
  )
)

#' Validation function for MonitorS4 objects
#'
#' @param object A MonitorS4 object
#' @return TRUE if valid, otherwise error message
#' @name validObject-MonitorS4
setValidity("MonitorS4", function(object) {
  errors <- character(0)
  
  # Check that meta.data is a data.frame
  if (!is.data.frame(object@meta.data)) {
    errors <- c(errors, "meta.data must be a data.frame")
  }
  
  # Check that assays is a list
  if (!is.list(object@assays)) {
    errors <- c(errors, "assays must be a list")
  }
  
  # Check that active.assay is character
  if (length(object@active.assay) > 0 && !is.character(object@active.assay)) {
    errors <- c(errors, "active.assay must be character")
  }
  
  # Check that active.assay exists in assays if specified
  if (length(object@active.assay) > 0 && !object@active.assay %in% names(object@assays)) {
    errors <- c(errors, "active.assay must exist in assays names")
  }
  
  # Check that time is a list
  if (!is.list(object@time)) {
    errors <- c(errors, "time must be a list")
  }
  
  if (length(errors) == 0) TRUE else errors
})

#' Constructor for MonitorS4 objects
#'
#' @param meta.data A data.frame with metadata (optional)
#' @param assays A list of assay data (optional)
#' @param active.assay Character string for active assay (optional)
#' @param time A list with time data (optional)
#' @return A MonitorS4 object
#' @export
MonitorS4 <- function(meta.data = data.frame(), 
                      assays = list(), 
                      active.assay = character(0),
                      time = list()) {
  new("MonitorS4", 
      meta.data = meta.data,
      assays = assays,
      active.assay = active.assay,
      time = time)
}

#' Show method for MonitorS4 objects
#'
#' @param object A MonitorS4 object
#' @return Prints object summary
setMethod("show", "MonitorS4", function(object) {
  cat("An object of class MonitorS4\n")
  cat("Number of flies:", nrow(object@meta.data), "\n")
  cat("Available assays:", paste(names(object@assays), collapse = ", "), "\n")
  if (length(object@active.assay) > 0) {
    cat("Active assay:", object@active.assay, "\n")
  }
  if (length(object@time) > 0) {
    cat("Time information available:", paste(names(object@time), collapse = ", "), "\n")
  }
})

#' Get metadata from MonitorS4 object
#'
#' @param object A MonitorS4 object
#' @return The metadata data.frame
#' @export
getMeta <- function(object) {
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  return(object@meta.data)
}

#' Set metadata in MonitorS4 object
#'
#' @param object A MonitorS4 object
#' @param meta.data A data.frame with metadata
#' @return Updated MonitorS4 object
#' @export
setMeta <- function(object, meta.data) {
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  if (!is.data.frame(meta.data)) {
    stop("meta.data must be a data.frame")
  }
  object@meta.data <- meta.data
  validObject(object)
  return(object)
}

#' Get assay data from MonitorS4 object
#'
#' @param object A MonitorS4 object
#' @param assay Character string specifying which assay to retrieve
#' @return The specified assay data
#' @export
getAssay <- function(object, assay = NULL) {
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  
  if (is.null(assay)) {
    if (length(object@active.assay) == 0) {
      stop("No active assay set and no assay specified")
    }
    assay <- object@active.assay
  }
  
  if (!assay %in% names(object@assays)) {
    stop("Assay '", assay, "' not found in object")
  }
  
  return(object@assays[[assay]])
}

#' Set assay data in MonitorS4 object
#'
#' @param object A MonitorS4 object
#' @param assay Character string specifying assay name
#' @param data Data to store in the assay
#' @return Updated MonitorS4 object
#' @export
setAssay <- function(object, assay, data) {
  if (!is(object, "MonitorS4")) {
    stop("Object must be of class MonitorS4")
  }
  object@assays[[assay]] <- data
  if (length(object@active.assay) == 0) {
    object@active.assay <- assay
  }
  validObject(object)
  return(object)
}