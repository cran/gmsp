# Exact JSON access helpers. Avoid `$` on JSON-derived lists because R partial
# matching can resolve missing keys.

.readJson <- function(path, simplifyVector = TRUE) {
  jsonlite::read_json(path = path, simplifyVector = simplifyVector)
}

.jsonHas <- function(x, key) {
  Value <- x
  for (Part in key) {
    if (!is.list(Value) || !(Part %in% names(Value))) return(FALSE)
    Value <- Value[[Part, exact = TRUE]]
  }
  TRUE
}

.jsonGet <- function(x, key, default = NULL) {
  Value <- x
  for (Part in key) {
    if (!is.list(Value) || !(Part %in% names(Value))) return(default)
    Value <- Value[[Part, exact = TRUE]]
  }
  Value
}

.jsonRequire <- function(x, key, path = NULL) {
  if (.jsonHas(x = x, key = key)) return(.jsonGet(x = x, key = key))
  Label <- paste(key, collapse = ".")
  if (!is.null(path)) Label <- sprintf("%s in %s", Label, path)
  stop(sprintf("Missing required JSON key: %s", Label), call. = FALSE)
}
