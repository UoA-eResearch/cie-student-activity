## Shared error reporting helpers for the Shiny apps.
##
## traceback() is useless from inside a tryCatch() error handler: the frames it would
## report have already been unwound by the time the handler runs, which is why failures
## were logged as "No traceback available". These helpers capture the call stack with
## withCallingHandlers(), which runs while the stack is still intact.

# Condition-handling plumbing that only adds noise to a stack trace.
.traceback_noise_frames <- c(
  "tryCatch", "tryCatchList", "tryCatchOne", "doTryCatch", "try",
  "withCallingHandlers", "withRestarts", "withOneRestart", "docall", "force",
  ".handleSimpleError", "h", "stop", "signalCondition", "..stacktraceon..",
  "..stacktraceoff..", "with_traceback", "log_error_with_traceback"
)

.call_name <- function(call) {
  fn <- if (is.call(call)) call[[1L]] else NULL
  if (is.name(fn)) as.character(fn) else ""
}

# Deparse a captured call stack into "  1: fn(args)" lines, innermost call last.
format_call_stack <- function(calls, max_width = 240) {
  calls <- Filter(function(call) !.call_name(call) %in% .traceback_noise_frames, calls)
  if (length(calls) == 0) {
    return("No traceback available.")
  }

  labels <- vapply(calls, function(call) {
    txt <- paste(deparse(call), collapse = " ")
    txt <- gsub("[[:space:]]+", " ", txt)
    if (nchar(txt) > max_width) paste0(substr(txt, 1, max_width - 3), "...") else txt
  }, character(1))

  paste(sprintf("%3d: %s", seq_along(labels), labels), collapse = "\n")
}

# Write an error and its captured stack to stderr, where shiny-server picks it up.
log_error_with_traceback <- function(cnd, calls = NULL, label = NULL) {
  prefix <- if (is.null(label)) "ERROR" else paste0("ERROR [", label, "]")
  message <- conditionMessage(cnd)
  call <- conditionCall(cnd)

  cat(prefix, ": ", message, "\n", sep = "", file = stderr())
  if (!is.null(call)) {
    cat("Error raised in: ", paste(deparse(call), collapse = " "), "\n", sep = "", file = stderr())
  }
  cat("Traceback (innermost call last):\n", file = stderr())
  cat(format_call_stack(calls), "\n", sep = "", file = stderr())
  flush(stderr())

  invisible(NULL)
}

# Evaluate `expr`, logging a full traceback if it fails.
# Without `handler` the error is rethrown once logged; with one, its return value
# becomes the result and the error is swallowed.
with_traceback <- function(expr, handler = NULL, label = NULL) {
  stack <- NULL

  tryCatch(
    withCallingHandlers(expr, error = function(e) stack <<- sys.calls()),
    error = function(e) {
      log_error_with_traceback(e, stack, label)
      if (is.null(handler)) {
        stop(e)
      } else {
        handler(e, format_call_stack(stack))
      }
    }
  )
}

# Make uncaught errors — app startup, reactives, anything not wrapped above —
# report a stack trace too. Call once when the app loads.
install_traceback_handler <- function(label = NULL) {
  # Include shiny's internal frames rather than the elided stack it prints by default.
  options(shiny.fullstacktrace = TRUE)

  options(error = function() {
    calls <- sys.calls()
    prefix <- if (is.null(label)) "UNCAUGHT ERROR" else paste0("UNCAUGHT ERROR [", label, "]")
    cat(prefix, ": ", geterrmessage(), sep = "", file = stderr())
    cat("Traceback (innermost call last):\n", file = stderr())
    cat(format_call_stack(calls), "\n", sep = "", file = stderr())
    flush(stderr())
  })

  invisible(NULL)
}
