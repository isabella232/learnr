is_testing_enabled <- function() {
  x <- Sys.getenv("LEARNR_TEST", "false")
  if (tolower(x) %in% c("yes", "true", "1", "on")) return(TRUE)
  if (tolower(x) %in% c("no", "false", "0", "off")) return(FALSE)
  rlang::abort(
    "Invalid value for `LEARNR_TEST` environment variable, must be `true` or `false`."
  )
}

#' @export
expect_feedback <- function(
  expr,
  correct = NULL,
  message = NULL,
  type = NULL,
  location = NULL,
  ...,
  perl = FALSE,
  fixed = FALSE,
  ignore.case = FALSE,
  .exercise = NULL,
  .envir = NULL,
  .label = NULL,
  .eval_global_setup = FALSE
) {
  rlang::check_installed("testthat", reason = "Testing exercise feedback requires {testthat}")

  expr <- rlang::enexpr(expr)
  expr <- if (rlang::is_null(expr)) {
    ""
  } else if (rlang::is_string(expr)) {
    expr
  } else {
    rlang::expr_text(expr)
  }

  # evaluate the exercise (easy, right?)
  if (is.null(.exercise)) {
    .exercise <- mock_exercise(
      user_code = expr,
      label = .label %||% "ex"
    )
  } else {
    .exercise[["code"]] <- expr
  }

  .envir <- .envir %||% new.env(parent = globalenv())

  res <- evaluate_exercise(.exercise, .envir, evaluate_global_setup = .eval_global_setup)
  lab <- if (!is.null(.label)) {
    sprintf("'%s'", .label)
  } else {
    paste0("`", expr, "`")
  }

  if (is.null(res$feedback)) {
    testthat::fail(sprintf("%s did not return feedback", lab))
  }

  if (!is.null(res$feedback$error)) {
    testthat::fail(sprintf(
      "%s returned an internal error: %s",
      lab,
      conditionMessage(res$feedback$error)
    ))
  }

  expect_valid_feedback(res$feedback, lab)

  if (!is.null(correct)) {
    checkmate::assert_logical(correct, min.len = 0, max.len = 1, any.missing = FALSE)
    act_correct <- res$feedback$correct

    correct_label <- function(x) {
      if (isTRUE(x)) return("correct")
      if (identical(x, FALSE)) return("incorrect")
      if (identical(x, logical(0))) return("neutral")
      "an invalid `correct` value"
    }

    testthat::expect(
      ok = identical(res$feedback$correct, correct),
      failure_message = sprintf(
        "%s is marked as %s, expected %s",
        lab,
        correct_label(res$feedback$correct),
        correct_label(correct)
      )
    )
  }

  if (!is.null(message)) {
    checkmate::assert_character(message, min.len = 1, max.len = 1, any.missing = FALSE)
    testthat::expect_match(
      res$feedback$message,
      regexp = message,
      perl = perl,
      fixed = fixed,
      ignore.case = ignore.case,
      label = sprintf("Feedback `message` from %s", lab)
    )
  }

  if (!is.null(type)) {
    checkmate::assert_character(type, min.len = 1, max.len = 1, any.missing = FALSE)
    testthat::expect(
      ok = identical(res$feedback$type, type),
      failure_message = sprintf(
        "%s returned feedback with type '%s', expected '%s'",
        lab,
        res$feedback$type,
        type
      )
    )
  }

  if (!is.null(location)) {
    checkmate::assert_character(location, min.len = 1, max.len = 1, any.missing = FALSE)
    testthat::expect(
      ok = identical(res$feedback$location, location),
      failure_message = sprintf(
        "%s returned feedback with location '%s', expected '%s'",
        lab,
        res$feedback$location,
        location
      )
    )
  }

  extras <- list(...)
  if (length(extras) == 0) {
    testthat::succeed()
    return(invisible(res))
  }

  if (is.null(names(extras)) || any(!nzchar(names(extras)))) {
    rlang::warn("Unammed arguments to `expect_feedback()` are ignored.")
  }

  if (is.null(names(extras))) {
    testthat::succeed()
    return(invisible(res))
  }

  extras <- extras[nzchar(names(extras))]

  for (prop in names(extras)) {
    testthat::expect_equal(
      res$feedback[[prop]],
      !!extras[[prop]],
      label = sprintf("Feedback property `%s` from %s", prop, lab)
    )
  }

  testthat::succeed()
  invisible(res)
}

expect_valid_feedback <- function(feedback, label = NULL) {
  if (!rlang::is_installed("testthat")) {
    feedback_validated(feedback)
    return(invisible(feedback))
  }

  act <- testthat::quasi_label(rlang::enquo(feedback), arg = "feedback")

  if (!is.null(label)) {
    act$lab <- paste(label, "did not produce")
  } else {
    act$lab <- paste(act$lab, "is not")
  }

  ok <- FALSE
  res <- tryCatch({
    feedback_validated(feedback)
    ok <- TRUE
    NULL
  }, error = identity)

  if (ok) {
    testthat::succeed()
  } else {
    msg <- sprintf("%s valid feedback: %s", act$lab, conditionMessage(res))
    testthat::fail(msg)
  }

  invisible(act$val)
}
