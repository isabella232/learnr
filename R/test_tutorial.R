.test_exercise <- new.env(parent = emptyenv())

is_testing_enabled <- function() {
  is_envvar_true("LEARNR_TEST") || is_envvar_true("LEARNR_TEST_SOLUTIONS")
}

find_test_exercise <- function() {
  x <- get0("exercise", envir = .test_exercise, ifnotfound = NULL)
  if (is.null(x)) {
    rlang::abort("Please provide an `.exercise`.")
  }
  x
}

clear_test_env <- function() {
  rm(list = ls(envir = .test_exercise), envir = .test_exercise)
}

set_test_env <- function(name, value) {
  assign(name, value, envir = .test_exercise)
  invisible(value)
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
  expr <- expr_as_text(expr, .exercise$engine %||% "r") %||% ""

  .exercise <- .exercise %||% find_test_exercise()
  .exercise[["code"]] <- expr

  .envir <- .envir %||% new.env(parent = knitr::knit_global())

  res <- evaluate_exercise(
    exercise = .exercise,
    envir = .envir,
    evaluate_global_setup = TRUE
  )

  .label <- .label %||% .exercise$label

  lab <- if (!is.null(.label)) {
    sprintf("'%s'", .label)
  } else {
    paste0("`", rlang::as_label(rlang::parse_expr(expr)), "`")
  }

  if (is.null(res$feedback)) {
    testthat::fail(sprintf("%s did not return feedback", lab))
  }

  if (!is.null(res$feedback$error)) {
    msg <- conditionMessage(res$feedback$error)
    testthat::fail(
      sprintf("%s returned an internal error: %s", lab, msg)
    )
  }

  feedback <- expect_valid_feedback(res$feedback, lab)

  if (!is.null(correct)) {
    checkmate::assert_logical(correct, min.len = 0, max.len = 1, any.missing = FALSE)

    correct_label <- function(x) {
      if (isTRUE(x)) return("correct")
      if (identical(x, FALSE)) return("incorrect")
      if (identical(x, logical(0))) return("neutral")
      "an invalid `correct` value"
    }

    testthat::expect(
      ok = identical(feedback$correct, correct),
      failure_message = sprintf(
        "%s is marked as %s, expected %s",
        lab,
        correct_label(feedback$correct),
        correct_label(correct)
      )
    )
  }

  if (!is.null(message)) {
    checkmate::assert_character(message, min.len = 1, max.len = 1, any.missing = FALSE)
    testthat::expect_match(
      feedback$message,
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
      ok = identical(feedback$type, type),
      failure_message = sprintf(
        "%s returned feedback with type '%s', expected '%s'",
        lab,
        feedback$type,
        type
      )
    )
  }

  if (!is.null(location)) {
    checkmate::assert_character(location, min.len = 1, max.len = 1, any.missing = FALSE)
    testthat::expect(
      ok = identical(feedback$location, location),
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
    rlang::warn("Unnamed arguments to `expect_feedback()` are ignored.")
  }

  if (is.null(names(extras))) {
    testthat::succeed()
    return(invisible(res))
  }

  extras <- extras[nzchar(names(extras))]

  for (prop in names(extras)) {
    testthat::expect_equal(
      feedback[[prop]],
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

  res <- tryCatch(feedback_validated(feedback), error = identity)

  if (!inherits(res, "error")) {
    testthat::succeed(sprintf("%s is valid feedback", act$lab))
  } else {
    msg <- sprintf("%s valid feedback: %s", act$lab, conditionMessage(res))
    testthat::fail(msg)
  }

  invisible(res)
}

test_that_exercise <- function(exercise, test_solutions = NULL) {
  if (!rlang::is_installed("testthat")) {
    learnr_render_message(
      "Cannot test exercises because {testthat} is not installed.",
      level = "warn"
    )
    return()
  }

  exercise <- standardize_exercise_code(exercise)
  exercise$label <- exercise$label %||% "exercise"
  exercise$version <- exercise$version %||% current_exercise_version

  has_checks <- any(
    vapply(exercise[c("check", "code_check")], nzchar, logical(1))
  )

  if (!has_checks) {
    return()
  }

  test_solutions <- test_solutions %||% is_envvar_true("LEARNR_TEST_SOLUTIONS", "true")

  tests <- exercise[["tests"]]
  if (!length(tests)) {
    if (!test_solutions) {
      # no tests and don't want to test solutions
      return()
    }
  } else if (is.null(names(tests))) {
    names(tests) <- paste("test", seq_along(tests))
  }

  if (test_solutions) {
    tests <- prepend_solution_tests(exercise, tests)
  }

  names(tests) <- sprintf("[%s] - %s", exercise$label, names(tests))

  set_test_env("exercise", exercise)
  testthat::context(set_test_env("label", exercise$label))
  withr::defer(clear_test_env())

  for (i in seq_along(tests)) {
    args <- list(
      desc = names(tests)[i],
      code = rlang::parse_expr(paste0("{", tests[[i]], "}"))
    )

    test_that <- rlang::call2("test_that", !!!args, .ns = "testthat")
    rlang::eval_bare(test_that)
  }

  try(testthat::get_reporter()$end_context(), silent = TRUE)
  invisible(TRUE)
}

prepend_solution_tests <- function(exercise, tests) {
  solution <- exercise[["solution"]]
  if (!nzchar(solution)) {
    return(tests)
  }

  solution_tests <- list()
  solution_passes <- rlang::call2(
    rlang::expr(expect_feedback),
    rlang::parse_expr(sprintf("{%s}", solution)),
    correct = TRUE
  )
  solution_desc <- sprintf("solution passes", exercise$label)
  solution_tests[[solution_desc]] <- rlang::expr_text(solution_passes)

  c(solution_tests, tests)
}

test_tutorial <- function(
  path,
  test = c("all", "solutions", "tests"),
  ...,
  quiet = TRUE,
  safely = TRUE,
  reporter = NULL
) {
  test <- match.arg(test)

  test_env_vars <- safe_env()
  if (identical(test, "all")) {
    test <- c("solutions", "tests")
  }
  if ("tests" %in% test) {
    test_env_vars <- c(test_env_vars, LEARNR_TEST = "true")
  }
  if ("solutions" %in% test) {
    test_env_vars <- c(test_env_vars, LEARNR_TEST_SOLUTIONS = "true")
  }

  render_call <- rlang::call2(
    .fn = "render",
    input = path,
    output_file = quote(tmpfile),
    quiet = quiet,
    ...,
    .ns = "rmarkdown"
  )

  reporter <- reporter %||%
    testthat::get_reporter() %||%
    if (interactive() && !safely) "progress" else "summary"

  res <-
    if (isTRUE(safely)) {
      reporter_safe <- safe({
        testthat::with_reporter(
          reporter = "silent",
          start_end_reporter = FALSE,
          {
            tmpfile <- withr::local_tempfile()
            !!render_call
            testthat::get_reporter()
          }
        )
      }, env = test_env_vars, spinner = FALSE)

      testthat::with_reporter(
        reporter = reporter,
        start_end_reporter = !testthat::is_testing(),
        {
          local_reporter <- testthat::get_reporter()
          local_reporter$start_file(path)
          for (result in reporter_safe$expectations()) {
            local_reporter$add_result(test = result$test, result = result)
          }
          local_reporter$end_file()
        }
      )
    } else {
      testthat::with_reporter(
        reporter = reporter,
        start_end_reporter = !testthat::is_testing(),
        withr::with_envvar(test_env_vars, {
          withr::with_tempfile("tmpfile", {
            local_reporter <- testthat::get_reporter()
            local_reporter$start_file(path)
            eval(render_call)
            local_reporter$end_file()
          })
        })
      )
    }

  invisible(res)
}
