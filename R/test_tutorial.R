.test_exercise <- new.env(parent = emptyenv())

is_testing_enabled <- function() {
  is_envvar_true("LEARNR_TEST") || is_envvar_true("LEARNR_TEST_SOLUTIONS")
}

require_testthat <- function() {
  if (!is_testing_enabled()) return()

  learnr_render_catch(
    rlang::check_installed("testthat", "for testing learnr exercises")
  )

  if (is_testing_enabled() && rlang::is_installed("testthat")) {
    # FIXME: ensure testthat::fail() doesn't clobber gradethis::fail()
    require("testthat", character.only = TRUE, quietly = testthat::is_testing())
  }
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
  srcref <- if (!is.null(.exercise[[".srcref"]])) {
    srcref <- .exercise[[".srcref"]]
    attr(srcref, "srcfile")$filename <- basename(attr(srcref, "srcfile")$filename)
    srcref
  }

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
    testthat::expect(
      ok = FALSE,
      failure_message = sprintf("%s did not return feedback", lab),
      srcref = srcref
    )
  }

  if (!is.null(res$feedback$error)) {
    msg <- if (rlang::is_condition(res$feedback$error)) {
      paste0(": ", conditionMessage(res$feedback$error))
    } else if (rlang::has_name(res$feedback$error, "message")) {
      paste0(": ", res$feedback$error$message)
    } else {
      ""
    }

    if (rlang::is_condition(res$feedback$error)) {
      msg <- conditionMessage(res$feedback$error)
      testthat::expect(
        ok = FALSE,
        failure_message = sprintf("%s returned an internal error%s", lab, msg),
        srcref = srcref
      )
    }
  }

  feedback_ok <- check_valid_feedback(res$feedback, lab)
  testthat::expect(feedback_ok$ok, feedback_ok$message, srcref = srcref)
  # use validated feedback or the one that came from the exercise result
  feedback <- feedback_ok$result %||% res$feedback

  compare <- tryCatch(local({
    waldo_compare <- utils::getFromNamespace("waldo_compare", "testthat")
    function(x, y, ...) {
      waldo_compare(x, y, ..., x_arg = "actual", y_arg = "expected")
    }
  }), error = function(...) {
    testthat::compare
  })

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
        "%s was not marked as %s\n%s",
        lab,
        correct_label(correct),
        compare(feedback[["correct"]], correct)
      ),
      srcref = srcref
    )
  }

  if (!is.null(message)) {
    checkmate::assert_character(message, min.len = 1, max.len = 1, any.missing = FALSE)

    cnd <- rlang::catch_cnd(
      testthat::expect_match(
        feedback$message,
        regexp = message,
        perl = perl,
        fixed = fixed,
        ignore.case = ignore.case,
        label = sprintf("Feedback `message` from %s", lab)
      )
    )
    if (rlang::is_condition(cnd) && !is.null(srcref)) {
      cnd$srcref <- srcref
    }
    testthat::exp_signal(cnd)
  }

  if (!is.null(type)) {
    checkmate::assert_character(type, min.len = 1, max.len = 1, any.missing = FALSE)
    testthat::expect(
      ok = identical(feedback$type, type),
      failure_message = sprintf(
        "%s did not return feedback with type '%s'\n%s",
        lab,
        type,
        compare(feedback$type, type)
      ),
      srcref = srcref
    )
  }

  if (!is.null(location)) {
    checkmate::assert_character(location, min.len = 1, max.len = 1, any.missing = FALSE)
    testthat::expect(
      ok = identical(feedback$location, location),
      failure_message = sprintf(
        "%s did not return feedback with location '%s'\n%s",
        lab,
        location,
        compare(res$feedback$location, location)
      ),
      srcref = srcref
    )
  }

  extras <- list(...)
  if (length(extras) == 0) {
    return(invisible(res))
  }

  if (is.null(names(extras)) || any(!nzchar(names(extras)))) {
    rlang::warn("Unnamed arguments to `expect_feedback()` are ignored.")
  }

  if (is.null(names(extras))) {
    return(invisible(res))
  }

  extras <- extras[nzchar(names(extras))]

  for (prop in names(extras)) {
    cnd <- rlang::catch_cnd(
      testthat::expect_equal(
        feedback[[prop]],
        !!extras[[prop]],
        label = sprintf("Feedback property `%s` from %s", prop, lab)
      )
    )
    if (rlang::is_condition(cnd) && !is.null(srcref)) {
      cnd$srcref <- srcref
    }
    testthat::exp_signal(cnd)
  }

  invisible(res)
}

check_valid_feedback <- function(feedback, label = NULL) {
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

  tryCatch(
    list(
      result = feedback_validated(feedback),
      ok = TRUE,
      message = sprintf("%s is valid feedback", act$lab)
    ),
    error = function(err) {
      list(
        ok = FALSE,
        message = sprintf("%s valid feedback: %s", act$lab, conditionMessage(err))
      )
    }
  )
}

# expect_with_exercise <- function(expr, exercise = NULL) {
#   if (is.null(exercise)) {
#     return(invisble(rlang::eval_tidy(expr)))
#   }
#
#   exp <- list()
#   withCallingHandlers(
#     error = function(cnd) {
#       rlang::cnd_muffle(cnd)
#       exp <<- c(exp, list(cnd))
#     }
#   )
#
#   for (exp_this in exp) {
#     if (!inherits(exp_this, "expectation")) {
#       next
#     }
#
#     if (!is.null(exercise[[".srcref"]])) {
#       srcref <- exercise[[".srcref"]]
#       attr(srcref, "srcfile")$filename <- basename(attr(srcref, "srcfile")$filename)
#       exp_this$srcref <- srcref
#     }
#
#     testthat::exp_signal(exp_this)
#   }
#
#   invisible(exp)
# }

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
  if (is.null(tests) && !test_solutions) {
    # no tests and don't want to test solutions
    return()
  }

  if (test_solutions) {
    tests <- prepend_solution_tests(exercise, tests)
  }

  set_test_env("exercise", exercise)
  withr::defer(clear_test_env())

  test_exprs <- lapply(
    rlang::parse_exprs(tests),
    add_label_to_description,
    label = exercise$label
  )

  rlang::eval_bare(rlang::call2(`{`, !!!test_exprs))

  invisible(TRUE)
}

add_label_to_description <- function(expr, label) {
  if (!rlang::is_call(expr)) {
    return(expr)
  }

  fn <- rlang::call_fn(expr)
  if (!identical(fn, testthat::test_that)) {
    return(expr)
  }

  args <- rlang::call_args(rlang::call_match(expr, fn))
  args[["desc"]] <- sprintf("[%s] - %s", label, args[["desc"]])
  rlang::call2("test_that", !!!args, .ns = "testthat")
}

prepend_solution_tests <- function(exercise, tests) {
  solution <- exercise[["solution"]]
  if (!nzchar(solution)) {
    return(tests)
  }

  solution_passes <- rlang::call2(
    rlang::expr(test_that),
    .ns = "testthat",
    desc = sprintf("solution passes", exercise$label),
    rlang::call2(
      rlang::expr(expect_feedback),
      rlang::parse_expr(sprintf("{%s}", solution)),
      correct = TRUE
    )
  )
  solution_passes <- rlang::expr_text(solution_passes)

  paste(solution_passes, tests, sep = "\n\n")
}

test_tutorial <- function(
  path,
  test = c("all", "solutions", "tests"),
  ...,
  quiet = TRUE,
  safely = TRUE,
  reporter = NULL
) {
  test <- rlang::arg_match(test)

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

  is_testing <- testthat::is_testing()
  with_report_file <- function(reporter, path) {
    if (testthat::is_testing()) return()
    reporter$start_file(path)
    withr::defer_parent(reporter$end_file())
  }

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
        start_end_reporter = !is_testing,
        {
          local_reporter <- testthat::get_reporter()
          with_report_file(local_reporter, path)
          for (result in reporter_safe$expectations()) {
            local_reporter$add_result(test = result$test, result = result)
          }
        }
      )
    } else {
      require_testthat()
      testthat::with_reporter(
        reporter = reporter,
        start_end_reporter = !is_testing,
        withr::with_envvar(test_env_vars, {
          withr::with_tempfile("tmpfile", {
            local_reporter <- testthat::get_reporter()
            with_report_file(local_reporter, path)
            eval(render_call)
          })
        })
      )
    }

  invisible(res)
}
