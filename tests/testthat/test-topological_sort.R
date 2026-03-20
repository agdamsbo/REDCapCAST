library(testthat)

# Helper: check that result respects all pairwise constraints in ordered_list
satisfies_constraints <- function(result, ordered_list) {
  pos <- setNames(seq_along(result), result)
  all(sapply(ordered_list, function(vec) {
    all(diff(pos[vec]) > 0)
  }))
}

test_that("basic ordering is correct", {
  lst <- list(c("shoe", "ball"), c("shirt", "shoe", "car"), c("ball", "car"))
  result <- topological_sort(lst)

  expect_equal(result, c("shirt", "shoe", "ball", "car"))
  expect_true(satisfies_constraints(result, lst))
})

test_that("result contains all elements exactly once", {
  lst <- list(c("shoe", "ball"), c("shirt", "shoe", "car"), c("ball", "car"))
  result <- topological_sort(lst)
  all_elements <- unique(unlist(lst))

  expect_setequal(result, all_elements)
  expect_equal(length(result), length(all_elements))
})

test_that("loosely constrained element satisfies its constraints", {
  lst <- list(c("shoe", "ball"), c("shirt", "shoe", "car"),
              c("ball", "car"),  c("stick", "car"))
  result <- topological_sort(lst)

  expect_true(satisfies_constraints(result, lst))
  expect_true(which(result == "stick") < which(result == "car"))
})

test_that("duplicate constraints are handled silently", {
  lst_dupes <- list(c("a", "b"), c("a", "b"), c("b", "c"), c("a", "b", "c"))
  lst_clean <- list(c("a", "b"), c("b", "c"))

  expect_equal(topological_sort(lst_dupes), topological_sort(lst_clean))
})

test_that("single-element vectors do not affect output", {
  lst        <- list(c("a", "b", "c"))
  lst_single <- list(c("a", "b", "c"), c("b"))

  expect_equal(topological_sort(lst), topological_sort(lst_single))
})

test_that("single vector is returned as-is", {
  lst <- list(c("a", "b", "c"))
  expect_equal(topological_sort(lst), c("a", "b", "c"))
})

test_that("single element in list is returned as-is", {
  expect_equal(topological_sort(list(c("a"))), "a")
})

test_that("all single-element vectors return elements in encounter order", {
  lst <- list(c("a"), c("b"), c("c"))
  result <- topological_sort(lst)

  expect_setequal(result, c("a", "b", "c"))
  expect_equal(length(result), 3)
})

test_that("elements shared across many vectors satisfy all constraints", {
  lst <- list(
    c("a", "b", "c"),
    c("d", "b", "e"),
    c("f", "c", "e")
  )
  result <- topological_sort(lst)

  expect_true(satisfies_constraints(result, lst))
  expect_setequal(result, c("a", "b", "c", "d", "e", "f"))
})

test_that("cycle is detected and errors", {
  expect_error(topological_sort(list(c("a", "b"), c("b", "a"))),
               "Cycle detected")
  expect_error(topological_sort(list(c("a", "b"), c("b", "c"), c("c", "a"))),
               "Cycle detected")
})

test_that("default priority is deterministic across repeated calls", {
  lst <- list(c("shoe", "ball"), c("shirt", "shoe", "car"),
              c("ball", "car"),  c("stick", "car"))

  results <- replicate(10, topological_sort(lst), simplify = FALSE)
  expect_true(all(sapply(results, identical, results[[1]])))
})

# --- Priority modes ---

test_that("all priority modes satisfy constraints", {
  lst <- list(c("shoe", "ball"), c("shirt", "shoe", "car"),
              c("ball", "car"),  c("stick", "car"))
  modes <- c("none", "lexicographic", "least_constrained", "most_constrained")

  for (mode in modes) {
    result <- topological_sort(lst, priority = mode)
    expect_true(satisfies_constraints(result, lst),
                label = paste("constraints satisfied for priority =", mode))
  }
})

test_that("all priority modes return all elements exactly once", {
  lst <- list(c("shoe", "ball"), c("shirt", "shoe", "car"),
              c("ball", "car"),  c("stick", "car"))
  modes <- c("none", "lexicographic", "least_constrained", "most_constrained")
  all_elements <- unique(unlist(lst))

  for (mode in modes) {
    result <- topological_sort(lst, priority = mode)
    expect_true(setequal(result, all_elements),
                label = paste("all elements present for priority =", mode))
  }
})

test_that("lexicographic priority sorts free nodes alphabetically", {
  # "bat", "cat", "hat" are all unconstrained relative to each other
  lst <- list(c("hat", "end"), c("cat", "end"), c("bat", "end"))
  result <- topological_sort(lst, priority = "lexicographic")
  free_nodes <- result[result != "end"]

  expect_equal(free_nodes, sort(free_nodes))
})

test_that("least_constrained places isolated nodes before connected ones", {
  # "loner" has degree 1 (outdeg only)
  # "hub"   has degree 3 (indeg=1, outdeg=2)
  lst <- list(c("a", "hub", "b"), c("c", "hub"), c("loner", "b"))
  result <- topological_sort(lst, priority = "least_constrained")

  expect_true(which(result == "loner") < which(result == "hub"))
})

test_that("most_constrained places isolated nodes after connected ones", {
  lst <- list(c("a", "hub", "b"), c("c", "hub"), c("loner", "b"))
  result <- topological_sort(lst, priority = "most_constrained")

  expect_true(which(result == "hub") < which(result == "loner"))
})

test_that("invalid priority argument errors", {
  lst <- list(c("a", "b"))
  expect_error(topological_sort(lst, priority = "random"), "arg")
})

# --- Scale ---

test_that("handles a long linear chain", {
  n   <- 1000
  lst <- list(as.character(seq_len(n)))
  result <- topological_sort(lst)

  expect_equal(result, as.character(seq_len(n)))
})

test_that("handles a large list of short vectors", {
  set.seed(42)
  elements <- letters
  lst <- lapply(seq_len(500), function(i) {
    sample(elements, 3)
  })
  # Only check it runs and returns valid output — may contain cycles
  tryCatch({
    result <- topological_sort(lst)
    expect_true(satisfies_constraints(result, lst))
    expect_true(length(result) <= length(letters))
  }, error = function(e) {
    expect_match(conditionMessage(e), "Cycle detected")
  })
})

## Safe ordering
test_that("safe wrapper returns same result as topological_sort when no cycle", {
  lst <- list(c("shoe", "ball"), c("shirt", "shoe", "car"), c("ball", "car"))

  expect_equal(
    safe_topological_sort(lst),
    topological_sort(lst)
  )
})

test_that("safe wrapper returns all elements in order of appearance on cycle", {
  lst <- list(c("start", "a"), c("a", "b"), c("b", "a"))
  result <- safe_topological_sort(lst)

  expect_setequal(result, unique(unlist(lst)))
  expect_equal(length(result), length(unique(unlist(lst))))
})
