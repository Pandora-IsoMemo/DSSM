context("Conversion between lists and double matrix")

test_that("From list to matrix", {
  ll <- list(
    "a" = data.frame(
      mean = 1:3,
      sd = 4:6
    ),
    "b" = data.frame(
      mean = 10:30,
      sd = 40:60
    )
  )

  resFull <- listToDoubleMatrix(ll, c("a", "b"))
  resA <- listToDoubleMatrix(ll, c("a"))
  resVoid <- listToDoubleMatrix(ll, c())

  mFull <- cbind(
    ll$a,
    ll$b
  )
  names(mFull) <- c("a||mean", "a||sd", "b||mean", "b||sd")

  mA <- ll$a
  names(mA) <- c("a||mean", "a||sd")

  mVoid <- matrix(NA, 0, 0)

  expect_equal(resFull, as.matrix(mFull))
  expect_equal(resA, as.matrix(mA))
  expect_equal(resVoid, as.matrix(mVoid))
})

test_that("Missing name", {
  ll <- list()

  res <- matrix(NA, 0, 4, dimnames = list(NULL, c("a||mean", "a||sd", "b||mean", "b||sd")))

  expect_equal(listToDoubleMatrix(ll, c("a", "b")), res)
})

test_that("From matrix to list", {
  m <- matrix(1:12, 3, 4)
  colnames(m) <- c("a||mean", "a||sd", "b||mean", "b||sd")

  mMissing <- rbind(m, NA)

  mPartial <- m
  mPartial[3, 1:2] <- NA

  mEmpty <- matrix(1, 0, 4)
  colnames(mEmpty) <- colnames(m)


  ll <- list(
    a = data.frame(
      mean = 1:3,
      sd = 4:6
    ),
    b = data.frame(
      mean = 7:9,
      sd = 10:12
    )
  )

  llMissing <- ll

  llPartial <- list(
    a = data.frame(
      mean = c(1:2, NA),
      sd = c(4:5, NA)
    ),
    b = data.frame(
      mean = 7:9,
      sd = 10:12
    )
  )

  llEmpty <- list(
    a = data.frame(
      mean = numeric(0),
      sd = numeric(0)
    ),
    b = data.frame(
      mean = numeric(0),
      sd = numeric(0)
    )
  )

  expect_equal(doubleMatrixToList(m), ll)
  expect_equal(doubleMatrixToList(mMissing), llMissing)
  expect_equal(doubleMatrixToList(mPartial), llPartial)
  expect_equal(doubleMatrixToList(mEmpty), llEmpty)
})
