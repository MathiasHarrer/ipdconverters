test_that("ids", {x <- 10; expect_equal(x, round(ids.rev(ids(x))))})
test_that("bdi2", {x <- 10; expect_equal(x, round(bdi2.rev(bdi2(x))))})
test_that("cesd", {x <- 10; expect_equal(x, round(cesd.rev(cesd(x))))})
test_that("gad7", {x <- 10; expect_equal(x, round(gad7.rev(gad7(x))))})
test_that("hadsa", {x <- 10; expect_equal(x, round(hadsa.rev(hadsa(x))))})
test_that("hadsd", {x <- 10; expect_equal(x, round(hadsd.rev(hadsd(x))))})
test_that("phq9", {x <- 10; expect_equal(x, round(phq9.rev(phq9(x))))})
test_that("who5", {x <- 10; expect_equal(x, round(who5.rev(who5(x))))})
test_that("madrs", {x <- 10; expect_equal(x, round(madrs.rev(madrs(x))))})
test_that("hamd", {x <- 10; expect_equal(x, round(hamd.rev(hamd(x))))})
test_that("qids", {x <- 10; expect_equal(x, round(qids.rev(qids(x))))})
test_that("ids", {x <- 10; expect_equal(x, round(ids.rev(ids(x))))})
test_that("epds", {x <- 10; expect_equal(x, round(epds.rev(epds(x))))})
test_that("promis8", {x <- 10; expect_equal(x, round(promis8.rev(promis8(x))))})
