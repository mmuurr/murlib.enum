test_that("prepend_class", {
    x <- mtcars
    x <- prepend_class("baz")
    x <- prepend_class(c("foo", "bar", "foo"))
    expect_identical(class(x), c("foo", "bar", "baz", "data.frame"))
})


test_that("empty enum", {
    EmptyEnum <- enum()
    expect_identical(length(EmptyEnum), 0)
    expect_identical(names(EmptyEnum), character(0))
    expect_true(rlang::is_named(EmptyEnum))
})

test_that("bad construction", {
    enum("FOO", "FOO")  ## duplicates
    enum("Foo")  ## fails regex
})    
    
