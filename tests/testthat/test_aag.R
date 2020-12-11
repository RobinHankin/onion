test_that("Test suite aag.R, show methods",{
  
    options("show_onions_horizontally" = FALSE)
    expect_output(print(rquat()))
    expect_output(print(roct()))
    expect_output(print(romat()))

    options("show_onions_horizontally" = TRUE)
    expect_output(print(rquat()))
    expect_output(print(roct()))
    expect_output(print(romat()))

    options("show_onions_horizontally" = FALSE)

 
})
