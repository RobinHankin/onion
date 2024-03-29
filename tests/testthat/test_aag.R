test_that("Test suite aag.R, show methods",{
  
    options("show_onions_horizontally" = FALSE)
    expect_output(print(rquat()))
    expect_output(print(roct()))
    expect_output(print(romat()))

    options("show_onions_horizontally" = TRUE)
    expect_output(print(rquat()))
    expect_output(print(roct()))
    expect_output(print(romat()))

    options("show_onions_compactly" = TRUE)
    expect_output(print(rsquat()))
    expect_output(print(rsoct()))

    ## restore default:
    options("show_onions_horizontally" = FALSE) 
    options("show_onions_compactly" = TRUE)

 
})
