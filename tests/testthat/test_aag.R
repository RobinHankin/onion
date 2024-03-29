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

    expect_output(print(rsomat("quaternion")))
    expect_output(print(rsomat("octonion")))

    options("show_onionmats_in_place" = TRUE)
    expect_output(print(romat("quaternion")))
    expect_output(print(romat("octonion")))


    ## restore default:
    options("show_onions_horizontally" = FALSE) 
    options("show_onions_compactly" = FALSE)
    options("show_onionmats_in_place" = FALSE)

 
})
