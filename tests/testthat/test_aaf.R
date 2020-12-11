test_that("Test suite aaf.R, seq_onion()",{
  
  for(i in 1:2){
      for(slerp in c(TRUE,FALSE)){
          expect_silent(seq_onion(from=rquat(1),to=rquat(1),len=3,slerp=slerp))
          expect_silent(seq_onion(from=roct(1),to=roct(1),len=3,slerp=slerp))
          
          expect_silent(seq_onion(from=rquat(1),by=rquat(1),len=3,slerp=slerp))
          expect_silent(seq_onion(from=roct(1),by=roct(1),len=3,slerp=slerp))
          
          expect_silent(seq_onion(to=rquat(1),by=rquat(1),len=3,slerp=slerp))
          expect_silent(seq_onion(to=roct(1),by=roct(1),len=3,slerp=slerp))
      }
  }

  expect_error(seq_onion(length.out=0))
 
})
