program test
! TODO: not really a test. just to make sure the path doesn't break!
  character(len=80) ::text
  text = "test1"
  text = trim(text)
  print *,text, "test2"
end

