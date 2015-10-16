testSwitch <- function(x, type) {
  switch(type,
         a = print("Passed A"),
         b = print("Passed B"),
         c = print("Passed C"),
         print("Default"))
}

testSwitch(type="a")
testSwitch(type="1")
testSwitch(type=b)