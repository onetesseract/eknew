; ModuleID = 'ex.txt'
source_filename = "ex.txt"

declare void @printd(double)

declare void @printi(i64)

define void @pp(double %x, i64 %y) {
entry:
  call void @printd(double %x)
  call void @printi(i64 %y)
  ret void
}

define void @neonmain() {
entry:
  call void @pp(double 9.000000e+00, i64 8)
  ret void
}
