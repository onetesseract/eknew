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

define i64 @v(double %x) {
entry:
  ret i64 1
}

define void @neonmain() {
entry:
  call void @printi(i64 9)
  call void @printd(double 9.800000e+00)
  ret void
}
