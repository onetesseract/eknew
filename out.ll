; ModuleID = 'ex.txt'
source_filename = "ex.txt"

declare void @printd(double)

declare void @printi(i64)

declare void @printip(i64*)

define void @neonmain() {
entry:
  call void @printi(i64 6)
  ret void
}
