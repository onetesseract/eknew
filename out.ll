; ModuleID = 'ex2.txt'
source_filename = "ex2.txt"

declare void @printi(i64)

define void @neonmain() {
entry:
  call void @printi(i64 9)
  ret void
}
