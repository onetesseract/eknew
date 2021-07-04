; ModuleID = 'ex.txt'
source_filename = "ex.txt"

declare void @printi(i64)

define void @neonmain() {
entry:
  tail call void @printi(i64 9)
  ret void
}
