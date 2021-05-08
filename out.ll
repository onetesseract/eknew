; ModuleID = 'ex2.txt'
source_filename = "ex2.txt"

declare void @printi(i64)

define i64 @x({ i64 } %self) {
entry:
  %0 = extractvalue { i64 } %self, 0
  call void @printi(i64 %0)
  ret i64 8
}

define void @neonmain() {
entry:
  call void @printi(i64 undef)
  ret void
}
