; ModuleID = 'ex.txt'
source_filename = "ex.txt"

declare void @printip(i64*)

declare void @printi(i64)

define i64 @x({ { i64 } } %self) {
entry:
  %0 = extractvalue { { i64 } } %self, 0
  %1 = extractvalue { i64 } %0, 0
  ret i64 %1
}

define void @neonmain() {
entry:
  %tmp = call i64 @x({ { i64 } } { { i64 } { i64 8 } })
  call void @printi(i64 %tmp)
  ret void
}
