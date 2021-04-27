; ModuleID = 'ex.txt'
source_filename = "ex.txt"

declare void @printd(double)

declare void @printi(i64)

declare void @printip(i64*)

define void @neonmain() {
entry:
  %f = alloca { i64, { i64, double } }, align 8
  %"help im trapped in a universe factory3.repack" = getelementptr inbounds { i64, { i64, double } }, { i64, { i64, double } }* %f, i64 0, i32 1, i32 0
  store i64 8, i64* %"help im trapped in a universe factory3.repack", align 8
  call void @printip(i64* nonnull %"help im trapped in a universe factory3.repack")
  ret void
}
