; ModuleID = 'ex2.txt'
source_filename = "ex2.txt"

declare void @printip(i64*)

define { i64 } @somefn() {
entry:
  ret { i64 } { i64 8 }
}

define void @neonmain() {
entry:
  %structsubacesstmp = alloca { i64 }, align 8
  %tmp = call { i64 } @somefn()
  %0 = extractvalue { i64 } %tmp, 0
  %1 = getelementptr inbounds { i64 }, { i64 }* %structsubacesstmp, i64 0, i32 0
  store i64 %0, i64* %1, align 8
  call void @printip(i64* nonnull %1)
  ret void
}
