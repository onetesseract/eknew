; ModuleID = 'ex2.txt'
source_filename = "ex2.txt"

declare void @printip(i64*)

define i64 @loop() {
entry:
  %tmppointervalhelp = alloca i64, align 8
  store i64 8, i64* %tmppointervalhelp, align 8
  call void @printip(i64* nonnull %tmppointervalhelp)
  %tmp = call i64 @loop()
  ret i64 %tmp
}

define void @neonmain() {
entry:
  %tmp = tail call i64 @loop()
  ret void
}
