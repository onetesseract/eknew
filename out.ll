; ModuleID = 'ex.txt'
source_filename = "ex.txt"

declare double @printd(double)

define double @p(double %x) {
entry:
  %tmpadd = fadd double %x, 1.000000e+00
  ret double %tmpadd
}

define double @neonmain() {
entry:
  %tmp = call double @printd(double 2.000000e+00)
  %tmp1 = call double @p(double 2.000000e+00)
  %tmp2 = call double @printd(double %tmp1)
  ret double 1.000000e+00
}
