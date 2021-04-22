; ModuleID = 'tmp'
source_filename = "tmp"

define double @p(double %x) {
entry:
  %tmpadd = fadd double %x, 1.000000e+00
  %tmpadd3 = fadd double %tmpadd, 1.000000e+00
  ret double %tmpadd3
}

define double @anonymous() {
entry:
  %tmp = call double @p(double 2.000000e+00)
  ret double %tmp
}
