
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>
#include <caml/signals.h>
#include <assert.h>

#if defined(_MSC_VER)

CAMLprim value caml_devkit_fsync(value v)
{
   CAMLparam1(v);
   caml_failwith("fsync not implemented");
   CAMLnoreturn; 
}

#else

#include <unistd.h>

CAMLprim value caml_devkit_fsync(value v_fd)
{
    CAMLparam1(v_fd);
    int r = 0;
    assert(Is_long(v_fd));
    caml_enter_blocking_section();
    r = fsync(Int_val(v_fd));
    caml_leave_blocking_section();
    if (0 != r)
      uerror("fsync",Nothing);
    CAMLreturn(Val_unit);
}

#endif

