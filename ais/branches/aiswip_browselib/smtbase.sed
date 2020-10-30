/^CFLAGS/ {h;G;s/CFLAGS/CFLAGS_NOOPT/;s/-O2//}

/fconio\.o/ {s/CFLAGS/CFLAGS_NOOPT/}
/fdatabas\.o/ {s/CFLAGS/CFLAGS_NOOPT/}
/fpred2\.o/ {s/CFLAGS/CFLAGS_NOOPT/}
/fsmtbase\.o/ {s/CFLAGS/CFLAGS_NOOPT/}
/futil1\.o/ {s/CFLAGS/CFLAGS_NOOPT/}
/futil2\.o/ {s/CFLAGS/CFLAGS_NOOPT/}
/futil3\.o/ {s/CFLAGS/CFLAGS_NOOPT/}
/fvmcode\.o/ {s/CFLAGS/CFLAGS_NOOPT/}
/fvmscpt\.o/ {s/CFLAGS/CFLAGS_NOOPT/}
/fvmscpt2\.o/ {s/CFLAGS/CFLAGS_NOOPT/}

/^all:/ {s/Makefile.Release//}
