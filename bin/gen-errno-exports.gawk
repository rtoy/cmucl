BEGIN {
    count = 0
}

/^#define[ \t]+(E[A-Z0-9]+)[ \t]+([A-Z0-9]+).*$/ {
    errlist[count] = $2;
    ++count;
}

END {
    asort(errlist)
    print "(defpackage \"UNIX-ERRNO\""
    print "  (:export"
    for (k = 1; k < count; ++k) {
	printf "   \"%s\"\n", errlist[k];
    }
    print "   ))\n"
}
