BEGIN {
    count = 0
}

/^#define[ \t]+(E[A-Z0-9]+)[ \t]+([A-Z0-9]+).*$/ {
    errlist[count] = $2 " " $3;
    ++count;
}

END {
    print "(defpackage \"UNIX-ERRNO\""
    print "  (:export"
    for (k = 0; k < count; ++k) {
	split(errlist[k], errno, " ")
	printf "   \"%s\"\n", errno[1];
    }
    print "))"
}
