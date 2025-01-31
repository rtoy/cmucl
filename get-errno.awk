BEGIN {
    count = 0
}

/^#define[ \t]+(E[A-Z0-9]+)[ \t]+([A-Z0-9]+).*$/ {
    errlist[count] = $2 " " $3;
    ++count;
}

END {
    for (k = 0; k < count; ++k) {
	printf "(def-unix-error %s)\n", errlist[k];
    }
}
    
    
