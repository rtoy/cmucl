BEGIN {
    count = 0
}

# Find anything that matches '#define Efoo value'
/^#define[ \t]+(E[A-Z0-9]+)[ \t]+([A-Z0-9]+).*$/ {
    # If we haven't found $2 in errno before, add it to the table.
    # This is to catch any duplicates.  We assume the latter value is
    # NOT what we want.  This happens on Darwin where EOPNOSUPP is
    # defined twice due to #if's.
    found = 0
    for (key in errno) {
	if (errno[key] == $2) {
	    printf ";; Ignoring dup of %s: old value %d, new value %\n", $2, errno[key], $3;
	    found = 1;
	    break;
       }
    }
    if (!found) {
	errno[count] = $2;
	value[count] = $3;
	++count;
    }
}

END {
    for (k = 0; k < count; ++k) {
	printf "(def-unix-error %s %s)\n", errno[k], value[k];
    }
}
