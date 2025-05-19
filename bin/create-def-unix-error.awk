BEGIN {
    count = 0
}

# Find anything that matches '#define Efoo value'
/^#define[ \t]+(E[A-Z0-9]+)[ \t]+([A-Z0-9]+).*$/ {
    found = 0
    # Search to see if the current key, $2, is already in the table.
    # If so, we mark it as something to skip.  This happens on Darwin
    # where EOPNTOSUPP is seen twice.  We want the second entry.
    for (key in errno) {
	if (errno[key] == $2) {
	    printf ";; Ignoring dup of %s: old value %s, new value %s\n", $2, value[key], $3;
	    found = 1;
	    skip[key] = 1;
	    break;
       }
    }
    errno[count] = $2;
    value[count] = $3;
    ++count;
}

END {
    for (k = 0; k < count; ++k) {
	printf "%s(def-unix-error %s %s)\n",
	    (skip[k] != 1) ? "": ";; ", errno[k], value[k];
    }
}
