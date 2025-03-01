BEGIN {
    count = 0
}

# Find anything that matches '#define Efoo value'
/^#define[ \t]+(E[A-Z0-9]+)[ \t]+([A-Z0-9]+).*$/ {
    # If we haven't found $2 in errno before, add it to the table.
    # This is to catch any duplicates.  We assume the latter value is
    # NOT what we want.  This happens on Darwin where EOPNOSUPP is
    # defined twice due to #if's.
    if (!($2 in errno)) {
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
