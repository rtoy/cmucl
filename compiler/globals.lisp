
(in-package "C")
(proclaim '(special
	    *defprint-pretty* *event-info* *event-note-threshold*
	    *instruction-formats* *instructions* *current-fixup* *first-fixup*
	    *next-location* *code-vector* *labels* *current-label*
	    *last-label-created* *assembler-nodes* *current-assembler-node*
	    *last-assembler-node-created* *other-code-vector*
	    *other-next-location* *fixup-offset* *fixup-offset-map*
	    *fixup-last-shortening* *result-fixups* *source-path-tree*
	    *compiler-error-context* *word-length* 
	    *undefined-warnings*
	    *code-segment* *elsewhere*))
